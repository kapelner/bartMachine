package bartMachine;

import java.util.ArrayDeque;

import uk.ac.manchester.tornado.api.GridScheduler;
import uk.ac.manchester.tornado.api.ImmutableTaskGraph;
import uk.ac.manchester.tornado.api.KernelContext;
import uk.ac.manchester.tornado.api.TaskGraph;
import uk.ac.manchester.tornado.api.TornadoExecutionPlan;
import uk.ac.manchester.tornado.api.WorkerGrid1D;
import uk.ac.manchester.tornado.api.enums.DataTransferMode;

/**
 * GPU-accelerated batch prediction via TornadoVM.
 *
 * Uses the KernelContext API (CUDA-style explicit thread IDs) rather than
 * the @Parallel annotation API.  The KernelContext path does not trigger
 * TornadoVM's ASMClassVisitor.getParallelAnnotations(), which would otherwise
 * call ClassLoader.getSystemClassLoader().getResourceAsStream(...) — a lookup
 * that fails when bartMachine is loaded via rJava's child URLClassLoader.
 *
 * TornadoVM must be on the compile classpath; it is optional at runtime.
 * If no GPU device is found at startup, GPU_AVAILABLE is false and every
 * call falls back to the caller's CPU path transparently.
 *
 * Design:
 *   1. flatten()           - convert pointer-linked tree forest to flat arrays
 *   2. gpuPredictMeans()   - GPU kernel, one thread per test record
 *   3. predictWithGpu()    - orchestrates flatten + TaskGraph + un-transform
 */
public final class GpuForestPredictor {

	// -------------------------------------------------------------------------
	// GPU detection (once, at class-load time)
	// -------------------------------------------------------------------------

	public static final boolean GPU_AVAILABLE;

	static {
		boolean found = false;
		try {
			// Reflect into TornadoRuntimeProvider so that a missing TornadoVM
			// JAR at runtime causes a caught exception rather than a hard crash.
			// TornadoVM 4.x API: getNumBackends / getBackend / getNumDevices / getTypeDefaultDevice
			Class<?> provCls = Class.forName(
					"uk.ac.manchester.tornado.api.runtime.TornadoRuntimeProvider");
			Object rt = provCls.getMethod("getTornadoRuntime").invoke(null);
			int numBackends = (int) rt.getClass().getMethod("getNumBackends").invoke(rt);
			outer:
			for (int d = 0; d < numBackends; d++) {
				Object backend = rt.getClass()
						.getMethod("getBackend", int.class).invoke(rt, d);
				Object devType = backend.getClass()
						.getMethod("getTypeDefaultDevice").invoke(backend);
				if (devType != null && devType.toString().contains("GPU")) {
					int devCount = (int) backend.getClass()
							.getMethod("getNumDevices").invoke(backend);
					if (devCount > 0) { found = true; break outer; }
				}
			}
		} catch (Throwable t) { /* TornadoVM absent or no GPU driver */ }
		GPU_AVAILABLE = found;
	}

	// -------------------------------------------------------------------------
	// Tree flattening — BFS ordering within each tree
	// -------------------------------------------------------------------------

	static int flattenTree(
			bartMachineTreeNode root, int baseOffset,
			int[] splitAttr, double[] splitVal,
			int[] leftChild, int[] rightChild,
			int[] sendMissRight, double[] yPred) {

		ArrayDeque<bartMachineTreeNode> nodeQ = new ArrayDeque<>();
		ArrayDeque<Integer> idxQ = new ArrayDeque<>();
		nodeQ.add(root);
		idxQ.add(baseOffset);
		int nextFree = baseOffset + 1;

		while (!nodeQ.isEmpty()) {
			bartMachineTreeNode node = nodeQ.poll();
			int k = idxQ.poll();
			yPred[k]         = node.y_pred;
			sendMissRight[k] = node.sendMissingDataRight ? 1 : 0;

			if (node.isLeaf) {
				splitAttr[k] = -1;
				splitVal[k]  = 0.0;
				leftChild[k] = -1;
				rightChild[k] = -1;
			} else {
				splitAttr[k] = node.splitAttributeM;
				splitVal[k]  = node.splitValue;
				int lk = nextFree++;
				int rk = nextFree++;
				leftChild[k]  = lk;
				rightChild[k] = rk;
				nodeQ.add(node.left);  idxQ.add(lk);
				nodeQ.add(node.right); idxQ.add(rk);
			}
		}
		return nextFree;
	}

	public static FlatForest flatten(
			bartMachineTreeNode[][] samplesAfterBurnIn, int G, int T) {

		int[] treeOffsets = new int[G * T + 1];
		int total = 0;
		for (int g = 0; g < G; g++) {
			for (int t = 0; t < T; t++) {
				treeOffsets[g * T + t] = total;
				total += samplesAfterBurnIn[g][t].numNodesAndLeaves();
			}
		}
		treeOffsets[G * T] = total;

		int[]    splitAttr    = new int[total];
		double[] splitVal     = new double[total];
		int[]    leftChild    = new int[total];
		int[]    rightChild   = new int[total];
		int[]    sendMissRight = new int[total];
		double[] yPred        = new double[total];

		for (int g = 0; g < G; g++) {
			for (int t = 0; t < T; t++) {
				flattenTree(samplesAfterBurnIn[g][t], treeOffsets[g * T + t],
						splitAttr, splitVal, leftChild, rightChild, sendMissRight, yPred);
			}
		}
		return new FlatForest(treeOffsets, splitAttr, splitVal,
				leftChild, rightChild, sendMissRight, yPred, G, T);
	}

	// -------------------------------------------------------------------------
	// GPU kernels — KernelContext API (one thread per outer-loop iteration)
	// -------------------------------------------------------------------------

	/**
	 * Compute the posterior mean prediction for every record in parallel.
	 * Each GPU thread handles one test record (i = context.globalIdx).
	 * dims = [n_star, G, T, p] packed to stay within TornadoVM's Task14 arity limit.
	 */
	public static void gpuPredictMeans(
			KernelContext context,
			int[]    dims,
			double[] flatRecords,
			int[]    treeOffsets,
			int[]    splitAttr,
			double[] splitVal,
			int[]    leftChild,
			int[]    rightChild,
			int[]    sendMissRight,
			double[] yPred,
			double   halfDiff,
			double   yRange,
			double   yMin,
			double[] yHats
	) {
		int n_star = dims[0]; int G = dims[1]; int T = dims[2]; int p = dims[3];
		int i = context.globalIdx;
		if (i >= n_star) return;

		double sum = 0.0;
		int base = i * p;
		for (int g = 0; g < G; g++) {
			int gtBase = g * T;
			for (int t = 0; t < T; t++) {
				int node = treeOffsets[gtBase + t];
				for (int depth = 0; depth < 64; depth++) {
					int attr = splitAttr[node];
					if (attr == -1) break;
					double val = flatRecords[base + attr];
					if (Double.isNaN(val)) {
						node = sendMissRight[node] == 1 ? rightChild[node] : leftChild[node];
					} else if (val <= splitVal[node]) {
						node = leftChild[node];
					} else {
						node = rightChild[node];
					}
				}
				sum += yPred[node];
			}
		}
		yHats[i] = (sum / G + halfDiff) * yRange + yMin;
	}

	/**
	 * GPU kernel to compute ALL posterior samples for every record.
	 * Output posteriorSamples is flat: [i*G + g] for record i, sample g.
	 */
	public static void gpuPredictPosteriorSamples(
			KernelContext context,
			int[]    dims,
			double[] flatRecords,
			int[]    treeOffsets,
			int[]    splitAttr,
			double[] splitVal,
			int[]    leftChild,
			int[]    rightChild,
			int[]    sendMissRight,
			double[] yPred,
			double   halfDiff,
			double   yRange,
			double   yMin,
			double[] posteriorSamples
	) {
		int n_star = dims[0]; int G = dims[1]; int T = dims[2]; int p = dims[3];
		int i = context.globalIdx;
		if (i >= n_star) return;

		int base    = i * p;
		int outBase = i * G;
		for (int g = 0; g < G; g++) {
			double sampleSum = 0.0;
			int gtBase = g * T;
			for (int t = 0; t < T; t++) {
				int node = treeOffsets[gtBase + t];
				for (int depth = 0; depth < 64; depth++) {
					int attr = splitAttr[node];
					if (attr == -1) break;
					double val = flatRecords[base + attr];
					if (Double.isNaN(val)) {
						node = sendMissRight[node] == 1 ? rightChild[node] : leftChild[node];
					} else if (val <= splitVal[node]) {
						node = leftChild[node];
					} else {
						node = rightChild[node];
					}
				}
				sampleSum += yPred[node];
			}
			posteriorSamples[outBase + g] = (sampleSum + halfDiff) * yRange + yMin;
		}
	}

	/** Standard normal CDF approximation (absolute error < 7.5e-8). */
	public static double phi(double x) {
		double t = 1.0 / (1.0 + 0.2316419 * Math.abs(x));
		double d = 0.3989422804014327 * Math.exp(-x * x / 2.0);
		double p = d * t * (0.319381530 + t * (-0.356563782 + t * (1.781477937 + t * (-1.821255978 + t * 1.330274429))));
		double result = x > 0 ? 1.0 - p : p;
		if (result <= 1e-14) return 1e-14;
		if (result >= 1.0 - 1e-14) return 1.0 - 1e-14;
		return result;
	}

	/** GPU kernel: posterior mean probabilities for classification. */
	public static void gpuPredictClassificationMeans(
			KernelContext context,
			int n_star, int G, int T, int p,
			double[] flatRecords,
			int[]    treeOffsets,
			int[]    splitAttr,
			double[] splitVal,
			int[]    leftChild,
			int[]    rightChild,
			int[]    sendMissRight,
			double[] yPred,
			double[] pHats
	) {
		int i = context.globalIdx;
		if (i >= n_star) return;

		double probSum = 0.0;
		int base = i * p;
		for (int g = 0; g < G; g++) {
			double sampleSum = 0.0;
			int gtBase = g * T;
			for (int t = 0; t < T; t++) {
				int node = treeOffsets[gtBase + t];
				for (int depth = 0; depth < 64; depth++) {
					int attr = splitAttr[node];
					if (attr == -1) break;
					double val = flatRecords[base + attr];
					if (Double.isNaN(val)) {
						node = sendMissRight[node] == 1 ? rightChild[node] : leftChild[node];
					} else if (val <= splitVal[node]) {
						node = leftChild[node];
					} else {
						node = rightChild[node];
					}
				}
				sampleSum += yPred[node];
			}
			probSum += phi(sampleSum);
		}
		pHats[i] = probSum / G;
	}

	/** GPU kernel: ALL posterior probabilities for classification. */
	public static void gpuPredictClassificationPosteriorSamples(
			KernelContext context,
			int n_star, int G, int T, int p,
			double[] flatRecords,
			int[]    treeOffsets,
			int[]    splitAttr,
			double[] splitVal,
			int[]    leftChild,
			int[]    rightChild,
			int[]    sendMissRight,
			double[] yPred,
			double[] posteriorProbs
	) {
		int i = context.globalIdx;
		if (i >= n_star) return;

		int base    = i * p;
		int outBase = i * G;
		for (int g = 0; g < G; g++) {
			double sampleSum = 0.0;
			int gtBase = g * T;
			for (int t = 0; t < T; t++) {
				int node = treeOffsets[gtBase + t];
				for (int depth = 0; depth < 64; depth++) {
					int attr = splitAttr[node];
					if (attr == -1) break;
					double val = flatRecords[base + attr];
					if (Double.isNaN(val)) {
						node = sendMissRight[node] == 1 ? rightChild[node] : leftChild[node];
					} else if (val <= splitVal[node]) {
						node = leftChild[node];
					} else {
						node = rightChild[node];
					}
				}
				sampleSum += yPred[node];
			}
			posteriorProbs[outBase + g] = phi(sampleSum);
		}
	}

	/**
	 * GPU kernel: find two quantiles (Type 7) for each record via partial sort.
	 * Each thread sorts its own slice of the samples array.
	 */
	public static void gpuComputeQuantiles(
			KernelContext context,
			int n_star, int G,
			double[] samples,
			int hfL, int hfU,
			double interpL, double interpU,
			double[] intervals
	) {
		int i = context.globalIdx;
		if (i >= n_star) return;

		int base = i * G;

		// Partial selection sort from bottom to find x_{(hfL-1)}, x_{(hfL)}
		for (int k = 0; k <= hfL; k++) {
			int m = base + k;
			for (int j = base + k + 1; j < base + G; j++) {
				if (samples[j] < samples[m]) m = j;
			}
			double tmp = samples[base + k]; samples[base + k] = samples[m]; samples[m] = tmp;
		}
		double x_hfL_m1 = samples[base + hfL - 1];
		double x_hfL    = samples[base + hfL];
		intervals[i * 2] = x_hfL_m1 + interpL * (x_hfL - x_hfL_m1);

		// Partial selection sort from top to find x_{(hfU-1)}, x_{(hfU)}
		int kU_needed = G - hfU;
		for (int k = 0; k <= kU_needed; k++) {
			int m = base + G - 1 - k;
			for (int j = base; j < base + G - 1 - k; j++) {
				if (samples[j] > samples[m]) m = j;
			}
			double tmp = samples[base + G - 1 - k]; samples[base + G - 1 - k] = samples[m]; samples[m] = tmp;
		}
		double x_hfU    = samples[base + hfU];
		double x_hfU_m1 = samples[base + hfU - 1];
		intervals[i * 2 + 1] = x_hfU_m1 + interpU * (x_hfU - x_hfU_m1);
	}

	// -------------------------------------------------------------------------
	// GPU dispatch entry points
	// -------------------------------------------------------------------------

	public static double[] predictWithGpu(
			double[][] records, FlatForest forest,
			double halfDiff, double yRange, double yMin) {

		int n_star = records.length;
		int p = n_star > 0 ? records[0].length : 0;

		double[] flatRecords = new double[n_star * p];
		for (int i = 0; i < n_star; i++) System.arraycopy(records[i], 0, flatRecords, i * p, p);

		double[] yHats = new double[n_star];
		int[] dims = {n_star, forest.G, forest.T, p};

		KernelContext ctx = new KernelContext();
		GridScheduler scheduler = new GridScheduler("gpuPredict.predictMeans", new WorkerGrid1D(n_star));

		TaskGraph tg = new TaskGraph("gpuPredict")
				.transferToDevice(DataTransferMode.EVERY_EXECUTION,
						dims, flatRecords,
						forest.treeOffsets,
						forest.splitAttr, forest.splitVal,
						forest.leftChild, forest.rightChild,
						forest.sendMissRight, forest.yPred)
				.task("predictMeans", GpuForestPredictor::gpuPredictMeans,
						ctx, dims, flatRecords,
						forest.treeOffsets,
						forest.splitAttr, forest.splitVal,
						forest.leftChild, forest.rightChild,
						forest.sendMissRight, forest.yPred,
						halfDiff, yRange, yMin,
						yHats)
				.transferToHost(DataTransferMode.EVERY_EXECUTION, yHats);

		ImmutableTaskGraph immutable = tg.snapshot();
		try (TornadoExecutionPlan plan = new TornadoExecutionPlan(immutable)) {
			plan.withGridScheduler(scheduler).execute();
		} catch (Exception e) {
			throw new RuntimeException("TornadoVM GPU execution failed", e);
		}
		return yHats;
	}

	public static double[][] predictPosteriorSamplesWithGpu(
			double[][] records, FlatForest forest,
			double halfDiff, double yRange, double yMin) {

		int n_star = records.length;
		int G = forest.G;
		int p = n_star > 0 ? records[0].length : 0;

		if ((long) n_star * G > 200_000_000L)
			throw new RuntimeException("Prediction volume too large for GPU memory.");

		double[] flatRecords = new double[n_star * p];
		for (int i = 0; i < n_star; i++) System.arraycopy(records[i], 0, flatRecords, i * p, p);

		double[] flatSamples = new double[n_star * G];
		int[] dims = {n_star, G, forest.T, p};

		KernelContext ctx = new KernelContext();
		GridScheduler scheduler = new GridScheduler("gpuPredictSamples.predictSamples", new WorkerGrid1D(n_star));

		TaskGraph tg = new TaskGraph("gpuPredictSamples")
				.transferToDevice(DataTransferMode.EVERY_EXECUTION,
						dims, flatRecords,
						forest.treeOffsets,
						forest.splitAttr, forest.splitVal,
						forest.leftChild, forest.rightChild,
						forest.sendMissRight, forest.yPred)
				.task("predictSamples", GpuForestPredictor::gpuPredictPosteriorSamples,
						ctx, dims, flatRecords,
						forest.treeOffsets,
						forest.splitAttr, forest.splitVal,
						forest.leftChild, forest.rightChild,
						forest.sendMissRight, forest.yPred,
						halfDiff, yRange, yMin,
						flatSamples)
				.transferToHost(DataTransferMode.EVERY_EXECUTION, flatSamples);

		ImmutableTaskGraph immutable = tg.snapshot();
		try (TornadoExecutionPlan plan = new TornadoExecutionPlan(immutable)) {
			plan.withGridScheduler(scheduler).execute();
		} catch (Exception e) {
			throw new RuntimeException("TornadoVM GPU execution failed", e);
		}

		double[][] samples = new double[n_star][G];
		for (int i = 0; i < n_star; i++) System.arraycopy(flatSamples, i * G, samples[i], 0, G);
		return samples;
	}

	public static double[] predictClassificationMeansWithGpu(
			double[][] records, FlatForest forest) {

		int n_star = records.length;
		int p = n_star > 0 ? records[0].length : 0;

		double[] flatRecords = new double[n_star * p];
		for (int i = 0; i < n_star; i++) System.arraycopy(records[i], 0, flatRecords, i * p, p);

		double[] pHats = new double[n_star];

		KernelContext ctx = new KernelContext();
		GridScheduler scheduler = new GridScheduler("gpuPredictClassMeans.predictClassMeans", new WorkerGrid1D(n_star));

		TaskGraph tg = new TaskGraph("gpuPredictClassMeans")
				.transferToDevice(DataTransferMode.EVERY_EXECUTION,
						flatRecords,
						forest.treeOffsets,
						forest.splitAttr, forest.splitVal,
						forest.leftChild, forest.rightChild,
						forest.sendMissRight, forest.yPred)
				.task("predictClassMeans", GpuForestPredictor::gpuPredictClassificationMeans,
						ctx,
						n_star, forest.G, forest.T, p,
						flatRecords,
						forest.treeOffsets,
						forest.splitAttr, forest.splitVal,
						forest.leftChild, forest.rightChild,
						forest.sendMissRight, forest.yPred,
						pHats)
				.transferToHost(DataTransferMode.EVERY_EXECUTION, pHats);

		ImmutableTaskGraph immutable = tg.snapshot();
		try (TornadoExecutionPlan plan = new TornadoExecutionPlan(immutable)) {
			plan.withGridScheduler(scheduler).execute();
		} catch (Exception e) {
			throw new RuntimeException("TornadoVM GPU execution failed", e);
		}
		return pHats;
	}

	public static double[][] predictClassificationPosteriorSamplesWithGpu(
			double[][] records, FlatForest forest) {

		int n_star = records.length;
		int G = forest.G;
		int p = n_star > 0 ? records[0].length : 0;

		if ((long) n_star * G > 200_000_000L)
			throw new RuntimeException("Prediction volume too large for GPU memory.");

		double[] flatRecords = new double[n_star * p];
		for (int i = 0; i < n_star; i++) System.arraycopy(records[i], 0, flatRecords, i * p, p);

		double[] flatProbs = new double[n_star * G];

		KernelContext ctx = new KernelContext();
		GridScheduler scheduler = new GridScheduler("gpuPredictClassSamples.predictClassSamples", new WorkerGrid1D(n_star));

		TaskGraph tg = new TaskGraph("gpuPredictClassSamples")
				.transferToDevice(DataTransferMode.EVERY_EXECUTION,
						flatRecords,
						forest.treeOffsets,
						forest.splitAttr, forest.splitVal,
						forest.leftChild, forest.rightChild,
						forest.sendMissRight, forest.yPred)
				.task("predictClassSamples", GpuForestPredictor::gpuPredictClassificationPosteriorSamples,
						ctx,
						n_star, G, forest.T, p,
						flatRecords,
						forest.treeOffsets,
						forest.splitAttr, forest.splitVal,
						forest.leftChild, forest.rightChild,
						forest.sendMissRight, forest.yPred,
						flatProbs)
				.transferToHost(DataTransferMode.EVERY_EXECUTION, flatProbs);

		ImmutableTaskGraph immutable = tg.snapshot();
		try (TornadoExecutionPlan plan = new TornadoExecutionPlan(immutable)) {
			plan.withGridScheduler(scheduler).execute();
		} catch (Exception e) {
			throw new RuntimeException("TornadoVM GPU execution failed", e);
		}

		double[][] samples = new double[n_star][G];
		for (int i = 0; i < n_star; i++) System.arraycopy(flatProbs, i * G, samples[i], 0, G);
		return samples;
	}

	public static double[][] predictCredibleIntervalsWithGpu(
			double[][] records, FlatForest forest,
			double halfDiff, double yRange, double yMin,
			double coverage) {

		int n_star = records.length;
		int G = forest.G;
		int p = n_star > 0 ? records[0].length : 0;

		double[] flatRecords = new double[n_star * p];
		for (int i = 0; i < n_star; i++) System.arraycopy(records[i], 0, flatRecords, i * p, p);
		double[] flatSamples  = new double[n_star * G];
		double[] flatIntervals = new double[n_star * 2];

		double pL = (1.0 - coverage) / 2.0, pU = 1.0 - pL;
		double hL = (G - 1) * pL + 1,       hU = (G - 1) * pU + 1;
		int hfL = (int) Math.floor(hL),      hfU = (int) Math.floor(hU);
		double interpL = hL - hfL,           interpU = hU - hfU;

		int[] dims = {n_star, G, forest.T, p};

		KernelContext ctx1 = new KernelContext();
		KernelContext ctx2 = new KernelContext();
		GridScheduler scheduler = new GridScheduler();
		scheduler.addWorkerGrid("gpuIntervals.predictSamples",   new WorkerGrid1D(n_star));
		scheduler.addWorkerGrid("gpuIntervals.computeQuantiles", new WorkerGrid1D(n_star));

		TaskGraph tg = new TaskGraph("gpuIntervals")
				.transferToDevice(DataTransferMode.EVERY_EXECUTION,
						dims, flatRecords,
						forest.treeOffsets, forest.splitAttr, forest.splitVal,
						forest.leftChild, forest.rightChild, forest.sendMissRight, forest.yPred)
				.task("predictSamples", GpuForestPredictor::gpuPredictPosteriorSamples,
						ctx1, dims, flatRecords, forest.treeOffsets, forest.splitAttr, forest.splitVal,
						forest.leftChild, forest.rightChild, forest.sendMissRight, forest.yPred,
						halfDiff, yRange, yMin,
						flatSamples)
				.task("computeQuantiles", GpuForestPredictor::gpuComputeQuantiles,
						ctx2, n_star, G, flatSamples, hfL, hfU, interpL, interpU, flatIntervals)
				.transferToHost(DataTransferMode.EVERY_EXECUTION, flatIntervals);

		ImmutableTaskGraph immutable = tg.snapshot();
		try (TornadoExecutionPlan plan = new TornadoExecutionPlan(immutable)) {
			plan.withGridScheduler(scheduler).execute();
		} catch (Exception e) {
			throw new RuntimeException("TornadoVM GPU execution failed", e);
		}

		double[][] intervals = new double[n_star][2];
		for (int i = 0; i < n_star; i++) {
			intervals[i][0] = flatIntervals[i * 2];
			intervals[i][1] = flatIntervals[i * 2 + 1];
		}
		return intervals;
	}

	public static double[][] predictClassificationCredibleIntervalsWithGpu(
			double[][] records, FlatForest forest, double coverage) {

		int n_star = records.length;
		int G = forest.G;
		int p = n_star > 0 ? records[0].length : 0;

		double[] flatRecords  = new double[n_star * p];
		for (int i = 0; i < n_star; i++) System.arraycopy(records[i], 0, flatRecords, i * p, p);
		double[] flatSamples   = new double[n_star * G];
		double[] flatIntervals = new double[n_star * 2];

		double pL = (1.0 - coverage) / 2.0, pU = 1.0 - pL;
		double hL = (G - 1) * pL + 1,       hU = (G - 1) * pU + 1;
		int hfL = (int) Math.floor(hL),      hfU = (int) Math.floor(hU);
		double interpL = hL - hfL,           interpU = hU - hfU;

		KernelContext ctx1 = new KernelContext();
		KernelContext ctx2 = new KernelContext();
		GridScheduler scheduler = new GridScheduler();
		scheduler.addWorkerGrid("gpuClassIntervals.predictClassSamples", new WorkerGrid1D(n_star));
		scheduler.addWorkerGrid("gpuClassIntervals.computeQuantiles",    new WorkerGrid1D(n_star));

		TaskGraph tg = new TaskGraph("gpuClassIntervals")
				.transferToDevice(DataTransferMode.EVERY_EXECUTION,
						flatRecords,
						forest.treeOffsets, forest.splitAttr, forest.splitVal,
						forest.leftChild, forest.rightChild, forest.sendMissRight, forest.yPred)
				.task("predictClassSamples", GpuForestPredictor::gpuPredictClassificationPosteriorSamples,
						ctx1,
						n_star, G, forest.T, p,
						flatRecords, forest.treeOffsets, forest.splitAttr, forest.splitVal,
						forest.leftChild, forest.rightChild, forest.sendMissRight, forest.yPred,
						flatSamples)
				.task("computeQuantiles", GpuForestPredictor::gpuComputeQuantiles,
						ctx2, n_star, G, flatSamples, hfL, hfU, interpL, interpU, flatIntervals)
				.transferToHost(DataTransferMode.EVERY_EXECUTION, flatIntervals);

		ImmutableTaskGraph immutable = tg.snapshot();
		try (TornadoExecutionPlan plan = new TornadoExecutionPlan(immutable)) {
			plan.withGridScheduler(scheduler).execute();
		} catch (Exception e) {
			throw new RuntimeException("TornadoVM GPU execution failed", e);
		}

		double[][] intervals = new double[n_star][2];
		for (int i = 0; i < n_star; i++) {
			intervals[i][0] = flatIntervals[i * 2];
			intervals[i][1] = flatIntervals[i * 2 + 1];
		}
		return intervals;
	}

	// -------------------------------------------------------------------------
	// Immutable holder for the flattened forest data
	// -------------------------------------------------------------------------

	public static final class FlatForest {
		public final int[]    treeOffsets;
		public final int[]    splitAttr;
		public final double[] splitVal;
		public final int[]    leftChild;
		public final int[]    rightChild;
		public final int[]    sendMissRight;
		public final double[] yPred;
		public final int G, T;

		FlatForest(int[] treeOffsets, int[] splitAttr, double[] splitVal,
				int[] leftChild, int[] rightChild, int[] sendMissRight,
				double[] yPred, int G, int T) {
			this.treeOffsets   = treeOffsets;
			this.splitAttr     = splitAttr;
			this.splitVal      = splitVal;
			this.leftChild     = leftChild;
			this.rightChild    = rightChild;
			this.sendMissRight = sendMissRight;
			this.yPred         = yPred;
			this.G = G;
			this.T = T;
		}
	}
}
