package bartMachine;

import java.lang.reflect.Method;

/**
 * Bridges between the core BART code and GpuForestPredictor.
 * Uses reflection to avoid a compile-time dependency on TornadoVM.
 * GpuForestPredictor.java is excluded from the main compilation pass
 * and only compiled if the TornadoVM jar is present.
 */
public final class GpuPredictorBridge {

    private static final Method FLATTEN_METHOD;
    private static final Method PREDICT_MEANS_METHOD;
    private static final Method PREDICT_SAMPLES_METHOD;
    private static final Method PREDICT_CLASS_MEANS_METHOD;
    private static final Method PREDICT_CLASS_SAMPLES_METHOD;
    private static final Method PREDICT_CRED_INTERVALS_METHOD;
    private static final Method PREDICT_CLASS_CRED_INTERVALS_METHOD;
    
    public static final boolean GPU_AVAILABLE;

    static {
        Method flat = null, means = null, samples = null, cMeans = null, cSamples = null, cred = null, cCred = null;
        boolean avail = false;
        try {
            Class<?> cls = Class.forName("bartMachine.GpuForestPredictor");
            avail = cls.getField("GPU_AVAILABLE").getBoolean(null);
            if (avail) {
                Class<?> forestCls = Class.forName("bartMachine.GpuForestPredictor$FlatForest");
                flat = cls.getMethod("flatten", bartMachineTreeNode[][].class, int.class, int.class);
                means = cls.getMethod("predictWithGpu", double[][].class, 
                        forestCls, double.class, double.class, double.class);
                samples = cls.getMethod("predictPosteriorSamplesWithGpu", double[][].class, 
                        forestCls, double.class, double.class, double.class);
                cMeans = cls.getMethod("predictClassificationMeansWithGpu", double[][].class, 
                        forestCls);
                cSamples = cls.getMethod("predictClassificationPosteriorSamplesWithGpu", double[][].class, 
                        forestCls);
                cred = cls.getMethod("predictCredibleIntervalsWithGpu", double[][].class,
                        forestCls, double.class, double.class, double.class, double.class);
                cCred = cls.getMethod("predictClassificationCredibleIntervalsWithGpu", double[][].class,
                        forestCls, double.class);
            }
        } catch (Throwable t) {
            avail = false;
        }
        FLATTEN_METHOD = flat;
        PREDICT_MEANS_METHOD = means;
        PREDICT_SAMPLES_METHOD = samples;
        PREDICT_CLASS_MEANS_METHOD = cMeans;
        PREDICT_CLASS_SAMPLES_METHOD = cSamples;
        PREDICT_CRED_INTERVALS_METHOD = cred;
        PREDICT_CLASS_CRED_INTERVALS_METHOD = cCred;
        GPU_AVAILABLE = avail;
    }

    public static Object flatten(bartMachineTreeNode[][] samples, int G, int T) throws Exception {
        return FLATTEN_METHOD.invoke(null, samples, G, T);
    }

    public static double[] predictMeans(double[][] records, Object forest, double halfDiff, double range, double min) throws Exception {
        return (double[]) PREDICT_MEANS_METHOD.invoke(null, records, forest, halfDiff, range, min);
    }

    public static double[][] predictSamples(double[][] records, Object forest, double halfDiff, double range, double min) throws Exception {
        return (double[][]) PREDICT_SAMPLES_METHOD.invoke(null, records, forest, halfDiff, range, min);
    }

    public static double[] predictClassMeans(double[][] records, Object forest) throws Exception {
        return (double[]) PREDICT_CLASS_MEANS_METHOD.invoke(null, records, forest);
    }

    public static double[][] predictClassSamples(double[][] records, Object forest) throws Exception {
        return (double[][]) PREDICT_CLASS_SAMPLES_METHOD.invoke(null, records, forest);
    }

    public static double[][] predictCredibleIntervals(double[][] records, Object forest, double halfDiff, double range, double min, double coverage) throws Exception {
        return (double[][]) PREDICT_CRED_INTERVALS_METHOD.invoke(null, records, forest, halfDiff, range, min, coverage);
    }

    public static double[][] predictClassCredibleIntervals(double[][] records, Object forest, double coverage) throws Exception {
        return (double[][]) PREDICT_CLASS_CRED_INTERVALS_METHOD.invoke(null, records, forest, coverage);
    }
}
