IMPORTANT
===========

* For the newest version >=1.4, *before* you load the package, you must set *both* the memory and the special speedup module and newer GC params are also recommended: `options(java.parameters = c("-Xmx20g", "--add-modules=jdk.incubator.vector", "-XX:+UseZGC"))`. If you don't do this you will get errors such as `Error in .jnew("bartMachine.bartMachineRegressionMultThread") : java.lang.NoClassDefFoundError: jdk/incubator/vector/Vector`. If you want GPU optimizations, see the necessary options setup in this README further on.

* For version <1.4, you must set the memory before `options(java.parameters = "-Xmx20g")` to set a larger amount of RAM than the default of 500MB which will get you intro trouble. Only after setting these options, then invoke `library(bartMachine)`. If you don't do this YOU WILL GET OUT OF MEMORY ERRORS OR STUFF THAT LOOKS LIKE THIS `Error in validObject(.Object) : invalid class “jobjRef” object: invalid object for slot "jobj" in class "jobjRef": got class "NULL", should be or extend class "externalptr"`.


bartMachine
===========

[![CRAN status](https://www.r-pkg.org/badges/version/bartMachine)](https://CRAN.R-project.org/package=bartMachine)
[![R-universe version](https://kapelner.r-universe.dev/bartMachine/badges/version)](https://kapelner.r-universe.dev/bartMachine)
[![R-universe checks](https://kapelner.r-universe.dev/bartMachine/badges/checks)](https://kapelner.r-universe.dev/bartMachine)
[![GitHub Actions pkgdown](https://github.com/kapelner/bartMachine/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/kapelner/bartMachine/actions/workflows/pkgdown.yaml)

An R-Java Bayesian Additive Regression Trees implementation (BART)
Software for Supervised Statistical Learning

Copyright (C) 2026
Adam Kapelner  
Department of Mathematics, Queens College, City University of New York 
& 
Justin Bleich
Department of Statistics, The Wharton School of the University of Pennsylvania

This is a Java implementation of the algorithm found in Chipman, George, & McCulloch 
[BART: Bayesian Additive Regressive Trees. The Annals of Applied Statistics. 
2010 4(1): 266-298](http://projecteuclid.org/DPubS/Repository/1.0/Disseminate?view=body&id=pdfview_1&handle=euclid.aoas/1273584455 "PDF download of the BART paper") as well as many other features.

Recent News
---------

May, 2026

v1.4.2 released - major speedups using GPU via TornadoVM (see relevant sections below). Additional speedups from multiple R -> Java migrations.
v1.3.5-1.4.1.1 released - major speedups using Java 21+ advancements and "Vector API code" (see benchmark section below), switched from legacy trove package to modern (and maintained) [fastutil](https://github.com/vigna/fastutil) package, verbose flag behavior cleaned up, ggplot2 implementation, argument checks, documentation cleanup tests, testing, multiple benchmarks. The package is faster than the `BART` package but slower than the `dbarts` package for both training and prediction. Note: v1.4.1 has a bug where classification doesn't work. Please upgrade to v1.4.1.1.


The Paper
---------

For a vignette describing the BART model and bartMachine's features, see our [JSS paper](https://www.jstatsoft.org/article/view/v070i04).


The Manual
----------

See the [manual](https://github.com/kapelner/bartMachine/blob/master/bartMachine.pdf?raw=true "BART package manual") for detailed information about the 
package's functions and parameters.

 
Setup Instructions
------------------

To install the bartMachine package in R, you first need to install Java and rJava and configure your computer, then you 
can install the package from CRAN or compile from source.

### Install Java JDK (not the JRE)

`bartMachine` requires `rJava` which requires the JDK; you cannot just have a JRE! Download the latest [Java JDK](https://jdk.java.net/) and install it properly. (Java >=21 is required for version >=1.4; Java >=8 is required for version <1.4). Specifically v21 is required for GPU optimizations. 

### Install rJava

Use `install.packages("rJava")` within R. If you experience errors, make sure your `JAVA_HOME` system variable is set to the root of your java installation (on a windows machine that would look something like `C:\Program Files\Java\jdk-13.0.2`). Also try running `R CMD javareconf` from the command line. On ubuntu, you should run `sudo apt-get install r-cran-rjava` to install from the command prompt. If you still have errors, you are not alone! rJava is tough to install and idiosyncratic across different platforms. Google your error message! The majority of issues have been resolved on Q&A forums.

### Install bartMachine via CRAN

```r
install.packages("bartMachine")
```

This route will *not* give you the GPU optimizations (see below).

### Install the latest bartMachine via R-universe

```r
install.packages(
  "bartMachine",
  repos = c(
    kapelner = "https://kapelner.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
  )
)
```

### Install bartMachine via compilation from source (CPU only)

For compiling with GPU optimizations, see next section.

1. Make sure you have [git](http://git-scm.com/downloads "Download git for all operating systems") 
properly installed.

2. Run `git clone https://github.com/kapelner/bartMachine.git` from your command line and navigate into the cloned project directory via `cd bartMachine`.

3. Make sure you have the latest [Java JDK](https://www.oracle.com/java/technologies/downloads/) installed properly. Then make sure the bin directory is an element in the PATH variable. We also recommend making a system variable `JAVA_HOME` pointing to the directory.

3. Make sure you have [apache ant](http://ant.apache.org/bindownload.cgi "Download apache ant for all operating systems") installed properly. 
Make sure you add the bin directory for ant to your system PATH variable (on a windows machine it would be something like `C:\Program Files (x86)\apache-ant-1.10.15\bin`). We also recommend making a system variable `ANT_HOME` pointing to the directory (save \bin).

4. Compile the JAVA source code into a JAR using `ant`. You should see a compilation record and then `BUILD SUCCESSFUL` and a total time.

5. Now you can install the package into R using `R CMD INSTALL bartMachine`. On Windows systems, this may fail because it expects multiple architectures. This can be corrected by running `R CMD INSTALL --no-multiarch bartMachine` (I haven't seen this issue in years though). This may also fail if you don't have the required packages installed (e.g. run `install.packages("bartMachineJARs")`, `install.packages("missForest")`, etc). Upon successful installation, the last line of the output should read `DONE (bartMachine)`. In R, you can now run `library(bartMachine)` and start using the package normally.


### Install bartMachine via compilation from source (GPU + CPU)

GPU acceleration is available via [TornadoVM](https://tornadovm.readthedocs.io/), which dispatches prediction kernels to CUDA, metal, etc. devices. The build auto-detects TornadoVM — no special flags are needed when TornadoVM is properly installed. But installing TornadoVM sometimes is vexing. Read below carefully.

1. Follow steps 1–3 of the CPU-only instructions above (git clone, Java JDK v21, Apache Ant). At the time of this writing, TornadoVM unfortunately only works with Java JDK version 21, so you must download that version.

2. Install [TornadoVM](https://tornadovm.readthedocs.io/en/latest/installation.html). The TornadoVM installer sets the `TORNADO_SDK` environment variable; make sure it is set in the shell where you will run `ant`.

3. Compile with `ant clean`. The build will automatically find `$TORNADO_SDK/share/java/tornado/tornado-api*.jar` and compile `GpuForestPredictor.java` in a second pass. You will see a line like `TornadoVM detected at: ...` in the build output. For non-standard installs where `TORNADO_SDK` is not set, you can override manually: `ant -Dtornadovm.jar=/path/to/tornado-api.jar clean`.

4. Install into R using `R CMD INSTALL bartMachine`. **IMPORTANT: You must run this command after building with `ant` to ensure the updated `bart_java.jar` is moved into your R library.**

5. Before loading the package in R, set the JVM flags for your GPU hardware. The five base flags are always required; the rest are TornadoVM backend flags.

   - `-Xmx20g` — heap size
   - `--add-modules=jdk.incubator.vector` — Vector API (required by bartMachine ≥ 1.4)
   - `-XX:+UseZGC` — low-pause GC recommended for real-time workloads
   - `--enable-preview` — TornadoVM's JARs are compiled with Java preview features
   - `-javaagent:/path/to/bart_java.jar=deps` — TornadoVM's kernel compiler reads class bytes via `ClassLoader.getSystemClassLoader()`; rJava adds this jar to a child class loader after JVM startup, so the system loader cannot see it. Using the jar as a Java agent causes the JVM to append it to the system classpath before any user classes load (Java SE specification guarantee). Since `bart_java.jar` is then on the system classpath, its dependencies (from `bartMachineJARs`) must also be added to the system classpath search; we pass them as agent arguments.

   **Recommended setup snippet for GPU:**
   ```r
   # 1. Locate the main jar and dependency jars
   bart_jar <- system.file("java", "bart_java.jar", package = "bartMachine")
   deps     <- c(
     system.file("java", "fastutil-core-8.5.18.jar", package = "bartMachineJARs"),
     system.file("java", "commons-math-2.1.jar",     package = "bartMachineJARs")
   )
   deps <- deps[nzchar(deps) & file.exists(deps)]
   agent_flag <- paste0("-javaagent:", bart_jar, "=", paste(deps, collapse = .Platform$path.sep))

   # 2. Set JVM flags (change backend as needed: ptx-backend, spirv-backend, opencl-backend, metal-backend)
   options(java.parameters = c("-Xmx20g", "--add-modules=jdk.incubator.vector", "-XX:+UseZGC",
       "--enable-preview",
       agent_flag,
       "-Dtornado.backends=ptx-backend", #this is for Nvidia (for other hardware, see next five sections below)
       "--add-opens=java.base/jdk.internal.misc=ALL-UNNAMED",
       "--add-opens=java.base/jdk.internal.loader=ALL-UNNAMED"))

   # 3. Load the package
   library(bartMachine)
   ```

   **NVIDIA GPU (CUDA/PTX backend):**
   Keep `-Dtornado.backends=ptx-backend` in the snippet above.

   **Intel GPU (Level Zero / SPIR-V backend):**
   Use `-Dtornado.backends=spirv-backend` in the snippet above instead of `-Dtornado.backends=ptx-backend`.

   **AMD GPU or Intel GPU (OpenCL backend):**
   Use `-Dtornado.backends=opencl-backend` in the snippet above instead of `-Dtornado.backends=ptx-backend`.

   **Mac — Apple Silicon or Intel Mac (OpenCL backend, supported):**
   Use `-Dtornado.backends=opencl-backend` in the snippet above instead of `-Dtornado.backends=ptx-backend`.
   Note: macOS ships with OpenCL (deprecated but functional) and is the recommended path. TornadoVM's Metal backend is still experimental but can be tried on Apple Silicon:

   **Mac — Apple Silicon (Metal backend, experimental):**
   Use `-Dtornado.backends=metal-backend` in the snippet above instead of `-Dtornado.backends=ptx-backend`.

   For the authoritative, version-specific list of all required flags for your TornadoVM build, run `$TORNADO_SDK/bin/tornado --printJVMFlags` and append any additional flags it emits to the `java.parameters` vector above.

6. GPU acceleration is enabled by default. You can toggle it per-session:
   ```r
   options(bartMachine.use_gpu = TRUE)   # default — use GPU when available
   options(bartMachine.use_gpu = FALSE)  # force CPU path
   ```
   At runtime the GPU path activates only when a real CUDA/OpenCL device is found; it falls back to CPU transparently if no GPU is available or if `GpuForestPredictor` cannot be loaded.

The following regression operations dispatch directly to the GPU when the number of test records is ≥ 1,000:

- **Posterior mean prediction** (`predict`)
- **Posterior samples** (`bart_machine_get_posterior`)
- **Credible intervals** (`calc_credible_intervals`)

The following regression operations are Java-parallelized (CPU thread pool) and additionally use the GPU for their internal prediction calls when the dataset is large enough:

- **Variable importance permutations** (`var_selection_by_permute`)
- **Covariate importance tests** (`cov_importance_test`)
- **k-fold cross-validation** (`k_fold_cv`)

Classification and prediction batches with fewer than 1,000 records always use the CPU path.


#### Limiting CPU core usage

(At least under GNU/Linux) even if you set `set_bart_machine_num_cores(1)`, CPU usage per process can be much larger than 100% (reaching at times 200% or 300%). This can lead to CPU overloading, especially if you run multiple bartMachines in parallel (for example, if you use the [SuperLearner](https://cran.r-project.org/web/packages/SuperLearner/) package and use parallelization). This seems to be a consequence of the garbage collector. One way to avoid this problem is to issue `Sys.setenv(JAVA_TOOL_OPTIONS = "-XX:ParallelGCThreads=1")` *before* invoking `library(bartMachine)`. (If you use a cluster, for example a SNOW cluster, you will want to do this in the slaves too, for example `clusterEvalQ(the_name_of_your_cluster, {Sys.setenv(JAVA_TOOL_OPTIONS = "-XX:ParallelGCThreads=1")})`).

#### Benchmarks

v1.4.2 to v1.4.1.1
------------------

GPU prediction speedups were measured on an NVIDIA Quadro T2000 (1024 CUDA cores, 4 GB GDDR5, PTX backend) against 12 CPU threads (Intel Core i7), using 50 trees, 1000 posterior samples, N_TRAIN = 2000, and N_TEST = 50,000 test records. Speedups grow with dataset size; GPU overhead dominates at N_TEST < 1000 (where the CPU path is used automatically).

| Operation | CPU (12-core) | GPU (Quadro T2000) | **Speedup** | Max \|CPU−GPU\| |
| :--- | :--- | :--- | :--- | :--- |
| **Regression: `predict()`** | 6.341 s | 0.417 s | **15.21x** | 7.5e-14 |
| **Regression: `bart_machine_get_posterior()`** | 12.724 s | 5.769 s | **2.21x** | < 1e-15 |
| **Regression: `calc_credible_intervals()`** | 8.057 s | 1.982 s | **4.07x** | < 1e-15 |
| **Classification: `predict(type="prob")`** | 9.805 s | 0.492 s | **19.93x** | 5.5e-8 |
| **Classification: `bart_machine_get_posterior()`** | 15.814 s | 6.605 s | **2.39x** | 1.1e-7 |
| **Classification: `calc_credible_intervals()`** | 10.146 s | 2.096 s | **4.84x** | 1.1e-7 |

Note: `get_posterior` functions return large matrices (N_TEST × N_SAMP), so PCIe memory transfer limits GPU gains. Posterior-mean and credible-interval functions — which return only N_TEST outputs — scale best onto the GPU. The classification numerical differences (~1e-7) reflect GPU vs JVM precision for the `exp()` function used in the probit transform; differences are scientifically negligible.

We also migrated several computationally intensive tasks from R loops to a high-performance Java backend using virtual threads. The following table summarizes the speedups achieved on a 4-core system (CPU-only):

| Operation | Baseline (R) | Current (Java) | **Speedup** | Correctness |
| :--- | :--- | :--- | :--- | :--- |
| **Variable Selection** | 6.568s | 0.701s | **9.37x** | Match: TRUE |
| **Covariate Importance** | 4.364s | 0.662s | **6.59x** | Match: TRUE |
| **K-Fold CV (RMSE)** | 1.802s | 1.439s | **1.25x** | Diff: 0.14 |

Note: Numerical differences in CV results are expected due to initialization variances in the independent multi-threaded BART builds now handled by the JVM.*

The R→Java migration speedups apply on top of the GPU speedups for `var_selection_by_permute`, `cov_importance_test`, and `k_fold_cv`, since their internal prediction calls also dispatch to the GPU when N_TEST ≥ 1000. These results are unshown.

v1.4.1.1 to v1.3.5
------------------

You can see how we did these benchmarks in `run_comparisons.sh`. We compared regression and classification for 1 core and 12 cores for (a) predictions (to ensure they are roughly the same to the previous version) and (b) speed. Here are the results.

```
--- Single-Core Regression ---
Maximum Absolute Difference in Predictions (Last Iter): 3.10832
Average MSE (Old): 2.0015080457
Average MSE (New): 2.0337961078
MSE Difference Mean: 3.2288e-02 (p-val: 4.4706e-01)
Winner (MSE): No Significant Difference
Average Train Time (Old): 8.057 s, (New): 4.369 s
Average Predict Time (Old): 14.013 s, (New): 2.558 s
Training Speedup: 45.77%
Predict Speedup: 81.74%
Training Time p-value:   3.6343e-08
Prediction Time p-value: 1.2483e-10

--- Multi-Core Regression (12 Cores) ---
Maximum Absolute Difference in Predictions (Last Iter): 1.24406
Average MSE (Old): 1.7044720879
Average MSE (New): 1.6851107882
MSE Difference Mean: -1.9361e-02 (p-val: 7.0423e-02)
Winner (MSE): No Significant Difference
Average Train Time (Old): 8.864 s, (New): 2.605 s
Average Predict Time (Old): 1.644 s, (New): 0.307 s
Training Speedup: 70.61%
Predict Speedup: 81.32%
Training Time p-value:   3.5447e-24
Prediction Time p-value: 1.7334e-12

--- Single-Core Classification ---
Maximum Absolute Difference in Predictions (Last Iter): 0.0561842
Average Misclassification Error (Old): 0.1693235294
Average Misclassification Error (New): 0.1694411765
Misclassification Error Difference Mean: 1.1765e-04 (p-val: 8.5238e-01)
Winner (Misclassification Error): No Significant Difference
Average Train Time (Old): 10.508 s, (New): 5.903 s
Average Predict Time (Old): 13.741 s, (New): 2.703 s
Training Speedup: 43.82%
Predict Speedup: 80.33%
Training Time p-value:   1.0643e-10
Prediction Time p-value: 3.8494e-09

--- Multi-Core Classification (12 Cores) ---
Maximum Absolute Difference in Predictions (Last Iter): 0.0584479
Average Misclassification Error (Old): 0.1687058824
Average Misclassification Error (New): 0.1687941176
Misclassification Error Difference Mean: 8.8235e-05 (p-val: 7.6356e-01)
Winner (Misclassification Error): No Significant Difference
Average Train Time (Old): 11.213 s, (New): 4.098 s
Average Predict Time (Old): 1.941 s, (New): 0.318 s
Training Speedup: 63.45%
Predict Speedup: 83.63%
Training Time p-value:   4.0458e-23
Prediction Time p-value: 4.4088e-15
```

Acknowledgements
------------------

We thank Ed George, Abba Krieger, Shene Jensen and Richard Berk for helpful discussions. We thank Matt Olson for pointing out an important memory issue. We thank [JProfiler](http://www.ej-technologies.com/products/jprofiler/overview.html) for profiling the code which allowed us to create a lean implementation. We thank claude, codex and gemini for helping code v1.4+.
