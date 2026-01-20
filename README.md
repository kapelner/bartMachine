IMPORTANT
===========

* For the newest version >=1.4, *before* you load the package, you must set *both* the memory and the special speedup module and newer GC params are also recommended: `options(java.parameters = c("-Xmx20g", "--add-modules=jdk.incubator.vector", "-XX:+UseZGC"))`. If you don't do this you will get errors such as `Error in .jnew("bartMachine.bartMachineRegressionMultThread") : java.lang.NoClassDefFoundError: jdk/incubator/vector/Vector`.

* For version <1.4, you must set the memory before `options(java.parameters = "-Xmx20g")` to set a larger amount of RAM than the default of 500MB which will get you intro trouble. Only after setting these options, then invoke `library(bartMachine)`. If you don't do this YOU WILL GET OUT OF MEMORY ERRORS OR STUFF THAT LOOKS LIKE THIS `Error in validObject(.Object) : invalid class “jobjRef” object: invalid object for slot "jobj" in class "jobjRef": got class "NULL", should be or extend class "externalptr"`.


bartMachine
===========

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

January, 2026

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

Download the latest [Java JDK](https://jdk.java.net/) and install it properly. (Java >=21 is required for version >=1.4; Java >=8 is required for version <1.4). bartMachine requires `rJava` which requires the JDK; you cannot just have a JRE!

### Install rJava

Use `install.packages("rJava")` within R. If you experience errors, make sure your `JAVA_HOME` system variable is set to the root of your java installation (on a windows machine that would look something like `C:\Program Files\Java\jdk-13.0.2`). Also try running `R CMD javareconf` from the command line. On ubuntu, you should run `sudo apt-get install r-cran-rjava` to install from the command prompt. If you still have errors, you are not alone! rJava is tough to install and idiosyncratic across different platforms. Google your error message! The majority of issues have been resolved on Q&A forums.

### Install bartMachine via CRAN

Use `install.packages("bartMachine")` within R.

### Install bartMachine via compilation from source

1. Make sure you have [git](http://git-scm.com/downloads "Download git for all operating systems") 
properly installed.

2. Run `git clone https://github.com/kapelner/bartMachine.git` from your command line and navigate into the cloned project directory via `cd bartMachine`.

3. Make sure you have the latest [Java JDK](https://www.oracle.com/java/technologies/downloads/) installed properly. Then make sure the bin directory is an element in the PATH variable. We also recommend making a system variable `JAVA_HOME` pointing to the directory.

3. Make sure you have [apache ant](http://ant.apache.org/bindownload.cgi "Download apache ant for all operating systems") installed properly. 
Make sure you add the bin directory for ant to your system PATH variable (on a windows machine it would be something like `C:\Program Files (x86)\apache-ant-1.10.15\bin`). We also recommend making a system variable `ANT_HOME` pointing to the directory (save \bin).

4. Compile the JAVA source code into a JAR using `ant`. You should see a compilation record and then `BUILD SUCCESSFUL` and a total time.

5. Now you can install the package into R using `R CMD INSTALL bartMachine`. On Windows systems, this may fail because it expects multiple architectures. This can be corrected by running `R CMD INSTALL --no-multiarch bartMachine` (I haven't seen this issue in years though). This may also fail if you don't have the required packages installed (run `install.packages("bartMachineJARs")` and `install.packages("missForest")`). Upon successful installation, the last line of the output should read `DONE (bartMachine)`. In R, you can now run `library(bartMachine)` and start using the package normally.


#### Limiting CPU usage

(At least under GNU/Linux) even if you set `set_bart_machine_num_cores(1)`, CPU usage per process can be much larger than 100% (reaching at times 200% or 300%). This can lead to CPU overloading, especially if you run multiple bartMachines in parallel (for example, if you use the [SuperLearner](https://cran.r-project.org/web/packages/SuperLearner/) package and use parallelization). This seems to be a consequence of the garbage collector. One way to avoid this problem is to issue `Sys.setenv(JAVA_TOOL_OPTIONS = "-XX:ParallelGCThreads=1")` *before* invoking `library(bartMachine)`. (If you use a cluster, for example a SNOW cluster, you will want to do this in the slaves too, for example `clusterEvalQ(the_name_of_your_cluster, {Sys.setenv(JAVA_TOOL_OPTIONS = "-XX:ParallelGCThreads=1")})`).

Benchmarks v1.4.1.1 to v1.3.5
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

We thank Ed George, Abba Krieger, Shene Jensen and Richard Berk for helpful discussions. We thank Matt Olson for pointing out an important memory issue. We thank [JProfiler](http://www.ej-technologies.com/products/jprofiler/overview.html) for profiling the code which allowed us to create a lean implementation.
