bartMachine
===========

An R-Java Bayesian Additive Regression Trees implementation (BART)
Software for Supervised Statistical Learning

Copyright (C) 2022
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
9/13/22

v1.3.1 is released and fixed bugs in v1.3 due to something I can't figure out in the fastutil library. Please don't use v1.3 --- upgrade to v1.3.1 or downgrade to v1.2.7

8/25/22

v1.3 is released (and should be on CRAN soon) which comes from some nice speedups. Note it is not backwards compatible with previous versions. To use previous versions' models, install a previous version manually e.g. via `install.packages("https://cran.r-project.org/src/contrib/Archive/bartMachine/bartMachine_1.2.1.tar.gz", repos=NULL, type="source")`.

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

Download the latest [Java JDK](https://jdk.java.net/) and install it properly. (Java 7 or above is required for v1.2.x and Java 8 or above is required for v1.3 and above). bartMachine requires rJava which requires the JDK; you cannot just have a JRE!

### Install rJava

Use `install.packages("rJava")` within R. If you experience errors, make sure your `JAVA_HOME` system variable is set to the root of your java installation (on a windows machine that would look something like `C:\Program Files\Java\jdk-13.0.2`). Also try running `R CMD javareconf` from the command line. On ubuntu, you should run `sudo apt-get install r-cran-rjava` to install from the command prompt. If you still have errors, you are not alone! rJava is tough to install and idiosyncratic across different platforms. Google your error message! The majority of issues have been resolved on Q&A forums.

### Install bartMachine via CRAN

Use `install.packages("bartMachine")` within R and then `options(java.parameters = "-Xmx2500m")` to set a larger amount of RAM than the default of 500MB and then `library(bartMachine)`.

### Install bartMachine via compilation from source

Due to CRAN limitations, we cannot release bartMachine for Java >7. Thus it is recommended to install bartMachine from source. This is recommended as you will get the benefits of Java 8-14 as well as the latest release of bartMachine if it's not on CRAN yet (see [changelog](https://github.com/kapelner/bartMachine/blob/master/bartMachine/CHANGELOG).

1. Make sure you have [git](http://git-scm.com/downloads "Download git for all operating systems") 
properly installed.

2. Run `git clone https://github.com/kapelner/bartMachine.git` from your command line and navigate into the cloned project directory via `cd bartMachine`.

3. Make sure you have a [Java JDK](https://jdk.java.net/14/) installed properly. Then make sure the bin directory is an element in the PATH variable (on a windows machine it would look something like `C:\Program Files\Java\jdk-13.0.2\bin`). We also recommend making a system variable `JAVA_HOME` pointing to the directory (save \bin).

3. Make sure you have [apache ant](http://ant.apache.org/bindownload.cgi "Download apache ant for all operating systems") installed properly. 
Make sure you add the bin directory for ant to your system PATH variable (on a windows machine it would be something like `C:\Program Files (x86)\apache-ant-1.10.8\bin`). We also recommend making a system variable `ANT_HOME` pointing to the directory (save \bin).

4. Compile the JAVA source code into a JAR using `ant`. You should see a compilation record and then `BUILD SUCCESSFUL` and a total time.

5. Now you can install the package into R using `R CMD INSTALL bartMachine`. On Windows systems, this may fail because it expects multiple architectures. This can be corrected by running `R CMD INSTALL --no-multiarch bartMachine` (I haven't seen this issue in years though). This may also fail if you don't have the required packages installed (run `install.packages("bartMachineJARs")` and `install.packages("missForest")`). Upon successful installation, the last line of the output should read `DONE (bartMachine)`. In R, you can now run `library(bartMachine)` and start using the package normally.

Acknowledgements
------------------

We thank Ed George, Abba Krieger, Shene Jensen and Richard Berk for helpful discussions. We thank Matt Olson for pointing out an important memory issue. We thank [JProfiler](http://www.ej-technologies.com/products/jprofiler/overview.html) for profiling the code which allowed us to create a lean implementation.  We also thank the Laboratory for Web Algorithmics at the Università degli studi di Milano for [fastutil](https://fastutil.di.unimi.it/).
