bartMachine
===========

An R-Java Bayesian Additive Regression Trees implementation (BART)
Software for Supervised Statistical Learning

Copyright (C) 2013 Adam Kapelner & Justin Bleich, 
Department of Statistics
The Wharton School of the University of Pennsylvania

This is a Java implementation of the algorithm found in Chipman, George, & McCulloch 
[BART: Bayesian Additive Regressive Trees. The Annals of Applied Statistics. 
2010 4(1): 266-298](http://projecteuclid.org/DPubS/Repository/1.0/Disseminate?view=body&id=pdfview_1&handle=euclid.aoas/1273584455 "PDF download of the BART paper")
as well as many other features.


The Paper
---------

For a vignette describing the BART model and bartMachine's features, see our [vignette](https://github.com/kapelner/bartMachine/blob/master/bartMachine/vignettes/bartMachine_vignette.pdf?raw=true "Download the vignette") 
which is also on arXiv and is currently being reviewed by the Journal of Statistical Software.


The Manual
----------

See the [manual](https://github.com/kapelner/bartMachine/blob/master/bartMachine.pdf?raw=true "BART package manual") for detailed information about the 
package's functions and parameters.

 
Setup Instructions
------------------

To install the bartMachine package in R, you first need to install Java and rJava and configure your computer, then you 
can install the package from CRAN or compile from source

### Install Java JDK (not the JRE)

Download the latest [Java JDK](http://www.oracle.com/technetwork/java/javase/downloads/jdk7-downloads-1880260.html "Download the Java JDK for all operating systems")
and install it properly. rJava requires the JDK; you cannot just have a JRE. We recommend installing JDK for Java 7 so you can get all the latest benefits (the shipped release 
of bartMachine is in Java 6r27 for the most widespread compatibility).

### Install rJava

Use `install.packages("rJava")` within R. If you experience errors, make sure your `JAVA_HOME` system variable is set to the root of your java installation (on a windows machine that
would look something like `C:\Program Files\Java\jdk1.7.0_09`). Also try running `R CMD javareconf` from the command line. On ubuntu, you should run `sudo apt-get install r-cran-rjava`
to install from the command prompt. If you still have errors, google your error message. There are many people who have had problems with the rJava installation in the past and the 
majority of issues have been resolved on forums.

### Install bartMachine via CRAN

Use `install.packages("bartMachine")` within R and then `library(bartMachine)`.

### Install bartMachine via compilation from source

This is recommended as you will get the [benefits of Java 7](http://www.oracle.com/technetwork/java/javase/jdk7-relnotes-418459.html) as well as the latest release of the
software (see [changelog](https://github.com/kapelner/bartMachine/blob/master/bartMachine/CHANGELOG)).

1. Make sure you have [git](http://git-scm.com/downloads "Download git for all operating systems") 
properly installed.

2. Run `git clone https://github.com/kapelner/bartMachine.git` from your command line and navigate into the cloned project directory via `cd bartMachine`.

3. Make sure you have a [Java JDK](http://www.oracle.com/technetwork/java/javase/downloads/jdk7-downloads-1880260.html "Install Java JDK 7 for all operating systems") 
installed properly. Then make sure the bin directory is an element in the PATH variable (on a windows machine it would look something like 
`C:\Program Files\Java\jdk1.7.0_09\bin`).

3. Make sure you have [apache ant](http://ant.apache.org/bindownload.cgi "Download apache ant for all operating systems") installed properly. 
Make sure you add the bin directory for ant to your system PATH variable (on a windows machine it would be something like `C:\Program Files\apache-ant-1.8.4\bin`).
We also recommend making a system variable `ANT_HOME` which would be set to the root of ant (on a windows machine it would be something like 
`C:\Program Files\apache-ant-1.8.4`). Also, double check the `JAVA_HOME` variable to make sure it points to the JDK 

4. Compile the JAVA source code into a JAR using `ant`. You should see a compilation record and then `BUILD SUCCESSFUL` and a total time.

5. Now you can install the package into R using `R CMD INSTALL bartMachine`. On Windows systems, this may fail because it expects multiple architectures. This can 
be corrected by running `R CMD INSTALL --no-multiarch bartMachine`. This may also fail if you don't have the required packages installed (run `install.packages("car")` 
and `install.packages("missForest")`). Upon successful installation, the last line of the output should read `DONE (bartMachine)`. 
In R, you can now run `library(bartMachine)` and start using the package normally.


