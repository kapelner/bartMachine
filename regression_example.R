library(bartMachine)
x = 1 : 100; y = x + rnorm(100)
bart_machine = build_bart_machine(as.data.frame(x), y)
save.image("test_bart_machine.RData")
#close R, open R
library(bartMachine)
load("test_bart_machine.RData")
bart_machine$java_bart_machine
#[1] "Java-Object<null>"
## doesn't work

R
library(bartMachine)
x = 1 : 100; y = x + rnorm(100)
bart_machine = build_bart_machine(as.data.frame(x), y)
serialized = .jserialize(bart_machine$java_bart_machine)
serialized = .jcall("RJavaClassLoader", "[B", "toByte", .jcast(bart_machine$java_bart_machine, "java.lang.Object"))
save.image("test_bart_machine.RData")
q("no")
#close R, open R
R
library(bartMachine)
load("test_bart_machine.RData")
#now we have to init Java just like bartMachine does
mem_flag_as_string = "-Xmx1100m"
#we pass in the mem flag TWICE due to bug in MAC OS X which will be fixed in rJava 0.9.6
#since it works with all versions of rJava, we keep this here in case someone may happen to
#be running MAC OS X with rJAVA version < 0.9.6
.jinit(parameters = c(mem_flag_as_string, mem_flag_as_string))
JAR_DEPENDENCIES = c("bart_java.jar", "commons-math-2.1.jar", "trove-3.0.3.jar", "junit-4.10.jar")
for (dependency in JAR_DEPENDENCIES){
	.jaddClassPath(paste(find.package("bartMachine"), "/java/", dependency, sep = ""))
}
.jclassPath()
bart_machine$java_bart_machine = .junserialize(serialized)
#Error in .jcall("RJavaClassLoader", "Ljava/lang/Object;", "toObjectPL",  : 
#				java.lang.ClassNotFoundException



## try using jcache
library(bartMachine)
x = 1 : 100; y = x + rnorm(100)
bart_machine = build_bart_machine(as.data.frame(x), y)
.jcache(bart_machine$java_bart_machine)
save.image("test_bart_machine.RData")
q("no")
#close R, open R
R
library(bartMachine)
load("test_bart_machine.RData")
.jinit()
bart_machine$java_bart_machine
#[1] "Java-Object<null>"
## doesn't work

library(bartMachine)
x = 1 : 100; y = x + rnorm(100)
for (i in 1 : 10000){
	gc()
	bart_machine = build_bart_machine(as.data.frame(x), y)
}

## If it helps, this may

#get some data
library(MASS)
data(Boston)
X = Boston
y = X$medv
X$medv = NULL

Xtrain = X[1 : (nrow(X) / 2), ]
ytrain = y[1 : (nrow(X) / 2)]
Xtest = X[(nrow(X) / 2 + 1) : nrow(X), ]
ytest = y[(nrow(X) / 2 + 1) : nrow(X)]

set_bart_machine_num_cores(4)
bart_machine = build_bart_machine(Xtrain, ytrain,
		num_trees = 200,
		num_burn_in = 300,
		num_iterations_after_burn_in = 1000,
		use_missing_data = TRUE,
		debug_log = TRUE,
		verbose = TRUE)
bart_machine

plot_y_vs_yhat(bart_machine)

yhat = predict(bart_machine, Xtest)