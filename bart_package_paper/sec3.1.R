##bart vs bayestree speed runs

library(bartMachine)
library(BayesTree)
library(randomForest)

set_bart_machine_memory(2500)

nlist = c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 40000)
p = 20

n = 20000
num_trees = 50

time_mat = matrix(nrow = length(nlist), ncol = 7)
colnames(time_mat) = c("Him", "Us1coremem", "Us1core", "Us4coremem", "Us4core", "Us4core_no_run", "rf")
rownames(time_mat) = as.character(nlist)

counter = 1

for (n in nlist){	
	cat("n =", n, "\n")

	X = matrix(rnorm(n * p), nrow = n, ncol = p)
	beta = runif(p, -1, 1)
	y = as.numeric(X %*% beta + rnorm(n))
  
	t1 = Sys.time()
	rob_bart = bart(x.train = X, y.train = y, ntree = num_trees, nskip = 250, verbose = FALSE)
	t2 = Sys.time()
	rob_time = t2 - t1
	cat("Rob time =", rob_time, "\n")
	time_mat[as.character(n), "Him"] = rob_time
	  
	set_bart_machine_num_cores(1) ## 1 core
	t3 = Sys.time()
	bart_machine = bartMachine(as.data.frame(X), y, num_trees = num_trees, mem_cache_for_speed = TRUE)
	t4 = Sys.time()
	destroy_bart_machine(bart_machine)
	our_time_1_m = t4 - t3
	cat("Our 1 core memcache time =", our_time_1_m, "\n")  
	time_mat[as.character(n), "Us1coremem"] = our_time_1_m
	  
	set_bart_machine_num_cores(1) ## 1 core
	t5 = Sys.time()
	bart_machine = bartMachine(as.data.frame(X), y, num_trees = num_trees, mem_cache_for_speed = FALSE)
	t6 = Sys.time()
	destroy_bart_machine(bart_machine)
	our_time_1 = t6 - t5
	cat("Our 1 core time =", our_time_1, "\n") 
	time_mat[as.character(n), "Us1core"] = our_time_1
	  
	set_bart_machine_num_cores(4) ## 4 core
	t7 = Sys.time()
	bart_machine = bartMachine(as.data.frame(X), y, num_trees = num_trees, mem_cache_for_speed = TRUE)
	t8 = Sys.time()
	destroy_bart_machine(bart_machine)
	our_time_4_m = t8 - t7
	cat("Our 4 core memcache time =", our_time_4_m, "\n")
	time_mat[as.character(n), "Us4coremem"] = our_time_4_m
	  
	set_bart_machine_num_cores(4) ## 4 core
	t9 = Sys.time()
	bart_machine = bartMachine(as.data.frame(X), y, num_trees = num_trees, mem_cache_for_speed = FALSE)
	t10 = Sys.time()
	destroy_bart_machine(bart_machine)
	our_time_4 = t10 - t9
	cat("Our 4 core time =", our_time_4, "\n")
	time_mat[as.character(n), "Us4core"] = our_time_4
	  
	set_bart_machine_num_cores(4) ## 4 core
	t11 = Sys.time()
	bart_machine = bartMachine(as.data.frame(X), y, num_trees = num_trees, mem_cache_for_speed = FALSE, run_in_sample = FALSE)
	t12 = Sys.time()
	destroy_bart_machine(bart_machine)
	our_time_4_no_run = t12 - t11
	cat("Our 4 core time with no run in sample =", our_time_4_no_run, "\n")
	time_mat[as.character(n), "Us4core_no_run"] = our_time_4_no_run  
	
	t13 = Sys.time()
	rf_mod = randomForest(as.data.frame(X), y)
	t14 = Sys.time()
	rf_time = t14 - t13
	cat("RF time =", rf_time, "\n")
	time_mat[as.character(n), "rf"] = rf_time  
	  
	counter = counter + 1
}


# convert some minutes to seconds
time_mat[8 : 9, ] = time_mat[8 : 9, ] * 60
time_mat[7, c(1,2,3,5,7)] = time_mat[7, c(1,2,3,5,7)] * 60
time_mat[6, 1] = time_mat[6, 1] * 60

#Figure 1a
COLORS = c("red", "darkblue", "darkblue", "darkviolet", "darkviolet", "darkgreen", "darkorange")
LTYS = c(1, 1, 2, 1, 2, 1, 1) 
NAMES = c("BayesTree", "bartMachine (1 core,\n memcache)", "bartMachine (1 core)", "bartMachine (4 cores,\n memcache)", "bartMachine (4 cores)", "bartMachine (4 cores,\n no in-sample)", "randomForest")
plot(nlist / 1000, time_mat[, 1] / 60, type = "o", col = COLORS[1], lty = LTYS[1], lwd = 3, xlab = "Sample Size (1000's)", ylab = "Minutes", ylim = c(0, 12))
for (j in 2 : 7){
	lines(nlist / 1000, time_mat[, j] / 60, type = "o", col = COLORS[j], lty = LTYS[j], lwd = 3)
}
legend(x = -2, y = 13, NAMES, COLORS, lty = LTYS)

#Figure 1b
plot(nlist, time_mat[, 1], type = "o", col = COLORS[1], lty = LTYS[1], lwd = 3, xlab = "Sample Size", ylab = "Seconds", ylim = c(0, 27), xlim = c(100, 2000))
for (j in 2 : 7){
	lines(nlist, time_mat[, j], type = "o", col = COLORS[j], lty = LTYS[j], lwd = 3)
}
legend(x = -2, y = 29, NAMES, COLORS, lty = LTYS)
