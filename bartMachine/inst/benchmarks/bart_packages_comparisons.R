#!/usr/bin/env Rscript

# bartMachine vs BART vs dbarts benchmark
# Accuracy and Speed comparison

options(java.parameters = c("-Xmx20g", "--add-modules=jdk.incubator.vector"))

suppressPackageStartupMessages({
  library(bartMachine)
  library(BART)
  library(dbarts)
  library(mlbench)
  library(ggplot2)
})

# Benchmark Configuration
args <- commandArgs(trailingOnly = TRUE)

# Default values
N_REPS <- 5
N_SAMPLES <- 500

# Parse arguments
for (arg in args) {
  if (grepl("^--reps=", arg)) {
    N_REPS <- as.integer(sub("--reps=", "", arg))
  } else if (grepl("^--n=", arg)) {
    N_SAMPLES <- as.integer(sub("--n=", "", arg))
  }
}

N_TREES <- 200
N_BURN <- 500
N_POST <- 1000
CORES <- 4

set_bart_machine_num_cores(CORES)

# Datasets to benchmark
# We'll use some standard ones from mlbench and Friedman data
generate_friedman <- function(n, p = 10, sigma = 1.0) {
  X <- matrix(runif(n * p), nrow = n)
  y_mean <- 10 * sin(pi * X[, 1] * X[, 2]) + 20 * (X[, 3] - 0.5)^2 + 10 * X[, 4] + 5 * X[, 5]
  y <- y_mean + rnorm(n, 0, sigma)
  data.frame(X, y = y)
}

generate_friedman_class <- function(n, p = 10) {
  X <- matrix(runif(n * p), nrow = n)
  logit <- 1.5 * sin(pi * X[, 1] * X[, 2]) - 2 * (X[, 3] - 0.5) + 3 * X[, 4] * X[, 5] + 2 * (X[, 6] - 0.5)^2
  prob <- 1 / (1 + exp(-logit))
  y <- factor(rbinom(n, 1, prob), levels = c(0, 1), labels = c("c0", "c1"))
  data.frame(X, y = y)
}

run_benchmark <- function(data, task = "regression", dataset_name = "data") {
  results <- list()
  
  n <- nrow(data)
  train_idx <- sample(1:n, round(0.8 * n))
  train_data <- data[train_idx, ]
  test_data <- data[-train_idx, ]
  
  X_train <- train_data[, setdiff(names(train_data), "y")]
  y_train <- train_data$y
  X_test <- test_data[, setdiff(names(test_data), "y")]
  y_test <- test_data$y
  
  # For BART and dbarts, we need matrix input and dummies for factors
  # Ensure consistent columns by processing train and test together
  all_data <- rbind(train_data, test_data)
  X_all_mat <- model.matrix(y ~ . - 1, data = all_data)
  X_train_mat <- X_all_mat[1:nrow(train_data), , drop = FALSE]
  X_test_mat <- X_all_mat[(nrow(train_data) + 1):nrow(all_data), , drop = FALSE]
  
  if (task == "classification") {
    y_train_num <- as.numeric(y_train) - 1 # 0, 1
    y_test_num <- as.numeric(y_test) - 1
  }

  models <- c("bartMachine", "BART", "dbarts")
  
  for (m in models) {
    message(sprintf("Running %s on %s...", m, dataset_name))
    
    start_time <- Sys.time()
    
    if (m == "bartMachine") {
      fit <- bartMachine(X_train, y_train, num_trees = N_TREES, num_burn_in = N_BURN, 
                         num_iterations_after_burn_in = N_POST, verbose = FALSE, seed = 1,
                         use_xoshiro = TRUE)
      train_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      
      start_pred <- Sys.time()
      if (task == "regression") {
        pred <- predict(fit, X_test)
        acc <- sqrt(mean((pred - y_test)^2)) # RMSE
      } else {
        pred <- predict(fit, X_test, type = "class")
        acc <- mean(pred != y_test) # Misclassification
      }
      pred_time <- as.numeric(difftime(Sys.time(), start_pred, units = "secs"))
      
    } else if (m == "BART") {
      if (task == "regression") {
        fit <- try(mc.wbart(X_train_mat, y_train, X_test_mat, ntree = N_TREES, nskip = N_BURN, ndpost = N_POST, 
                        mc.cores = as.integer(CORES), seed = 1, printevery = 10000L))
        if (inherits(fit, "try-error") || (!inherits(fit, "wbart") && is.list(fit))) {
           stop(sprintf("BART::mc.wbart failed on %s. Check for worker crashes or column inconsistencies.", dataset_name))
        }
        train_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        pred_time <- 0 
        pred <- fit$yhat.test.mean
        acc <- sqrt(mean((pred - y_test)^2))
      } else {
        fit <- try(mc.pbart(X_train_mat, y_train_num, X_test_mat, ntree = N_TREES, nskip = N_BURN, ndpost = N_POST, 
                        mc.cores = as.integer(CORES), seed = 1, printevery = 10000L))
        if (inherits(fit, "try-error") || (!inherits(fit, "pbart") && is.list(fit))) {
           stop(sprintf("BART::mc.pbart failed on %s. Check for worker crashes or column inconsistencies.", dataset_name))
        }
        train_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        pred_time <- 0
        prob <- fit$prob.test.mean
        pred <- factor(ifelse(prob > 0.5, levels(y_test)[2], levels(y_test)[1]), levels = levels(y_test))
        acc <- mean(pred != y_test)
      }
      
    } else if (m == "dbarts") {
      if (task == "regression") {
        fit <- dbarts::bart(X_train_mat, y_train, X_test_mat, ntree = N_TREES, nskip = N_BURN, ndpost = N_POST, 
                            nthread = as.integer(CORES), verbose = FALSE, seed = 1)
        train_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        pred_time <- 0
        pred <- fit$yhat.test.mean
        acc <- sqrt(mean((pred - y_test)^2))
      } else {
        fit <- dbarts::bart(X_train_mat, y_train_num, X_test_mat, ntree = N_TREES, nskip = N_BURN, ndpost = N_POST, 
                            nthread = as.integer(CORES), verbose = FALSE, seed = 1)
        train_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        pred_time <- 0
        prob <- colMeans(pnorm(fit$yhat.test))
        pred <- factor(ifelse(prob > 0.5, levels(y_test)[2], levels(y_test)[1]), levels = levels(y_test))
        acc <- mean(pred != y_test)
      }
    }
    
    results[[m]] <- data.frame(
      dataset = dataset_name,
      model = m,
      task = task,
      train_time = train_time,
      pred_time = pred_time,
      total_time = train_time + pred_time,
      accuracy = acc
    )
  }
  
  do.call(rbind, results)
}

# Run on several datasets
datasets <- list(
  list(name = "Friedman_Reg", task = "regression", data = generate_friedman(N_SAMPLES)),
  list(name = "Friedman_Class", task = "classification", data = generate_friedman_class(N_SAMPLES)),
  list(name = "BostonHousing", task = "regression", data = {
    data(BostonHousing)
    df <- BostonHousing
    names(df)[names(df) == "medv"] <- "y"
    df
  }),
  list(name = "PimaIndiansDiabetes", task = "classification", data = {
    data(PimaIndiansDiabetes)
    df <- PimaIndiansDiabetes
    names(df)[names(df) == "diabetes"] <- "y"
    df
  })
)

all_results <- list()
for (i in 1:N_REPS) {
  message(sprintf("\n--- Replicate %d ---", i))
  for (ds in datasets) {
    res <- run_benchmark(ds$data, ds$task, ds$name)
    res$replicate <- i
    all_results[[length(all_results) + 1]] <- res
  }
}

final_df <- do.call(rbind, all_results)

# Summary
summary_list <- list()
for (ds_name in unique(final_df$dataset)) {
  for (mdl_name in unique(final_df$model)) {
    sub_df <- final_df[final_df$dataset == ds_name & final_df$model == mdl_name, ]
    if (nrow(sub_df) == 0) next
    
    summary_list[[length(summary_list) + 1]] <- data.frame(
      dataset = ds_name,
      model = mdl_name,
      task = sub_df$task[1],
      train_time_mean = mean(sub_df$train_time),
      train_time_sd = if(nrow(sub_df) > 1) sd(sub_df$train_time) else NA_real_,
      pred_time_mean = mean(sub_df$pred_time),
      pred_time_sd = if(nrow(sub_df) > 1) sd(sub_df$pred_time) else NA_real_,
      total_time_mean = mean(sub_df$total_time),
      total_time_sd = if(nrow(sub_df) > 1) sd(sub_df$total_time) else NA_real_,
      accuracy_mean = mean(sub_df$accuracy),
      accuracy_sd = if(nrow(sub_df) > 1) sd(sub_df$accuracy) else NA_real_,
      n = nrow(sub_df)
    )
  }
}

summary_flat <- do.call(rbind, summary_list)

print(summary_flat)

write.csv(summary_flat, "bartMachine/inst/benchmarks/bart_packages_comparisons_results/summary.csv", row.names = FALSE)
write.csv(final_df, "bartMachine/inst/benchmarks/bart_packages_comparisons_results/raw_results.csv", row.names = FALSE)

# Plotting
pdf("bartMachine/inst/benchmarks/bart_packages_comparisons_results/plots.pdf", width = 10, height = 8)

# Time Plot
p1 <- ggplot(summary_flat, aes(x = model, y = total_time_mean, fill = model)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = total_time_mean - total_time_sd, ymax = total_time_mean + total_time_sd), width = 0.2) +
  facet_wrap(~dataset, scales = "free_y") +
  theme_minimal() +
  labs(title = "Total Time Comparison (Training + Prediction)", y = "Time (seconds)")

print(p1)

# Accuracy Plot
p2 <- ggplot(summary_flat, aes(x = model, y = accuracy_mean, fill = model)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = accuracy_mean - accuracy_sd, ymax = accuracy_mean + accuracy_sd), width = 0.2) +
  facet_wrap(~dataset, scales = "free_y") +
  theme_minimal() +
  labs(title = "Accuracy Comparison (RMSE for Reg, Misclass for Class)", y = "Error (Lower is better)")

print(p2)

dev.off()

message("\nBenchmark complete. Results saved in bartMachine/inst/benchmarks/bart_packages_comparisons_results/")
