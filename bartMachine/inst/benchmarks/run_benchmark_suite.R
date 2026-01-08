#!/usr/bin/env Rscript

parse_args <- function(args) {
  opts <- list()
  for (arg in args) {
    if (arg == "--list") {
      opts$list <- TRUE
      next
    }
    if (arg == "--dry-run") {
      opts$dry_run <- TRUE
      next
    }
    if (arg == "--help") {
      opts$help <- TRUE
      next
    }
    if (grepl("^--", arg)) {
      key_val <- sub("^--", "", arg)
      parts <- strsplit(key_val, "=", fixed = TRUE)[[1]]
      key <- parts[1]
      val <- if (length(parts) > 1) parts[2] else ""
      opts[[key]] <- val
    }
  }
  opts
}

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  match <- grep(file_arg, args)
  if (length(match) == 0) {
    return(getwd())
  }
  file_path <- sub(file_arg, "", args[match[1]])
  dirname(normalizePath(file_path))
}

print_usage <- function() {
  cat("Usage: Rscript run_benchmark_suite.R [options]\n")
  cat("Options:\n")
  cat("  --list                    List available datasets and exit\n")
  cat("  --dry-run                 List selected datasets and exit\n")
  cat("  --output-dir=PATH         Output directory (default: inst/benchmarks/results)\n")
  cat("  --folds=N                 Number of folds (default: 5)\n")
  cat("  --repeats=N               Number of CV repeats (default: 1)\n")
  cat("  --seed=N                  Random seed (default: 123)\n")
  cat("  --datasets=A,B            Only run specific dataset names\n")
  cat("  --packages=A,B            Only run datasets from packages\n")
  cat("  --max-datasets=N          Limit the number of datasets\n")
  cat("  --skip-tags=A,B           Skip datasets with these tags\n")
  cat("  --cores=N                 bartMachine cores (default: 1)\n")
  cat("  --java-params=STRING      Set options(java.parameters)\n")
  cat("  --bart-num-trees=N        bartMachine num_trees\n")
  cat("  --bart-burn-in=N          bartMachine num_burn_in\n")
  cat("  --bart-iter=N             bartMachine num_iterations_after_burn_in\n")
  cat("  --rf-ntree=N              randomForest ntree\n")
  cat("  --rf-mtry=N               randomForest mtry\n")
}

as_int <- function(value, default) {
  if (is.null(value) || !nzchar(value)) {
    return(default)
  }
  as.integer(value)
}

as_char_vec <- function(value) {
  if (is.null(value) || !nzchar(value)) {
    return(NULL)
  }
  strsplit(value, ",", fixed = TRUE)[[1]]
}

script_dir <- get_script_dir()
registry_path <- file.path(script_dir, "benchmark_registry.R")
if (!file.exists(registry_path)) {
  stop("Cannot find benchmark registry at: ", registry_path)
}
invisible(source(registry_path, local = TRUE))

raw_opts <- parse_args(commandArgs(trailingOnly = TRUE))
if (isTRUE(raw_opts$help)) {
  print_usage()
  quit(status = 0)
}

opts <- list(
  output_dir = file.path("inst", "benchmarks", "results"),
  folds = 5,
  repeats = 1,
  seed = 123,
  cores = 1,
  datasets = NULL,
  packages = NULL,
  max_datasets = Inf,
  skip_tags = character(0),
  list = FALSE,
  dry_run = FALSE,
  java_params = Sys.getenv("BART_JAVA_PARAMS", ""),
  bart_num_trees = 50,
  bart_burn_in = 200,
  bart_iter = 200,
  rf_ntree = 500,
  rf_mtry = NA_real_
)

if (!is.null(raw_opts$output_dir)) {
  opts$output_dir <- raw_opts$output_dir
}
opts$folds <- as_int(raw_opts$folds, opts$folds)
opts$repeats <- as_int(raw_opts$repeats, opts$repeats)
opts$seed <- as_int(raw_opts$seed, opts$seed)
opts$cores <- as_int(raw_opts$cores, opts$cores)
opts$datasets <- as_char_vec(raw_opts$datasets)
opts$packages <- as_char_vec(raw_opts$packages)
opts$max_datasets <- as_int(raw_opts$`max-datasets`, opts$max_datasets)
opts$skip_tags <- as_char_vec(raw_opts$`skip-tags`)
opts$java_params <- if (!is.null(raw_opts$`java-params`)) raw_opts$`java-params` else opts$java_params
opts$bart_num_trees <- as_int(raw_opts$`bart-num-trees`, opts$bart_num_trees)
opts$bart_burn_in <- as_int(raw_opts$`bart-burn-in`, opts$bart_burn_in)
opts$bart_iter <- as_int(raw_opts$`bart-iter`, opts$bart_iter)
opts$rf_ntree <- as_int(raw_opts$`rf-ntree`, opts$rf_ntree)
if (!is.null(raw_opts$`rf-mtry`) && nzchar(raw_opts$`rf-mtry`)) {
  opts$rf_mtry <- as.numeric(raw_opts$`rf-mtry`)
}
opts$list <- isTRUE(raw_opts$list)
opts$dry_run <- isTRUE(raw_opts$dry_run)

print_registry <- function(registry) {
  entries <- vapply(
    registry,
    function(item) paste(item$package, item$name, item$task, sep = " | "),
    character(1)
  )
  cat(paste(entries, collapse = "\n"), "\n")
}

registry <- benchmark_registry
if (!is.null(opts$datasets)) {
  registry <- Filter(function(item) item$name %in% opts$datasets, registry)
}
if (!is.null(opts$packages)) {
  registry <- Filter(function(item) item$package %in% opts$packages, registry)
}
if (!is.null(opts$skip_tags) && length(opts$skip_tags) > 0) {
  registry <- Filter(
    function(item) {
      tags <- item$tags
      if (is.null(tags)) {
        return(TRUE)
      }
      !any(tags %in% opts$skip_tags)
    },
    registry
  )
}
if (is.finite(opts$max_datasets)) {
  registry <- head(registry, opts$max_datasets)
}

if (opts$list) {
  print_registry(benchmark_registry)
  quit(status = 0)
}
if (opts$dry_run) {
  print_registry(registry)
  quit(status = 0)
}

if (nzchar(opts$java_params)) {
  options(java.parameters = opts$java_params)
}

suppressPackageStartupMessages({
  if (!requireNamespace("bartMachine", quietly = TRUE)) {
    stop("bartMachine is not installed.")
  }
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("randomForest is not installed.")
  }
  library(bartMachine)
  library(randomForest)
})

set_bart_machine_num_cores(opts$cores, verbose = FALSE)

normalize_predictors <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col) || is.logical(col)) {
      return(as.factor(col))
    }
    if (inherits(col, "ordered")) {
      return(factor(col, ordered = FALSE))
    }
    col
  })
  df
}

drop_unused_factor_levels <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.factor(col)) {
      return(droplevels(col))
    }
    col
  })
  df
}

align_factor_levels <- function(train_df, test_df, test_y) {
  drop_idx <- rep(FALSE, nrow(test_df))
  for (col_name in names(train_df)) {
    train_col <- train_df[[col_name]]
    test_col <- test_df[[col_name]]
    if (is.factor(train_col)) {
      test_col <- factor(as.character(test_col), levels = levels(train_col))
      missing_level <- is.na(test_col) & !is.na(test_df[[col_name]])
      drop_idx <- drop_idx | missing_level
      test_df[[col_name]] <- test_col
    }
  }
  if (any(drop_idx)) {
    test_df <- test_df[!drop_idx, , drop = FALSE]
    test_y <- test_y[!drop_idx]
  }
  list(train = train_df, test = test_df, test_y = test_y, dropped = sum(drop_idx))
}

drop_high_cardinality_factors <- function(train_df, test_df, max_levels) {
  factor_cols <- names(train_df)[vapply(train_df, is.factor, logical(1))]
  if (length(factor_cols) == 0) {
    return(list(train = train_df, test = test_df, dropped = character(0)))
  }
  level_counts <- vapply(train_df[factor_cols], nlevels, integer(1))
  drop_cols <- factor_cols[level_counts > max_levels]
  if (length(drop_cols) > 0) {
    keep_idx <- !(names(train_df) %in% drop_cols)
    train_df <- train_df[, keep_idx, drop = FALSE]
    test_df <- test_df[, keep_idx, drop = FALSE]
  }
  list(train = train_df, test = test_df, dropped = drop_cols)
}

load_dataset <- function(def) {
  if (!requireNamespace(def$package, quietly = TRUE)) {
    return(list(ok = FALSE, reason = paste("Package missing:", def$package)))
  }
  env <- new.env(parent = emptyenv())
  data(list = def$name, package = def$package, envir = env)
  if (!exists(def$name, envir = env)) {
    return(list(ok = FALSE, reason = "Dataset not found"))
  }
  dat <- get(def$name, envir = env)
  if (is.matrix(dat)) {
    dat <- as.data.frame(dat)
  } else if (is.list(dat) && !is.data.frame(dat)) {
    if (!is.null(dat$x) && !is.null(dat$y)) {
      dat <- data.frame(dat$x, y = dat$y)
    } else {
      return(list(ok = FALSE, reason = "Dataset format not supported"))
    }
  }
  dat <- as.data.frame(dat)
  if (!is.null(def$drop)) {
    dat <- dat[, !(names(dat) %in% def$drop), drop = FALSE]
  }
  if (!(def$response %in% names(dat))) {
    return(list(ok = FALSE, reason = "Response column not found"))
  }
  dat <- dat[complete.cases(dat), , drop = FALSE]
  y <- dat[[def$response]]
  X <- dat[, setdiff(names(dat), def$response), drop = FALSE]
  X <- normalize_predictors(X)
  if (def$task == "classification") {
    if (is.logical(y) || is.numeric(y) || is.integer(y)) {
      y <- as.factor(y)
    } else if (is.character(y)) {
      y <- as.factor(y)
    }
    y <- droplevels(y)
    if (!is.factor(y) || length(levels(y)) != 2) {
      return(list(ok = FALSE, reason = "Classification target is not binary"))
    }
    # Enforce uniformity: first level becomes "1", second becomes "0"
    original_levels <- levels(y)
    y <- factor(ifelse(y == original_levels[1], "1", "0"), levels = c("1", "0"))
  } else {
    if (!is.numeric(y) && !is.integer(y)) {
      return(list(ok = FALSE, reason = "Regression target is not numeric"))
    }
    y <- as.numeric(y)
  }
  list(ok = TRUE, X = X, y = y, data = dat)
}

make_folds <- function(y, k, seed, stratify) {
  set.seed(seed)
  n <- length(y)
  fold_ids <- integer(n)
  if (stratify) {
    for (lvl in levels(y)) {
      idx <- which(y == lvl)
      idx <- sample(idx)
      fold_ids[idx] <- rep(seq_len(k), length.out = length(idx))
    }
  } else {
    fold_ids <- sample(rep(seq_len(k), length.out = n))
  }
  fold_ids
}

score_regression <- function(pred, y) {
  rmse <- sqrt(mean((pred - y) ^ 2))
  mae <- mean(abs(pred - y))
  denom <- sum((y - mean(y)) ^ 2)
  rsq <- if (denom > 0) 1 - sum((pred - y) ^ 2) / denom else NA_real_
  c(rmse = rmse, mae = mae, rsq = rsq)
}

score_classification <- function(pred, y, prob = NULL) {
  pred <- factor(pred, levels = levels(y))
  misclass <- mean(pred != y)
  accuracy <- mean(pred == y)
  c(misclass = misclass, accuracy = accuracy)
}

fold_rows <- list()
skip_rows <- list()

metrics_cols <- c("rmse", "mae", "rsq", "misclass", "accuracy")
progress_cols <- c(
  "dataset", "package", "task", "model", "repeat_id", "fold", "n", "n_train", "n_test",
  "p", "train_seconds", "predict_seconds", "rmse", "mae", "rsq", "misclass", "accuracy"
)
fold_id <- 1

invisible(dir.create(opts$output_dir, recursive = TRUE, showWarnings = FALSE))
progress_path <- file.path(opts$output_dir, "benchmark_progress.csv")
if (file.exists(progress_path)) {
  invisible(file.remove(progress_path))
}
append_progress_rows <- function(rows) {
  if (nrow(rows) == 0) {
    return()
  }
  if (!all(progress_cols %in% colnames(rows))) {
    stop("Progress rows missing expected columns.")
  }
  write.table(
    rows[, progress_cols, drop = FALSE],
    progress_path,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(progress_path),
    append = file.exists(progress_path),
    quote = TRUE
  )
}

create_summary_plots <- function(summary_df, fold_df, output_path) {
  if (nrow(summary_df) == 0 && nrow(fold_df) == 0) {
    return(invisible(NULL))
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("ggplot2 is required to generate benchmark plots.")
    return(invisible(NULL))
  }
  plot_list <- list()
  if (nrow(summary_df) > 0) {
    for (metric in unique(summary_df$metric)) {
      df <- subset(summary_df, metric == metric)
      if (nrow(df) == 0) {
        next
      }
      df$dataset <- factor(df$dataset, levels = unique(df$dataset[order(df$mean)]))
      p <- ggplot2::ggplot(df, ggplot2::aes(x = dataset, y = mean, fill = model)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
        ggplot2::scale_y_log10() +
        ggplot2::labs(
          title = metric,
          x = "Dataset",
          y = "Mean (log scale)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      plot_list[[length(plot_list) + 1]] <- p
    }
  }
  if (nrow(fold_df) > 0) {
    time_summary <- aggregate(cbind(train_seconds, predict_seconds) ~ dataset + model, data = fold_df, FUN = mean)
    if (nrow(time_summary) > 0) {
      time_long <- data.frame(
        dataset = rep(time_summary$dataset, times = 2),
        model = rep(time_summary$model, times = 2),
        stage = rep(c("train", "predict"), each = nrow(time_summary)),
        seconds = c(time_summary$train_seconds, time_summary$predict_seconds)
      )
      time_long$dataset <- factor(time_long$dataset, levels = unique(time_long$dataset))
      p <- ggplot2::ggplot(time_long, ggplot2::aes(x = dataset, y = seconds, fill = stage)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
        ggplot2::scale_y_log10() +
        ggplot2::facet_wrap(~model, scales = "free_y", ncol = 1) +
        ggplot2::labs(title = "Average Timing by Dataset", x = "Dataset", y = "Seconds (log scale)") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      plot_list[[length(plot_list) + 1]] <- p
    }
  }
  if (length(plot_list) == 0) {
    return(invisible(NULL))
  }
  pdf(output_path, width = 8, height = 6)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (plot_obj in plot_list) {
    print(plot_obj)
  }
  invisible(NULL)
}

for (dataset_index in seq_along(registry)) {
  def <- registry[[dataset_index]]
  message("Loading ", def$package, "::", def$name, "...")
  loaded <- load_dataset(def)
  if (!loaded$ok) {
    skip_rows[[length(skip_rows) + 1]] <- data.frame(
      dataset = def$name,
      package = def$package,
      reason = loaded$reason,
      stringsAsFactors = FALSE
    )
    message("  Skipping: ", loaded$reason)
    next
  }
  X <- loaded$X
  y <- loaded$y
  n <- nrow(X)
  p <- ncol(X)
  if (n < opts$folds) {
    skip_rows[[length(skip_rows) + 1]] <- data.frame(
      dataset = def$name,
      package = def$package,
      reason = "Not enough rows for folds",
      stringsAsFactors = FALSE
    )
    message("  Skipping: not enough rows for folds.")
    next
  }
  if (def$task == "classification") {
    class_counts <- table(y)
    if (min(class_counts) < 2) {
      skip_rows[[length(skip_rows) + 1]] <- data.frame(
        dataset = def$name,
        package = def$package,
        reason = "Insufficient class counts",
        stringsAsFactors = FALSE
      )
      message("  Skipping: insufficient class counts.")
      next
    }
  }

  for (rep_idx in seq_len(opts$repeats)) {
    fold_seed <- opts$seed + dataset_index * 1000 + rep_idx * 100
    fold_ids <- make_folds(y, opts$folds, fold_seed, def$task == "classification")

    for (fold in seq_len(opts$folds)) {
      test_idx <- which(fold_ids == fold)
      train_idx <- which(fold_ids != fold)
      train_X <- X[train_idx, , drop = FALSE]
      test_X <- X[test_idx, , drop = FALSE]
      train_y <- y[train_idx]
      test_y <- y[test_idx]

      train_X <- drop_unused_factor_levels(train_X)

      if (def$task == "classification") {
        train_y <- droplevels(train_y)
        if (length(levels(train_y)) < 2) {
          message("  Fold skipped: training data has one class only.")
          next
        }
        test_y <- factor(test_y, levels = levels(train_y))
        if (any(is.na(test_y))) {
          message("  Fold skipped: test data has unseen response levels.")
          next
        }
      }

      aligned <- align_factor_levels(train_X, test_X, test_y)
      train_X <- aligned$train
      test_X <- aligned$test
      test_y <- aligned$test_y
      if (aligned$dropped > 0) {
        message("  Dropped ", aligned$dropped, " test rows with unseen factor levels.")
      }
      if (nrow(test_X) == 0) {
        message("  Fold skipped: no test rows after factor alignment.")
        next
      }

      dropped_factors <- drop_high_cardinality_factors(train_X, test_X, 53L)
      train_X <- dropped_factors$train
      test_X <- dropped_factors$test
      if (length(dropped_factors$dropped) > 0) {
        message(
          "  Dropped high-cardinality factors for randomForest compatibility: ",
          paste(dropped_factors$dropped, collapse = ", ")
        )
      }
      if (ncol(train_X) == 0) {
        message("  Fold skipped: no predictors after dropping high-cardinality factors.")
        next
      }

      n_train <- nrow(train_X)
      n_test <- nrow(test_X)
      fold_p <- ncol(train_X)

      fold_seed <- fold_seed + fold
      bart_params <- list(
        num_trees = opts$bart_num_trees,
        num_burn_in = opts$bart_burn_in,
        num_iterations_after_burn_in = opts$bart_iter,
        verbose = FALSE
      )
      rf_params <- list(
        ntree = opts$rf_ntree
      )
      if (!is.na(opts$rf_mtry)) {
        rf_params$mtry <- opts$rf_mtry
      }

      set.seed(fold_seed)
      bart_time <- system.time({
        bart_fit <- do.call(bartMachine, c(list(train_X, train_y), bart_params))
      })["elapsed"]
      set.seed(fold_seed)
      rf_time <- system.time({
        rf_fit <- do.call(randomForest, c(list(train_X, train_y), rf_params))
      })["elapsed"]

      if (def$task == "classification") {
        bart_pred_time <- system.time({
          bart_prob <- predict(bart_fit, test_X, type = "prob", verbose = FALSE)
        })["elapsed"]
        bart_pred <- ifelse(bart_prob >= 0.5, levels(train_y)[1], levels(train_y)[2])
        bart_pred <- factor(bart_pred, levels = levels(train_y))
        bart_metrics <- score_classification(bart_pred, test_y, bart_prob)

        rf_pred_time <- system.time({
          rf_prob <- predict(rf_fit, test_X, type = "prob")
        })["elapsed"]
        rf_prob <- rf_prob[, levels(train_y)[1]]
        rf_pred <- predict(rf_fit, test_X, type = "response")
        rf_metrics <- score_classification(rf_pred, test_y, rf_prob)
      } else {
        bart_pred_time <- system.time({
          bart_pred <- predict(bart_fit, test_X, verbose = FALSE)
        })["elapsed"]
        bart_metrics <- score_regression(bart_pred, test_y)

        rf_pred_time <- system.time({
          rf_pred <- predict(rf_fit, test_X)
        })["elapsed"]
        rf_metrics <- score_regression(rf_pred, test_y)
      }

      bart_row <- data.frame(
        dataset = def$name,
        package = def$package,
        task = def$task,
        model = "bartMachine",
        repeat_id = rep_idx,
        fold = fold,
        n = n,
        n_train = n_train,
        n_test = n_test,
        p = fold_p,
        train_seconds = as.numeric(bart_time),
        predict_seconds = as.numeric(bart_pred_time),
        rmse = ifelse("rmse" %in% names(bart_metrics), bart_metrics["rmse"], NA_real_),
        mae = ifelse("mae" %in% names(bart_metrics), bart_metrics["mae"], NA_real_),
        rsq = ifelse("rsq" %in% names(bart_metrics), bart_metrics["rsq"], NA_real_),
        misclass = ifelse("misclass" %in% names(bart_metrics), bart_metrics["misclass"], NA_real_),
        accuracy = ifelse("accuracy" %in% names(bart_metrics), bart_metrics["accuracy"], NA_real_),
        stringsAsFactors = FALSE
      )
      fold_rows[[fold_id]] <- bart_row
      append_progress_rows(bart_row)
      fold_id <- fold_id + 1

      rf_row <- data.frame(
        dataset = def$name,
        package = def$package,
        task = def$task,
        model = "randomForest",
        repeat_id = rep_idx,
        fold = fold,
        n = n,
        n_train = n_train,
        n_test = n_test,
        p = fold_p,
        train_seconds = as.numeric(rf_time),
        predict_seconds = as.numeric(rf_pred_time),
        rmse = ifelse("rmse" %in% names(rf_metrics), rf_metrics["rmse"], NA_real_),
        mae = ifelse("mae" %in% names(rf_metrics), rf_metrics["mae"], NA_real_),
        rsq = ifelse("rsq" %in% names(rf_metrics), rf_metrics["rsq"], NA_real_),
        misclass = ifelse("misclass" %in% names(rf_metrics), rf_metrics["misclass"], NA_real_),
        accuracy = ifelse("accuracy" %in% names(rf_metrics), rf_metrics["accuracy"], NA_real_),
        stringsAsFactors = FALSE
      )
      fold_rows[[fold_id]] <- rf_row
      append_progress_rows(rf_row)
      fold_id <- fold_id + 1
    }
  }
}

fold_results <- if (length(fold_rows) == 0) {
  data.frame()
} else {
  do.call(rbind, fold_rows)
}

summary_results <- data.frame()
if (nrow(fold_results) > 0) {
  summary_rows <- list()
  summary_id <- 1
  split_key <- paste(fold_results$dataset, fold_results$model, fold_results$task)
  for (key in unique(split_key)) {
    subset_idx <- which(split_key == key)
    subset_df <- fold_results[subset_idx, , drop = FALSE]
    for (metric in metrics_cols) {
      metric_vals <- subset_df[[metric]]
      if (all(is.na(metric_vals))) {
        next
      }
      summary_rows[[summary_id]] <- data.frame(
        dataset = subset_df$dataset[1],
        package = subset_df$package[1],
        task = subset_df$task[1],
        model = subset_df$model[1],
        metric = metric,
        mean = mean(metric_vals, na.rm = TRUE),
        sd = sd(metric_vals, na.rm = TRUE),
        n = sum(!is.na(metric_vals)),
        stringsAsFactors = FALSE
      )
      summary_id <- summary_id + 1
    }
  }
  if (length(summary_rows) > 0) {
    summary_results <- do.call(rbind, summary_rows)
  }
}

comparison_results <- data.frame()
if (nrow(fold_results) > 0 && "bartMachine" %in% fold_results$model && "randomForest" %in% fold_results$model) {
  comp_rows <- list()
  comp_id <- 1
  # Datasets that have both models
  datasets <- unique(fold_results$dataset)
  
  for (ds in datasets) {
    ds_df <- fold_results[fold_results$dataset == ds, , drop = FALSE]
    # Check if we have both models for this dataset
    if (!all(c("bartMachine", "randomForest") %in% ds_df$model)) {
      next
    }
    
    for (metric in metrics_cols) {
      # Extract vectors for each model
      # Ensure sorting by repeat and fold to align pairs
      bart_df <- ds_df[ds_df$model == "bartMachine", , drop = FALSE]
      rf_df <- ds_df[ds_df$model == "randomForest", , drop = FALSE]
      
      # Merge to ensure we only compare valid pairs
      # Using repeat_id and fold as keys
      merged <- merge(
        bart_df[, c("repeat_id", "fold", metric)], 
        rf_df[, c("repeat_id", "fold", metric)], 
        by = c("repeat_id", "fold"), 
        suffixes = c("_bart", "_rf")
      )
      
      val_bart <- merged[[paste0(metric, "_bart")]]
      val_rf <- merged[[paste0(metric, "_rf")]]
      
      # Filter NAs
      valid_idx <- !is.na(val_bart) & !is.na(val_rf)
      val_bart <- val_bart[valid_idx]
      val_rf <- val_rf[valid_idx]
      
      if (length(val_bart) < 2) {
        next
      }
      
      # T-test
      # We want to test difference
      # Default: two.sided, paired=TRUE
      test_res <- tryCatch(
        t.test(val_bart, val_rf, paired = TRUE, alternative = "two.sided"),
        error = function(e) NULL
      )
      
      pval <- if (!is.null(test_res)) test_res$p.value else NA_real_
      
      comp_rows[[comp_id]] <- data.frame(
        dataset = ds,
        package = ds_df$package[1],
        task = ds_df$task[1],
        metric = metric,
        bart_mean = mean(val_bart),
        rf_mean = mean(val_rf),
        diff_mean = mean(val_bart - val_rf),
        p_value = pval,
        n = length(val_bart),
        stringsAsFactors = FALSE
      )
      comp_id <- comp_id + 1
    }
  }
  
  if (length(comp_rows) > 0) {
    comparison_results <- do.call(rbind, comp_rows)
  }
}

skip_results <- if (length(skip_rows) == 0) {
  data.frame()
} else {
  do.call(rbind, skip_rows)
}

fold_path <- file.path(opts$output_dir, "benchmark_folds.csv")
summary_path <- file.path(opts$output_dir, "benchmark_summary.csv")
comparison_path <- file.path(opts$output_dir, "benchmark_comparisons.csv")
skip_path <- file.path(opts$output_dir, "benchmark_skipped.csv")
rds_path <- file.path(opts$output_dir, "benchmark_results.rds")
session_path <- file.path(opts$output_dir, "sessionInfo.txt")

if (nrow(fold_results) > 0) {
  write.csv(fold_results, fold_path, row.names = FALSE)
}
if (nrow(summary_results) > 0) {
  write.csv(summary_results, summary_path, row.names = FALSE)
}
if (nrow(comparison_results) > 0) {
  write.csv(comparison_results, comparison_path, row.names = FALSE)
}
if (nrow(skip_results) > 0) {
  write.csv(skip_results, skip_path, row.names = FALSE)
}
saveRDS(
  list(
    config = opts,
    registry_version = benchmark_registry_version,
    summary = summary_results,
    comparison = comparison_results,
    folds = fold_results,
    skipped = skip_results
  ),
  rds_path
)
capture.output(sessionInfo(), file = session_path)

plot_path <- file.path(opts$output_dir, "benchmark_summary_plots.pdf")
create_summary_plots(summary_results, fold_results, plot_path)

message("Benchmark complete.")
message("Summary: ", summary_path)
