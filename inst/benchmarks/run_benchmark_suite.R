#!/usr/bin/env Rscript

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

script_dir <- get_script_dir()
root_dir <- normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE)
target_script <- file.path(root_dir, "bartMachine", "inst", "benchmarks", "run_benchmark_suite.R")

if (!file.exists(target_script)) {
  stop("Cannot find benchmark suite at: ", target_script)
}

args <- commandArgs(trailingOnly = TRUE)
status <- system2("Rscript", c(target_script, args))
quit(status = status)
