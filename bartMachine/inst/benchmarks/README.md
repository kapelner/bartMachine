# Benchmark Suite: bartMachine vs randomForest

This benchmark suite runs repeated K-fold cross-validation and compares
out-of-sample performance between `bartMachine` and `randomForest` across
datasets drawn from standard benchmark libraries (e.g., `datasets`, `MASS`,
`mlbench`, `ISLR`).

## Quick start

1. Install the package and optional dataset libraries:

```r
install.packages(c("bartMachine", "randomForest", "mlbench", "ISLR", "pROC"))
```

2. Run the suite from the package root:

```bash
Rscript inst/benchmarks/run_benchmark_suite.R --folds=5 --repeats=3
```

3. Outputs are written to `inst/benchmarks/results`:
   - `benchmark_folds.csv`: per-fold metrics
   - `benchmark_summary.csv`: mean and standard deviation by dataset/model
   - `benchmark_skipped.csv`: datasets that were skipped and why
   - `benchmark_results.rds`: all results + config
   - `sessionInfo.txt`: session metadata

## Notes

- Set Java memory and other parameters *before* loading `bartMachine`. You can do this via:
  - `options(java.parameters = c("-Xmx20g", "--add-modules=jdk.incubator.vector", "-XX:+UseZGC"))`
- Use `--list` to view all dataset definitions and `--dry-run` to list the
  filtered selection without running.
- Large or slow datasets can be skipped with `--skip-tags=large`.
- `pROC` is optional; if installed, AUC is computed for classification tasks.

## Example filters

```bash
Rscript inst/benchmarks/run_benchmark_suite.R --packages=datasets,MASS --folds=3
Rscript inst/benchmarks/run_benchmark_suite.R --datasets=Boston,BostonHousing
Rscript inst/benchmarks/run_benchmark_suite.R --skip-tags=large --repeats=2
```
