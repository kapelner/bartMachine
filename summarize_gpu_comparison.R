old = readRDS("res_old.rds")
gpu = readRDS("res_gpu.rds")

report = function(name, o, g) {
  diff = max(abs(o$val - g$val))
  cat(sprintf("%-20s | Diff: %e | Time v1.4.1.1: %.3fs | Time v1.4.2 (GPU): %.3fs | Speedup: %.2fx\n", 
              name, diff, o$time, g$time, o$time / g$time))
}

cat("\n--- Numerical Parity and Performance: v1.4.2 (GPU) vs v1.4.1.1 (Baseline) ---\n")
report("Reg Mean", old$reg_mean, gpu$reg_mean)
report("Reg Samples", old$reg_samples, gpu$reg_samples)
report("Reg Cred Int", old$reg_cred, gpu$reg_cred)
report("Class Probs", old$class_probs, gpu$class_probs)
report("Class Samples", old$class_samples, gpu$class_samples)
cat("\nNote: GPU was enabled for v1.4.2. If timings are similar, the system fell back to CPU.\n")
