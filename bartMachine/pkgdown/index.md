# bartMachine <img src="man/figures/logo.png" align="right" height="139" alt="bartMachine hex logo" />

`bartMachine` is an R and Java implementation of Bayesian Additive Regression
Trees (BART) for regression and classification.

## Installation

Install the CRAN release with:

```r
install.packages("bartMachine")
```

Install the latest build from the kapelner R-universe with:

```r
install.packages(
  "bartMachine",
  repos = c(
    kapelner = "https://kapelner.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
  )
)
```

`bartMachine` 1.4 and later require Java 21 or newer. See the
[repository README](https://github.com/kapelner/bartMachine#setup-instructions)
for Java, memory, and optional GPU configuration.
