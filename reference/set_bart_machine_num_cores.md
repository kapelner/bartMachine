# Set the Number of Cores for BART

Sets the number of cores to be used for all parallelized BART functions.

## Usage

``` r
set_bart_machine_num_cores(num_cores, verbose = TRUE)
```

## Arguments

- num_cores:

  Number of cores to use. If the number of cores is more than 1, setting
  the seed during model construction cannot be deterministic.

- verbose:

  If TRUE, prints the updated core count.

## Value

None.

## See also

[`bart_machine_num_cores`](https://kapelner.github.io/bartMachine/reference/bart_machine_num_cores.md)

## Author

Adam Kapelner and Justin Bleich

## Examples

``` r
if (FALSE) { # \dontrun{
#set all parallelized functions to use 4 cores
set_bart_machine_num_cores(4)
} # }
```
