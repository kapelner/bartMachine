# Gets Raw Node data

Returns a list object that contains all the information for all trees in
a given Gibbs sample. Daughter nodes are nested in the list structure
recursively.

## Usage

``` r
extract_raw_node_data(bart_machine, g = 1)
```

## Arguments

- bart_machine:

  An object of class “bartMachine”.

- g:

  The gibbs sample number. It must be a natural number between 1 and the
  number of iterations after burn in. Default is 1.

## Value

Returns a list object that contains all the information for all trees in
a given Gibbs sample.

## Examples

``` r
if (FALSE) { # \dontrun{
options(java.parameters = c("-Xmx20g", "--add-modules=jdk.incubator.vector", "-XX:+UseZGC"))
pacman::p_load(bartMachine)

seed = 1984
set.seed(seed)
n = 100
x = rnorm(n, 0, 1)
sigma = 0.1
y = x + rnorm(n, 0, sigma)

num_trees = 200
num_iterations_after_burn_in = 1000
bart_mod = bartMachine(data.frame(x = x), y,
  flush_indices_to_save_RAM = FALSE,
  num_trees = num_trees,
  num_iterations_after_burn_in = num_iterations_after_burn_in,
  seed = seed)

raw_node_data = extract_raw_node_data(bart_mod)

} # }
```
