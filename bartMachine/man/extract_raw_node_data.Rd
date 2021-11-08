\name{extract_raw_node_data}
\alias{extract_raw_node_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets Raw Node data
}
\description{
Returns a list object that contains all the information for all trees in a given Gibbs sample. Daughter nodes are nested
in the list structure recursively.
}
\usage{
extract_raw_node_data(bart_machine, g = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
An object of class ``bartMachine''.
}
  \item{g}{
The gibbs sample number. It must be a natural number between 1 and the number of iterations after burn in. Default is 1.
}
}

\value{
Returns a list object that contains all the information for all trees in a given Gibbs sample.
}

\examples{
\dontrun{
options(java.parameters = "-Xmx10g")
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

}
}
