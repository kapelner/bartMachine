\name{investigate_var_importance}
\alias{investigate_var_importance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Explore Variable Inclusion Proportions in BART Model
}
\description{
Explore the variable inclusion proportions for a BART model to learn about the relative influence of the different covariates. This function includes an option to generate a plot of the variable inclusion proportions.
}
\usage{
investigate_var_importance(bart_machine, type = "splits", 
plot = TRUE, num_replicates_for_avg = 5, num_trees_bottleneck = 20, 
num_var_plot = Inf, bottom_margin = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
An object of class ``bartMachine''.
}
  \item{type}{
If ``splits'', then the proportion of times each variable is chosen for a splitting rule is computed. If ``trees'', then the proportion of times each variable appears in a tree is computed.
}
  \item{plot}{
If TRUE, a plot of the variable inclusion proportions is generated. 
}
  \item{num_replicates_for_avg}{
The number of replicates of BART to be used to generate variable inclusion proportions. Averaging across multiple BART models improves stability of the estimates. See Bleich et al. (2013) for more details. 
}
  \item{num_trees_bottleneck}{
Number of trees to be used in the sum-of-trees for computing the variable inclusion proportions. A small number of trees should be used to force the variables to compete for entry into the model. Chipman et al. (2010) recommend 20. See this reference for more details. 
}
  \item{num_var_plot}{
Number of variables to be shown on the plot. If ``Inf'', all variables are plotted.
}
  \item{bottom_margin}{
A display parameter that adjusts the bottom margin of the graph if labels are clipped. The scale of this parameter is the same as set with \code{par(mar = c(....))} in R.
Higher values allow for more space if the covariate names are long. Note that making this parameter too large will prevent plotting and the plot function in R will throw an error.
}
}
\details{
In the plot, the red bars correspond to the standard error of the variable inclusion proportion estimates.
}
\value{
Invisibly, returns a list with the following components:
\item{avg_var_props}{The average variable inclusion proportions for each variable\cr (across \code{num_replicates_for_avg})}
\item{sd_var_props}{The standard deviation of the variable inclusion proportions for each variable (across \code{num_replicates_for_avg})}

}
\references{
J Bleich, A Kapelner, ST Jensen, and EI George. Variable Selection Inference for Bayesian
Additive Regression Trees. ArXiv e-prints, 2013.

HA Chipman, EI George, and RE McCulloch. BART: Bayesian Additive Regressive Trees.
The Annals of Applied Statistics, 4(1): 266--298, 2010.
}
\author{
Adam Kapelner and Justin Bleich
}
\note{
This function is parallelized by the number of cores set in \code{\link{set_bart_machine_num_cores}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{interaction_investigator}}
}
\examples{
\dontrun{
#generate Friedman data
set.seed(11)
n  = 200 
p = 10
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = build_bart_machine(X, y, num_trees = 20)

#investigate variable inclusion proportions
investigate_var_importance(bart_machine)

#destroy BART model
destroy_bart_machine(bart_machine)
}

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
