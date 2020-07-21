\name{interaction_investigator}
\alias{interaction_investigator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Explore Pairwise Interactions in BART Model
}
\description{
Explore the pairwise interaction counts for a BART model to learn about interactions fit by the model. This function includes an option to generate a plot of the pairwise interaction counts.
}
\usage{
interaction_investigator(bart_machine, plot = TRUE, 
num_replicates_for_avg = 5, num_trees_bottleneck = 20, 
num_var_plot = 50, cut_bottom = NULL, bottom_margin = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
An object of class ``bartMachine''.
}
  \item{plot}{
If TRUE, a plot of the pairwise interaction counts is generated. 
}
  \item{num_replicates_for_avg}{
The number of replicates of BART to be used to generate pairwise interaction inclusion counts. 
Averaging across multiple BART models improves stability of the estimates.
}
  \item{num_trees_bottleneck}{
Number of trees to be used in the sum-of-trees model for computing pairwise interactions counts. 
A small number of trees should be used to force the variables to compete for entry into the model.
}
  \item{num_var_plot}{
Number of variables to be shown on the plot. If ``Inf,'' all variables are plotted (not recommended if
the number of predictors is large). Default is 50.
}
  \item{cut_bottom}{
A display parameter between 0 and 1 that controls where the y-axis is plotted. A value of 0 would begin the y-axis at 0; a value of 1 begins 
the y-axis at the minimum of the average pairwise interaction inclusion count (the smallest bar in the bar plot). Values between 0 and 1 begin the 
y-axis as a percentage of that minimum. 
}
  \item{bottom_margin}{
A display parameter that adjusts the bottom margin of the graph if labels are clipped. The scale of this parameter is the same as set with \code{par(mar = c(....))} in R.
Higher values allow for more space if the crossed covariate names are long. Note that making this parameter too large will prevent plotting and the plot function in R will throw an error.
}
}
\details{
An interaction between two variables is considered to occur whenever a path from any node of a tree to 
any of its terminal node contains splits using those two variables. See Kapelner and Bleich, 2013, Section 4.11.
}
\value{
	\item{interaction_counts_avg}{For each of the \eqn{p \times p}{p times p} interactions, what is the average count across all \code{num_replicates_for_avg}
BART model replicates' post burn-in Gibbs samples in all trees.}
	\item{interaction_counts_sd}{For each of the \eqn{p \times p}{p times p} interactions, what is the average sd of the interaction counts across the \code{num_replicates_for_avg}
BART models replicates.}
}
\references{
Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning
with Bayesian Additive Regression Trees. Journal of Statistical
Software, 70(4), 1-40. doi:10.18637/jss.v070.i04
}
\author{
Adam Kapelner and Justin Bleich
}
\note{
In the plot, the red bars correspond to the standard error of the variable inclusion proportion estimates (since multiple replicates were used).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{investigate_var_importance}}
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
bart_machine = bartMachine(X, y, num_trees = 20)

#investigate interactions
interaction_investigator(bart_machine)
}

}
