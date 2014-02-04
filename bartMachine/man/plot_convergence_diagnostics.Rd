\name{plot_convergence_diagnostics}
\alias{plot_convergence_diagnostics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Convergence Diagnostics
}
\description{
A suite of plots to assess convergence diagonstics and features of the BART model. 
}
\usage{
plot_convergence_diagnostics(bart_machine,  
plots = c("sigsqs", "mh_acceptance", "num_nodes", "tree_depths"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
An object of class ``bartMachine''.
}
\item{plots}{
The list of plots to be displayed. The four options are: "sigsqs", "mh_acceptance", "num_nodes", "tree_depths".
}
}
\details{
The ``sigsqs'' option plots the posterior error variance estimates by the Gibbs sample number. This is a standard tool to assess convergence of MCMC algorithms. This option is not applicable to classification BART models.\cr
The ``mh_acceptance'' option plots the proportion of Metropolis-Hastings steps accepted for each Gibbs sample (number accepted divided by number of trees).\cr
The ``num_nodes'' option plots the average number of nodes across each tree in the sum-of-trees model by the Gibbs sample number (for post burn-in only). The blue line
is the average number of nodes over all trees.\cr
The ``tree_depths'' option plots the average tree depth across each tree in the sum-of-trees model by the Gibbs sample number (for post burn-in only). The blue line
is the average number of nodes over all trees.
}
\value{
None.
}

\author{
Adam Kapelner and Justin Bleich
}
\note{
The ``sigsqs'' plot separates the burn-in \eqn{\sigma^2}'s for the first core by post burn-in \eqn{\sigma^2}'s estimates for all cores by grey vertical lines.
The ``mh_acceptance'' plot separates burn-in from post-burn in by a grey vertical line. Post burn-in, the different core proportions plot in different colors.
The ``num_nodes'' plot separates different core estimates by vertical lines (post burn-in only).
The `tree_depths'' plot separates different core estimates by vertical lines (post burn-in only).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
\dontrun{
#generate Friedman data
set.seed(11)
n  = 200 
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = bartMachine(X, y)

#plot convergence diagnostics
plot_convergence_diagnostics(bart_machine)

#destroy BART model
destroy_bart_machine(bart_machine)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
