\name{get_var_props_over_chain}
\alias{get_var_props_over_chain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the Variable Inclusion Proportions
}
\description{
Computes the variable inclusion proportions for a BART model. 
}
\usage{
get_var_props_over_chain(bart_machine, type = "splits")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
An object of class ``bartMachine''.
}
  \item{type}{
If ``splits'', then the proportion of times each variable is chosen for a splitting rule versus all splitting rules is computed. If ``trees'', then the proportion of times each variable appears in a tree versus all appearances of variables in trees is computed.
}
}

\value{
Returns a vector of the variable inclusion proportions.
}

\author{
Adam Kapelner and Justin Bleich
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get_var_counts_over_chain}}
}
\examples{
#generate Friedman data
set.seed(11)
n  = 200 
p = 10
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = build_bart_machine(X, y, num_trees = 20)

#Get variable inclusion proportions
var_props = get_var_props_over_chain(bart_machine)
print(var_props)

#destroy BART model
destroy_bart_machine(bart_machine)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
