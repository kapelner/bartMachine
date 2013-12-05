\name{get_sigsqs}
\alias{get_sigsqs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Posterior Error Variance Estimates 
}
\description{
Returns the posterior estimates of the error variance from the Gibbs samples with an option to create a histogram of the posterior estimates of the error variance  with a credible interval overlaid.
}
\usage{
get_sigsqs(bart_machine, after_burn_in = T, 
plot_hist = F, plot_CI = .95, plot_sigma = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
An object of class ``bartMachine''.
}
  \item{after_burn_in}{
If TRUE, only the \eqn{\sigma^2} draws after the burn-in period are returned.
}
  \item{plot_hist}{
If TRUE, a histogram of the posterior \eqn{\sigma^2} draws is generated.
}
  \item{plot_CI}{
Confidence level for credible interval on histogram.
}
  \item{plot_sigma}{
If TRUE, plots \eqn{\sigma} instead of \eqn{\sigma^2}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a vector of posterior \eqn{\sigma^2} draws (with or without the burn-in samples).
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Adam Kapelner and Justin Bleich
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get_sigsqs}}
}
\examples{
\dontrun{
#generate Friedman data
set.seed(11)
n  = 300 
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
bart_machine = build_bart_machine(X, y)

#get posterior sigma^2's after burn-in and plot
sigsqs = get_sigsqs(bart_machine, plot_hist = TRUE)

#destroy BART model
destroy_bart_machine(bart_machine)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
