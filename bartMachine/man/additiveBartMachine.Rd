\name{additiveBartMachine}
\alias{additiveBartMachine}
\title{
Fit Generalized Additive BART Regression Model
}
\description{
Builds a BART regression model where the interactions are constrained i.e. the model is a type of GAM. 
For instance, if domain knowledge tells you that x_1 cannot interact with other variables then the response must be 
modeled as f_a(x1) + f_b(x2, x3,...). This function allows BART models to be fit to each additive component of the 
model.
}
\usage{
additiveBartMachine(constraints = NULL, cv = FALSE, run_in_sample = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{constraints}{
A list of vectors of size \code{A} where the elements in the vectors correspond to features in the data frame \code{X} specified by 
either the column number as a numeric value or the column name as a string e.g. \code{list(c(1, 2), c("nox", "rm"))}. The 
elements of the vectors can be reused among components for any level of interaction complexity you wish. Default is 
\code{NULL} which corresponds to \code{list(1, 2, ..., p)} where \code{p} is the number of columns in the design matrix 
\code{X}. This thereby fits the canonical GAM. 
}
  \item{cv}{
Whether to run each additive BART component using cross-validation to select the locally optimal hyperparameter values.
}
  \item{num_runs_through}{
The number of times to iteratively backfit through the additive model. The default is 3. We recommend using at least 2 in order to
estimate sigsq properly for at least one set of the additive models.
}
  \item{run_in_sample}{
If TRUE, in-sample statistics such as \eqn{\hat{f}(x)}, Pseudo-\eqn{R^2}, and RMSE are computed. Setting this to FALSE when not needed can decrease computation time. 
}
  \item{\dots}{
Additional arguments to be passed to \code{bartMachine} or . Must include \code{X} and \code{y}.
}
}

\value{
Returns an object of class ``additiveBartMachine'' which is similar to what is returned from \code{bartMachine} except with an added
parameter: \code{models} which contains the constituent additive models.
}
\references{
Adam Kapelner, Justin Bleich (2016). bartMachine: Machine Learning
with Bayesian Additive Regression Trees. Journal of Statistical
Software, 70(4), 1-40. doi:10.18637/jss.v070.i04
}
\author{
Adam Kapelner
}
\note{
This function may require significant run-time.
This function is parallelized by the number of cores set in \code{\link{set_bart_machine_num_cores}} via calling \code{\link{bartMachine}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bartMachine}}
}
\examples{
\dontrun{
#generate Friedman data
set.seed(11)
n  = 200 
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)

##build BART regression model
additive_bart_machine = additiveBartMachine(list(3, 4, 5), X = X, y = y)

#information about the additive model
summary(additive_bart_machine)
}

}
