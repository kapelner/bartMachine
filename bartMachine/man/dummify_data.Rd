\name{dummify_data}
\alias{dummify_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dummify Design Matrix
}
\description{
Create a data frame with factors converted to dummies. 
}
\usage{
dummify_data(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
	Data frame to be dummified. 
	}
}
\details{
The column names of the dummy variables are given by the ``FactorName_LevelName'' and are augmented to the end of the design matrix. See the example below.
}
\value{
Returns a data frame with factors converted to dummy indicator variables.
}

\author{
Adam Kapelner and Justin Bleich
}
\note{
BART handles dummification internally. This function is provided as a utility function. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
\dontrun{
#generate data
set.seed(11)
x1 = rnorm(20)
x2 = as.factor(ifelse(x1 > 0, "A", "B"))
x3 = runif(20)
X = data.frame(x1,x2,x3)
#dummify data
X_dummified = dummify_data(X)
print(X_dummified)
}
}
