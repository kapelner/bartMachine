\name{set_bart_machine_num_cores}
\alias{set_bart_machine_num_cores}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set the Number of Cores for BART
}
\description{
Sets the number of cores to be used for all parallelized BART functions.
}
\usage{
set_bart_machine_num_cores(num_cores)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num_cores}{
Number of cores to use. If the number of cores is more than 1, setting the seed during model construction
cannot be deterministic.
}
}

\value{
None.
}

\author{
Adam Kapelner and Justin Bleich
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bart_machine_num_cores}}
}
\examples{
\dontrun{
## set all parallelized functions to use 4 cores
## set_bart_machine_num_cores(4) 
}
}
