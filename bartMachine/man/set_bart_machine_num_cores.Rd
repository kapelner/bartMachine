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
Number of cores to use
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
None.
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
\code{\link{bart_machine_num_cores}}
}
\examples{
\dontrun{
## set all parallelized functions to use 4 cores
## set_bart_machine_num_cores(4) 
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
