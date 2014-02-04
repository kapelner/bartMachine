\name{set_bart_machine_memory}
\alias{set_bart_machine_memory}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initialize a JVM with a pre-specified heap size
}
\description{
Initialize a Java virtual Machine (JVM) with a pre-specified heap size in MB.  
}
\usage{
set_bart_machine_memory(bart_max_mem)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_max_mem}{
Java Virtual Machine heap size in MB
}
}
\details{
This function can only be called once per \code{R} session and must be called before \code{\link{build_bart_machine}}, else the 
default heap size of 1.5GB will be used.
}
\value{
None.
}

\author{
Adam Kapelner and Justin Bleich
}



\examples{
\dontrun{
##initialize a Java Virtual Machine with heap size of 3000MB
##this should be run before any BART models are built 
set_bart_machine_memory(3000) 
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
