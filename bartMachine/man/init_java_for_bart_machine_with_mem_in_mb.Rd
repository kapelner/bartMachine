\name{init_java_for_bart_machine_with_mem_in_mb}
\alias{init_java_for_bart_machine_with_mem_in_mb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initialize a JVM with a pre-specified heap size
}
\description{
Initialize a Java virtual Machine (JVM) with a pre-specified heap size in MB.  
}
\usage{
init_java_for_bart_machine_with_mem_in_mb(bart_max_mem)
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
\references{
%% ~put references to the literature/web site here ~
}
\author{
Adam Kapelner and Justin Bleich
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##initialize a Java Virtual Machine with heap size of 3000MB
##this should be run before any BART models are built 
##init_java_for_bart_machine_with_mem_in_mb(3000) ##not run
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
