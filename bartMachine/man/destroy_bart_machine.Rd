\name{destroy_bart_machine}
\alias{destroy_bart_machine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Destroy BART Model (deprecated --- do not use!)
}
\description{
A deprecated function that previously was responsible for cleaning up the RAM 
associated with a BART model. This is now handled natively by R's garbage collection.
}
\usage{
destroy_bart_machine(bart_machine)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bart_machine}{
    deprecated --- do not use!
}
}
\details{
Removing a ``bart_machine'' object from \code{R} previously did not free heap space from Java. 
Since BART objects can consume a large amount of RAM, it is important to remove 
these objects by calling this function if they are no longer needed or many BART 
objects are being created. This operation is now taken care of by R's garbage collection.
This function is deprecated and should not be used. However, running it is harmless.
}
\value{
None.
}

\author{
Adam Kapelner and Justin Bleich
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
##None
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line