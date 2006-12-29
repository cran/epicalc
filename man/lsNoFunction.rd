\name{List non-function objects}
\alias{lsNoFunction}
\title{List non-function objects}
\description{List all objects except newly created function}
\usage{
lsNoFunction()
}
\details{Compared to standard 'ls()', this function displays only the subset of 'ls()' which are not function. 

The member of this list can be removed by 'zap()' but not the set of the functions created.

Usually the ordinary user do not need to use this command.}

\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'use', 'detach', 'ls', 'rm'}
\examples{
object1 <- 1:5
object2 <- list(a=3, b=5)
function1 <- function(x) {x^3 +1}
ls() 
lsNoFunction()
}
\keyword{database}