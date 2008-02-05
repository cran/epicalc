\name{zap}
\alias{zap}
\title{Remove and detach all}
\description{Detach and remove all objects and data frames from the global environment}
\usage{
zap()
}
\details{The R command 'attach()' copies the data frame in the argument into a data frame in the search path (usually the second position) consequently making all the variables in the data frame easy to refer to. However, changing any element of the index data frame has no effect on the one in the search path unless the changed data frame is attached to the search path again. Having too many data frames in the search path subsequently causes confusion, not to mention an increase in memory usage. It is a good practice to detach the index data frame first before manipulating it and then attaching to it again. 'detachAllData()' is a self explanatory command which solves the over-attaching problem.

'zap()' is a combination of 'detachAllData()' and removal of non-function objects in the R workspace. 

At the commencement of a new session, 'zap()' can be quite useful to clean the objects left over from previous R sessions and detach from any unwanted data frames. 

'zap()' as well as 'rm(list=ls())' do not remove any objects starting with a dot '.', which are meant to be hidden. Therefore the object '.data' is resistant to 'zap()'.}

\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'use', 'detach', 'ls', 'rm'}
\examples{
object1 <- 1:5
object2 <- list(a=3, b=5)
function1 <- function(x) {x^3 +1}
attach(CO2)
lsNoFunction()
ls() 
search()
detachAllData()
ls() 
search()
zap()
ls() 
search()
rm(function1)
}
\keyword{database}