\name{fillin}
\alias{fillin}
\title{fillin - Rectangularize a dataframe}
\description{
    fillin adds observations with missing data so that all combinations of 
    the specified variables exist, thus making a complete rectangularization.
}
\usage{
fillin(dataFrame=.data, select, fill=NA)   
}
\arguments{
	\item{dataFrame}{a data frame.}
	\item{select}{a vector of at least 2 variables from the data frame. If missing all variables in the data frame will be used.}
	\item{fill}{the value used to fill in all other variables from the data frame. Defaults to NA.}
}

\author{Edward McNeil
	\email{ <edward.m@psu.ac.th>}
}
\seealso{table}
\examples{
data <- data.frame(sex=c("female","male","male"), 
  race=c("black","black","white"), 
  x1=c(.5,.4,.1), 
  x2=c(32,40,53))
data
fillin(data, select=c(sex,race))
data.new <- fillin(data, select=c(sex,race), fill=0)
data.new

data <- data.frame(x = gl(3,3),
                   y = rep(gl(3,1),3),
                   z = gl(2,6,length=9),
                   n = rpois(9,10) )
data
fillin(data, c(x,y))
fillin(data, c(x,z))
fillin(data, c(x,y,z), fill=0)
}
\keyword{database}
