\name{Unclass factors in a dataframe}
\alias{unclassDataframe}
\title{Unclass factor(s) in the default data frame}
\description{This function unclasses factor(s) in the default data frame (.data). }
\usage{
unclassDataframe (vars)    
}
\arguments{
	\item{vars}{a vector of variables in the data frame, usually factors, that will be unclassed}
}
\details{This function 'unclass'es several variables of class factor to their corresponding integer values. This is useful in further summation of items.
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'unclass'}
\examples{
expect1 <- c(3,4,3,2,5,3,2,5,2,4,4,3,2,4,4, 
   1,3,2,4,4,4,3,4,2,4,5,4,4,3,4)
expect2 <- c(3,2,4,3,5,3,4,5,4,4,5,5,3,4,4,
   3,4,2,3,5,3,4,4,2,4,5,4,4,3,5)
found1  <- c(1,3,4,3,4,3,3,2,2,4,5,4,3,4,3,
   1,1,2,3,4,4,1,1,3,4,5,4,1,4,2)
found2  <- c(1,1,2,1,3,1,1,2,2,4,3,3,1,1,3,
   3,1,1,2,1,1,1,1,1,3,5,4,4,1,1)
.data <- data.frame(expect1, expect2, found1, found2)
rm(expect1, expect2, found1, found2) # clean up
des()
level.lab <- list("Excellent"=1, "Good"=2,
   "Fair"=3, "Poor"=4, "Very poor"=5)
for (i in 1:4) {
   .data[,i] <- factor(.data[,i])
   levels(.data[,i]) <- level.lab
}
des() # All variables are factors
unclassDataframe(vars=c(1,4))
des() # Only variables #1 and #4 are 'unclass'ed
}
\keyword{aplot}
