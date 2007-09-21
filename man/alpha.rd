\name{alpha}
\alias{alpha}
\title{Cronbach's alpha}
\description{Calculate reliability coefficient of items in a data frame}
\usage{
alpha (vars, dataFrame = .data, reverse = TRUE)   
}
\arguments{
	\item{vars}{a vector containing at least three variables from the data frame}
	\item{dataFrame}{data frame where items are set as variables}
	\item{reverse}{whether item(s) negatively correlated with other majority will be reversed}
}
\details{This function is based on the 'reliability' function from package 'Rcmdr', which computes Cronbach's alpha for a composite scale. 

There must be at least three items in 'vars' specified by names of the variables or their index.

The argument 'reverse' (default = TRUE) automatically reverses items negatively correlated with other majority into negative and reports the activities in the first column of the last result section.

Similar to the 'reliability' function, users can see the effect of removing each item on the coefficents and the item-rest correlation. 

Note that the current version of this function omits whole record(s) if any of the items specified in 'vars' contains missing values.

}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'cronbach' from 'psy' package and 'reliability' from 'Rcmdr' package}
\examples{
data(Cars93, package="MASS")
use(Cars93)
alpha(vars=c(Min.Price:MPG.highway, EngineSize:Rev.per.mile, Weight))
}
\keyword{database}
