\name{kap}
\alias{kap}
\title{Kappa statistic}
\description{Measurement of agreement in categorization}
\usage{
kap(kaptable, wttable = NULL) 
}
\arguments{
	\item{kaptable}{Cross tabulation of classifications made by two raters}
	\item{wttable}{Cross tabulation of weights of agreement among categories}
}
\details{
'kap' computes the kappa statistic, the level of agreement between two raters. 
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'table'}
\examples{
class  <- c("Normal","Benign","Suspect","Cancer")
raterA <- gl(4,4, label=class)
raterB <- gl(4,1,16, label=class)
freq   <- c(50,2,0,1,2,30,4,3,0,0,20,1,1,3,4,25)
table1 <- xtabs(freq ~ raterA + raterB)
table1
kap(table1)
wt <-c(1,.5,0,0,.5,1,0,0,0,0,1,.8,0,0,.8,1)
wttable <- xtabs(wt ~ raterA + raterB)
wttable # Agreement between benign vs normal is .5, suspect vs cancer is .8
kap(table1, wttable=wttable)
}
\keyword{array}
