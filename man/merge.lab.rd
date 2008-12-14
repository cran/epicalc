\name{merge with labels kept}
\alias{merge.lab}
\title{Merge two data frames with variable labels kept}
\description{Create a new data frame from merging two with variable labels of the data frame kept}
\usage{
\method{merge}{lab}(x, y, ...)
}
\arguments{
       \item{x, y}{data frames to be merged to one}
       \item{...}{additional arguments passed on to 'merge'}
}
\details{This is the 'merge' method exclusively used for objects of class 'data.frame'.

Epicalc can create and make use of variable labels extensively. Unfortunately, they are ignored by the function 'merge'.

The current method, 'merge.lab', carries the variable labels from both data frames into the results. If the labels from these two data frames are conflicting, that in 'x' will be used.

}
\author{Virasakdi Chongsuvivatwong
       \email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'label.var'}
\examples{
data1 <- data.frame(id = c("A","B"), age = c(12,25))
label.var(id, "personal id", dataFrame=data1)
label.var(age, "age in years", dataFrame=data1)
des(data1)
data2 <- data.frame(id= LETTERS, money = 1:26)
label.var(id, "Identification code", dataFrame=data2)
label.var(money, "money in dollar", dataFrame=data2)
des(data2)
merge(data1, data2) -> aa
des(aa) # No variable description
merge.lab(data1, data2) -> bb
des(bb)
merge.lab(data2, data1) -> cc
des(cc) 
# Note the difference in description of 'id' between the three methods.
}
\keyword{database}

