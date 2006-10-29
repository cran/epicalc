\name{lookup}
\alias{lookup}
\title{Recode several values of a variable}
\description{Systematic replacement of several values of a variable using an array}
\usage{
lookup(x, lookup.array) 
}
\arguments{
	\item{x}{a variable}
	\item{lookup.array}{a n-by-2 array used for looking up the recoding scheme}
}
\details{This command is used for changing more than one value of a variable using a n-by-2 look-up array. The first column of the look-up array (index column) must be unique.

If either the variable or the look-up table is character, the result vector will be character.

For changing groups of factor variable, please use 'levels(var) <- ' instead.
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'replace', 'change.value'}
\examples{
a       <- c( 1, 2, 3, 4, 5,  NA)
tx      <- rbind(c(1,2),c(2,1),c(3,NA),c(NA,999)) 

# Swapping values of 1 and 2; replacing 3 with missing and missing with 999
new.a  <- lookup(a, tx)
a
new.a
cbind(a, new.a)
table(a, new.a, exclude=NULL) # All diagonal cells which are non-zero are the recoded cells. 

## Character look-up table
tx1 <- cbind(c("1","2","3", NA), c("a","b",NA, "missing"))
b.new <- lookup(a, tx1)
table(a, b.new, exclude=NULL)
}
\keyword{database}