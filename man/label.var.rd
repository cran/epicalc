\name{label.var}
\alias{label.var}
\alias{pack}
\alias{sortBy}
\title{Label a variable, pack variables into a data frame and sort all data}
\description{Label a variable; integrate outside variable(s) into .data; sort the whole dataset.}
\usage{
label.var(var, label, pack = TRUE)   
pack(dataFrame = .data) 
sortBy(...)
}
\arguments{
	\item{var}{A variable inside .data or a free vector.}
	\item{label}{Short description of the variable}
	\item{pack}{Remove the original free variable?}
	\item{dataFrame}{Destination data frame where all variables of the same length are packed into}
	\item{...}{index variable(s) used for sorting}
}
\details{A data frame adopted from Stata or SPSS often has a 'variable label', which is recognized (but ignored) by R. 

Epicalc exploits this attribute by displaying them in the output from 'des()' and graphs,  such as those resulting from 'summ(var)', tab1(var)', 'tabpct(var1, var2)'.

'label.var' adds or changes the variable label or description to a variable in '.data'. If the variable is a free vector, the variable is added into '.data'. The argument 'pack', if TRUE, removes the original vector. This is useful to avoid redundancy and confusion. 

More than one free vector of the same length can be integrated into the data frame (.data) without labelling using 'pack()'.

Finally, the whole dataset, and the variables outside, can be sorted by an index variable(s) inside the (...).
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'use','des'}
\examples{
sbp <- c(120, 100, 110, 120, 140, 120,  NA,  NA) 
dbp <- c( 80,  80,  70,  80,  70,  NA,  70,  60)
.data <- data.frame(sbp, dbp)
attach(.data, warn.conflicts=FALSE)

# The above lines generate a hypothetical data frame. In reality,
# one just exploits 'use("datafile")', if the "datafile" exists.
des()
label.var(sbp, "systolic BP")
label.var(dbp, "diastolic BP")
des()
pp <- sbp - dbp # This is a new free vector.
summ(pp) # unlabelled
label.var(pp, "pulse pressure")
des()
summ(pp)

## Silly things to do. Just for demonstration.
pp2 <- pp^2
pp3 <- pp^3
pack()

age <- c(37, 32, 24, 33, 31, 30, 26, 25)
age2 <- age^2
sortBy(age)
pack()
des()
.data
sortBy(age, decreasing=TRUE)
.data
}
\keyword{database}