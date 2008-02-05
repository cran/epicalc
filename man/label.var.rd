\name{Variable manipulation}
\alias{label.var}
\alias{pack}
\title{Variable manipulation}
\description{Label a variable; integrate outside variable(s) into .data.}
\usage{
label.var(var, label, pack = TRUE, dataFrame = .data)   
pack(dataFrame = .data) 
}
\arguments{
	\item{var}{A variable inside .data or a free vector.}
	\item{label}{A short description of the variable}
	\item{pack}{Remove the original free variable?}
	\item{dataFrame}{Destination data frame where all variables of the same length are labeled or packed into}
}
\details{A data frame imported from Stata or SPSS can have 'variable labels', which is adopted as an attribute (but not used) by R. 

Epicalc exploits this attribute by displaying the labels in the output from 'des()' and graphs following 'summ(var)', tab1(var)', 'tabpct(var1, var2)'.

For free vector(s) (variables outside the data frame), 'label.var' appends vector(s) to the data frame specified with the descriptive label attached. For variables already in the data frame, the command simply attaches the label to the variable. The 'var.labels' attribute is updated in the data frame.  The argument 'pack', if TRUE, removes the original vector. This is useful to avoid redundancy and confusion. 

More than one free vector of the same length can be integrated into the data frame (.data) without labelling, using 'pack()'.
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'use','des'}
\examples{

sbp <- c(120, 100, 110, 120, 140, 120,  NA,  NA) 
dbp <- c( 80,  80,  70,  80,  70,  NA,  70,  60)
.data <- data.frame(sbp, dbp)
use(.data)
pack()
des()
label.var(sbp, "systolic BP")
label.var(dbp, "diastolic BP")
des()
pp <- sbp - dbp # This is a new free vector in the global environment.
summ(pp) # unlabelled
label.var(pp, "pulse pressure")
des()
summ(pp)

## Silly things to do. Just for demonstration.
pp2 <- pp^2
pp3 <- pp^3
pack()
.data
}
\keyword{database}