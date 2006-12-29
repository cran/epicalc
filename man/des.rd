\name{des}
\alias{des}
\title{Desription of a data frame or a variable}
\description{Description of a data frame or a variable}
\usage{
des(x=.data)
}
\arguments{
	\item{x}{an object such as a vector (variable), a matrix, a table, a list or a data frame}
}
\details{The default value of x (ie if no argument is supplied) is '.data'. If 'x' is a data frame, its variable names will be listed with class and the description of each variable. 

If 'x' is a variable, the environment and attached data frame containing 'x' will be described.}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'use', 'summ' and 'label.var'}
\examples{
data(oswego)
use(oswego)
des()
des(oswego)
des(infert)
attach(infert)
search()
des(sex)
des(induced)
age <- "abc" # Just a silly example for a variable
des(age)
rm(age)
detachAllData()  
}
\keyword{database}
