\name{des}
\alias{des}
\title{Desription of a data frame or a variable}
\description{Description of a data frame or a variable}
\usage{
des(x=.data, search.position=2)
}
\arguments{
	\item{x}{an object such as a vector (variable), a matrix, a table, a list or a data frame}
	\item{search.position}{position of the data frame to describe variable as referred by 'search()'}
}
\details{The default value of x (ie if no argument is supplied) is '.data'. If 'x' is a data frame, its variable names will be listed with class and the description of each variable}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'use', 'summ' and 'label.var'}
\examples{
data(oswego)
.data <- oswego
attach(.data)

# The above lines generate a hypothetical data frame. 
# In reality, one just exploits 'use("oswego.rec"), if the file is available.
des()
des(sex)
summ()
}
\keyword{database}
