\name{summ}
\alias{summ}
\title{Summary with graph}
\description{Summary of data frame in a convenient table. Summary of a variable with statistics and graph}
\usage{
summ(x = .data, by = NULL, graph = TRUE, box = FALSE, pch = 18, 
    ylab = "auto", main = "auto", cex.X.axis = 1, cex.Y.axis = 1, 
    dot.col = "auto", ...) 
}
\details{For data frames, 'summ' gives basic statistics of each variable in the data frame. The other arguments are ignored.

For single vectors, a sorted dot chart is also provided, if graph=TRUE (default).}
\arguments{
	\item{x}{'x' can be a data frame or a vector. 'summ()' is the same as 'summ(.data)'}
	\item{by}{a stratification variable, valid only when x is a vector}
	\item{graph}{automatic plot (sorted dot chart) if 'x' is a vector}
	\item{box}{add a boxplot to the graph (by=NULL)}
	\item{pch}{plot characters}
  \item{ylab}{annotation on Y axis}
  \item{main}{main title of the graph}
  \item{cex.X.axis}{character extension scale of X axis}
  \item{cex.Y.axis}{character extension scale of Y axis}
  \item{dot.col}{colour(s) of plot character(s)}
  \item{...}{additional graph parameters}
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'summary', 'use', 'des'}
\examples{
data(BP)
use(BP)
summ()
summ(sex)
summ(sbp, box=TRUE)
summ(sbp, dot.col="brown")
summ(sbp, by=sex)
# Changing dot colours
summ(sbp, by=sex, dot.col = c("blue","orange"))
# Enlarging main title and other elements
summ(sbp, by=sex, cex.main=1.5, cex.X.axis=1.5, cex.Y.axis=1.7)

# Free vector
summ(rnorm(1000))
summ((1:100)^2, by=rep(1:2, 50))
summ((1:100)^2, by=rep(c("Odd","Even"), 50), main="Quadratic distribution by odd and even numbers")
}
\keyword{database}
