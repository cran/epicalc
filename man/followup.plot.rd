\name{Follow-up Plot}
\alias{followup.plot}
\title{Longitudinal followup plot}
\description{Plot longitudinal values of individuals with or without stratification}
\usage{
followup.plot (id, time, outcome, by = NULL, 
	n.of.lines = NULL, legend = TRUE, line.col="blue") 
}
\arguments{
	\item{id}{idenfication variable of the same subject being followed up}
	\item{time}{time at each measurement}
	\item{outcome}{continuous outcome variable}
	\item{by}{stratification factor if any}
	\item{n.of.lines}{number of lines (or number of subjects in the data frame) randomly chosen for drawing}
	\item{legend}{whether a legend will be automatically included in the graph}
	\item{line.col}{line color(s) for non-stratified plot}
}
\details{'followup.plot' plots outcome over time of the individual subjects.

If a stratification variable 'by' is specified, the levels of this variable will be used to color the lines. 

'n.of.lines' is used to reduce the number of lines to allow the pattern to be seen more clearly. 

'legend' is omitted if 'n.of.lines' is not NULL or the number of subjects exceeds 7 without stratification.

'line.col' works only for non-stratified plot. It can be a single standard color or "multicolor".  
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'plot','lines'}
\examples{
use(Indometh)
followup.plot(Subject, time, conc)

library(MASS)
use(Sitka)
followup.plot(tree, Time, size)
followup.plot(tree, Time, size, line.col = "brown")
followup.plot(tree, Time, size, line.col = "multicolor")
followup.plot(tree, Time, size, n.of.lines=20, line.col = "multicolor")



# Breakdown of color by treatment group
followup.plot(tree, Time, size, by=treat)

# The number lines reduced to 40
followup.plot(tree, Time, size, by=treat, n.of.lines=40)
}
\keyword{aplot}