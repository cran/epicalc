\name{tab1}
\alias{tab1}
\alias{print.tab1}
\title{One-way tabulation}
\description{One-way tabulation with automatic bar chart}
\usage{
tab1(x0, decimal = 1, sort.group = c(FALSE, "decreasing", 
"increasing"), cum.percent = !any(is.na(x0)), graph = TRUE, 
missing = TRUE, bar.values = c("frequency", "percent", "none"),
horiz=FALSE, ...)

\method{print}{tab1}(x, ...)
}
\arguments{
	\item{x0}{a variable}
	\item{decimal}{number of decimals for the percentages in the table}
	\item{sort.group}{pattern for sorting categories in the table and in the chart. Default is no sorting.}
	\item{cum.percent}{presence of cumulative percentage in the output table. Default is TRUE for a variable without any missing values.}
	\item{graph}{whether a graph should be shown}
	\item{missing}{include the missing values category or <NA> in the graphic display}
	\item{bar.values}{include the value of frequency, percentage or none at the end of each bar}
	\item{horiz}{set the bar chart to horizontal orientation}
  \item{x}{object of class 'tab1' obtained from saving 'tab1' results}
  \item{...}{further arguments passed to or used by other methods}
}
\details{'tab1' is an advanced one-way tabulation providing a nice frequency table as well as a bar chart. The description of the variable is also used in the main title of the graph.

The bar chart is vertical unless the number of categories is more than six \strong{and} any of the labels of the levels consists of more than 8 characters \strong{or} 'horiz' is set to TRUE.

}
\value{Output table}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'tabpct', 'label.var', 'table', 'barplot'}
\examples{
tab1(state.division)
tab1(state.division, bar.values ="percent")
tab1(state.division, sort.group ="decreasing")
tab1(state.division, sort.group ="increasing")

data(Oswego)
use(Oswego)
tab1(ill) # Note the column of cumulative percentages in the table.
tab1(ill, cum.percent=FALSE)
tab1(chocolate) 
# Due to missing values, cumulative percentages are now automatically turned off.
tab1(chocolate, cum.percent=TRUE) # Slightly too many columns!
tab1(chocolate, missing=FALSE, bar.values="percent")
agegr <- cut(age, breaks=c(0,20,60,80))
tab1(agegr)
tab1(agegr, col=c("red","yellow","blue"))
tab1(agegr, horiz=TRUE)

tab1(agegr) -> a
print(a)
a # same results
attributes(a)
a$output.table
}
\keyword{aplot}