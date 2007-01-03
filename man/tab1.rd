\name{tab1}
\alias{tab1}
\title{One-way tabulation}
\description{One-way tabulation with automatic bar chart}
\usage{
tab1 (x0, decimal = 1, sort.group = c(FALSE, "decreasing", 
"increasing"), cum.percent = !any(is.na(x0)), graph = TRUE, 
missing = TRUE, bar.values = c("frequency", "percent", "none")) 
}
\arguments{
	\item{x0}{a variable}
	\item{decimal}{number of decimals for the percentage in the table}
	\item{sort.group}{pattern for sorting of categories in the table as in the chart. Default is no sorting.}
	\item{cum.percent}{presence of cumulative percentage on the output table. Default is TRUE for a variable without any missing value.}
	\item{graph}{automatic graphing}
	\item{missing}{include the missing values category or <NA> in the graphic display}
	\item{bar.values}{include the value of frequeny, percentage or none at the end of each bar}
}
\details{'tab1' is an advanced one-way tabulation providing a nice table as well as a bar chart. The description of the variable is also used in the main title of the graph.

The bar chart is vertical unless the number of categories is more than six \strong{and} any of the labels of the levels consists of more than 8 characters.

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
tab1(ill) # Note the presence of cumulative percentage on the table.
tab1(ill, cum.percent=FALSE)
tab1(chocolate) # Cumulative percentage is automatically off.
tab1(chocolate, cum.percent=TRUE) # Slightly too many columns!
tab1(chocolate, missing=FALSE, bar.values="percent")

}
\keyword{aplot}