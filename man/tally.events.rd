\name{tally events}
\alias{tally.events}
\title{Tally a date variable by larger time unit}
\description{Tabulate dates breakdown by week, month or year and plot the results}
\usage{
\method{tally}{events}(x, by=NULL, breaks=c("day","week","month","year"), 
       graph=TRUE, type="l", line.col="auto", legend = TRUE, legend.site="topright", 
       legend.bg = "white", ylim="auto", cex=1, addmargins=TRUE, ...) 
}
\arguments{
       \item{x}{a date variable}
       \item{by}{a grouping elements}
       \item{breaks}{time unit for aggregation of the events}
       \item{graph}{whether the table be plotted}
       \item{type}{graph type}
       \item{line.col}{line colour}
       \item{legend}{wheter the legend will be produced if there are more than one groups}
       \item{legend.site}{a single character string indicating location of the legend. See details of ?legend}
       \item{legend.bg}{background colour of the legend}
       \item{ylim}{Y axis limits}
       \item{cex}{character expanding factor for the legend}
       \item{addmargins}{whether the margin values of the cross-table will be computed}
       \item{...}{additional graphic parameters passed on to other methods} 
}
\details{This function produces table of events by day, month or year with zero cells included. It also plots the table.

'by' is a grouping variable, usually a factor.

'breaks' can be "day", "week", "month" and "year".

'type' can be "l", "p", "b", "c", "o", "h", "s" and "S" only when there is only group (by=NULL). Otherwise, graph type will be 'l'.

'line.col' control line colours in the graph and the legend

If 'legend = TRUE" (by default), a legend box is automatically drawn on the "topright" corner of the graph. This character string can be changed to others such as, "topleft", "center", etc (see examples).

}
\author{Virasakdi Chongsuvivatwong
       \email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'dotplot'}
\examples{
random.dates <- as.Date("2001/1/1") + round(20*stats::runif(100))
tally.events(random.dates)
# Compare with
summ(random.dates)
# and
dotplot(random.dates)
tally.events(random.dates, las=2)
tally.events(random.dates, las=2, type = "h", lwd =2, ylim = c(0,20))
random.dates2 <- as.Date("2001/1/1") + round(500*stats::runif(100))
gender100 <- c(rep("F", 50),rep("M", 50))
tally.events(random.dates2, las=2, breaks="week")
tally.events(random.dates2, las=2, breaks="month", type="h")
tally.events(random.dates2, breaks="week", by=gender100)
tally.events(random.dates2, breaks="month", by=gender100, cex=2, line.col=c("blue","brown"), lwd=2)
}
\keyword{database}

