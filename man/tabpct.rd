\name{tabpct}
\alias{tabpct}
\title{Two-way tabulation}
\description{Two-way tabulation with automatic mosaic plot}
\usage{
tabpct (row, col, ..., decimal = 1, graph = TRUE, las = 0)  
}
\arguments{
	\item{row, col}{variables}
	\item{...}{additional arguments for 'table'}
	\item{decimal}{number of decimals for the percentage in the table}
	\item{graph}{automatic graphing}
	\item{las}{orientation of group labelling, 

0: always parallel to axis

1: always horizontal,

2: always perpendicular to the axis,

3: always vertical.
}
}
\details{'tabpct' gives column and row percent cross-tabulation as well as mosaic plot. 

The width of the bar in the plot denotes the relative proportion of the row variable.

Inside each bar, the relative proportion denotes the distribution of column variables within each row variable.}
\value{Tables of row and column percentage}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'tab1', 'table', 'mosaicplot'}
\examples{
data(Oswego)
.data <- Oswego
attach(.data)

# The above commands generate a data frame. 
# In reality, one just exploits 'use("Oswego.rec")', if the file is available.
agegr <- cut(age, breaks=c(0,20,40,60,80))
label.var(agegr, "age group")
tabpct(agegr, ill)
}
\keyword{aplot}