\name{Detach all data frames}
\alias{detachAllData}
\title{Detach all data frames}
\description{Detach all data frames}
\usage{
detachAllData()
}
\details{The R command 'attach()' copies the data frame in the argument into a data frame in the search path (usually the second position) consequently making all the variables in the data frame easy to refer to. However, changing any element of the index data frame has no effect on the one in the search path unless the changed data frame is attached to the search path again. Having too many data frames in the search path subsequently causes confusion, not to mention an increase in memory usage. It is a good practice to detach the index data frame first before manipulating it and then attaching to it again. 'detachAllData()' is a self explanatory command which solves the over-attaching problem.

'detachAllData()' removes all non-function objects in the R search path. 
}

\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'use', 'detach', 'search'}
\examples{
attach(CO2)
data(Hakimi)
attach(Hakimi)
search()
detachAllData()
search()
}
\keyword{database}