\name{Codebook}
\alias{codebook}
\title{Codebook of a data frame}
\description{Print description, summary statistics and one-way tabulation of variables}
\usage{
codebook(dataFrame=.data)
}
\arguments{
	\item{dataFrame}{A data frame for printing the codebook}
}
\details{The default value of dataFrame (ie if no argument is supplied) is '.data'.

While 'summ' produces summary statistics of both numeric and factor variables, 'codebook' gives summary statistics of all numeric variables and one-way tabulation of all factors of the data frame.}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'use', 'summ', 'tab1' and 'tableStack'}
\examples{
data(Familydata)
use(Familydata)
des()
codebook(Familydata)
codebook() # Same result since line #2 has created .data from Familydata
}
\keyword{database}
