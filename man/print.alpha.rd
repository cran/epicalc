\name{print alpha}
\alias{print.alpha}
\title{Print alpha object}
\description{To print results related to Cronbach's alpha}
\usage{
\method{print}{alpha}(x, ...)
}
\arguments{
       \item{x}{object of 'alpha' class}
       \item{...}{further arguments passed to or used by methods.}
}
\author{Virasakdi Chongsuvivatwong
       \email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'tableStack'}
\examples{
data(Attitudes)
alpha(qa1:qa18, dataFrame=Attitudes) -> a
print(a)
}
\keyword{database}

