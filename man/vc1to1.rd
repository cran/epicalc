\name{Matched case-control study}
\docType{data}
\alias{vc1to1}
\alias{vc1to6}
\title{Datasets on a matched case-control study of esophageal cancer}
\description{
Two different datasets for a matched case-control studies with 1 case : 1 controls
and 1 case : varying number of controls (from 1 to 6).

}
\usage{data(vc1to1)

data(vc1to6)}
\format{
  A data frame with 119 observations on the following 5 variables.
  \describe{
    \item{\code{matset}}{a numeric vector indicating matched set number from 1 to 26}
    \item{\code{case}}{a numeric vector: 1=case, 0=control}
    \item{\code{smoking}}{a numeric vector: 1=smoker, 0=non-smoker}
    \item{\code{rubber}}{a numeric vector: 1=exposed, 0=never exposed to rubber industry}
    \item{\code{alcohol}}{a numeric vector: 1=drinker, 0=non-drinker}
  }
}
\source{Chongsuvivatwong,  V. 1990
A case-control study of esophageal cancer  in Southern Thailand. 
\emph{J Gastro Hep} \bold{5}:391--394.}
\seealso{
'infert' in the standard R datasets.
}
\examples{
data(vc1to6)
use(vc1to6)
des()
matchTab(case, alcohol, matset)
}
\keyword{datasets}
    