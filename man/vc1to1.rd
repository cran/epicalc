\name{Matched case-control study}
\docType{data}
\alias{vc1to1}
\alias{vc1to6}
\title{Dataset on matched case-control study of esophageal cancer}
\description{
Matched case-control studies with 1 case : 1 controls
and 1 case : varying number of controls (from 1 to 6).

matset   = matched set from 1 to 26                    

case     = whether the subject was case(1) or control(0) 

smoking  = whether the subject smoked: 0 = yes, 1 = no

rubber   = whether the subject was exposed to rubber
           processing industry in the past: 0 = yes, 1 = no

alcohol  = whether the subject drink alcohol regularly: 0 = yes, 1 = no
}
\usage{data(vc1to1)

data(vc1to6)}
\format{A data frame containing 26 case-control matched sets.}
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
match.tab(case, alcohol, matset)
}
\keyword{datasets}
    