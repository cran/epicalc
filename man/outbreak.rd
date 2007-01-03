\name{Outbreak investigation}
\docType{data}
\alias{Outbreak}
\title{Dataset from an outbreak of food poisoning on a sportsday, Thailand 1990.}
\description{
A dataset from an outbreak investigation concerning food poisoning in a sportsday, Thailand 1990.

Dichotomous variables for exposures and symptoms were coded as the following:
		\tabular{lll}{
        	\tab 0 \tab = no\cr
         	\tab 1 \tab = yes\cr
         	\tab 99 \tab = missing or unknown\cr
       }
}
\usage{data(Outbreak)}
\format{
  A data frame with 1094 observations on the following 13 variables.
  \describe{
    \item{\code{id}}{a numeric vector}
    \item{\code{sex}}{a numeric vector}
    \item{\code{age}}{a numeric vector: age in years}
		\tabular{lll}{
        	\tab 99 \tab = missing\cr
       }
    \item{\code{exptime}}{a AsIs or character: exposure time}
    \item{\code{beefcurry}}{a numeric vector}
    \item{\code{saltegg}}{a numeric vector}
    \item{\code{eclair}}{a numeric vector: pieces of eclair eaten}
		\tabular{lll}{
        	\tab 88 \tab = ate but not remember how much\cr
         	\tab 99 \tab = totally missing information\cr
       }
    \item{\code{water}}{a numeric vector}
    \item{\code{onset}}{a AsIs or character: onset time}
    \item{\code{nausea}}{a numeric vector}
    \item{\code{vomiting}}{a numeric vector}
    \item{\code{abdpain}}{a numeric vector: abdominal pain}
    \item{\code{diarrhea}}{a numeric vector}
  }
}
\references{Thaikruea, L., Pataraarechachai, J., Savanpunyalert, P., Naluponjiragul, U. 1995
An unusual outbreak of food poisoning. \emph{Southeast Asian J Trop Med Public Health} 
\bold{26(1)}:78-85.
}
\examples{
data(Outbreak)
use(Outbreak)

# Distribution of reported pieces of eclair taken
tab1(eclair) 

# Defining missing value
recode(eclair, eclair>20, NA) 
pieces.of.eclair <- cut(eclair, c(0,1,2,20))
tabpct(pieces.of.eclair, diarrhea) 
}
\keyword{datasets}
    