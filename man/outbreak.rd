\name{Outbreak investigation}
\docType{data}
\alias{outbreak}
\title{Outbreak of food poisoning in a sportsday, Thailand 1990.}
\description{
Variables were coded as 0 = no, 1 = yes and 9 = missing/unknown
for three food items consumed by participants: 

      'beefcurry' (beef curry)

      'saltegg' (salted eggs)  

      'water'. 

The variable 'eclair' records the number of pieces eaten by each participant.  
Missing values were coded as follows: 

      88 = "ate but do not remember how much", 

      90 = totally missing information. 


Some participants experienced gastrointestinal symptoms, such as: 
'nausea', 'vomiting', 'abdpain' (abdominal pain) and 'diarrhea'. 

All these outcomes were coded as 0 = no, 1 = yes.

The age of each participant was recorded in years with 99 = missing.
 
The variables 'exptime' and 'onset' are the exposure and onset times, 
which are in character format, or 'AsIs' in R terminology
}
\usage{data(outbreak)}
\format{A data frame containing 1,094 observations and 13 variables.}
\references{Thaikruea, L., Pataraarechachai, J., Savanpunyalert, P., Naluponjiragul, U. 1995
An unusual outbreak of food poisoning. \emph{Southeast Asian J Trop Med Public Health} 
\bold{26(1)}:78-85.
}
\examples{
data(outbreak)
use(outbreak)

# Distribution of reported pieces of eclair taken
tab1(eclair) 

# Defining missing value
recode(eclair, eclair>20, NA) 
pieces.of.eclair <- cut(eclair, c(0,1,2,20))
tabpct(pieces.of.eclair, diarrhea) 
}
\keyword{datasets}
    