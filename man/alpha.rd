\name{alpha}
\alias{alpha}
\alias{alphaBest}
\title{Cronbach's alpha}
\description{Calculate reliability coefficient of items in a data frame}
\usage{
alpha (vars, dataFrame = .data, casewise = FALSE, reverse = TRUE, 
    decimal = 4, vars.to.reverse = NULL, var.labels = TRUE, 
    var.labels.trunc =150)   
alphaBest (vars, standardized = FALSE, dataFrame = .data) 
}
\arguments{
	\item{vars}{a vector containing at least three variables from the data frame}
	\item{dataFrame}{data frame where items are set as variables}
	\item{casewise}{whether on records with completed data on vars will be used}
	\item{reverse}{whether item(s) negatively correlated with other majority will be reversed}
	\item{decimal}{number of decimal places displayed}
	\item{var.labels}{presence of descriptions of variables on the last column of output}
	\item{var.labels.trunc}{number of characters used for variable description}
	\item{vars.to.reverse}{variable(s) to reverse}
  \item{standardized}{whether choosing the best subset of items is based on standardized alpha coefficient}
}
\details{This function is based on the 'reliability' function from package 'Rcmdr', which computes Cronbach's alpha for a composite scale. 

There must be at least three items in 'vars' specified by names of the variables or their index.

The argument 'reverse' (default = TRUE) automatically reverses items negatively correlated with other majority into negative and reports the activities in the first column of the last result section. This is however overwritten by the argument 'vars.to.reverse'

Similar to the 'reliability' function, users can see the effect of removing each item on the coefficents and the item-rest correlation. 

'alphaBest' is a variant of 'alpha' for automatic selection of the subset of items (with appropriate reversion) giving highest Cronbach alpha. The resultant values include variable orders of items excluded by 'alphaBest' and the remaining, which can be forwarded to 'tableStack' to achieve total and mean scores of the selected items.
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'cronbach' from 'psy' package and 'reliability' from 'Rcmdr' package and 'tableStack' and 'unclassDataframe' of Epicalc}
\examples{
data(Cars93, package="MASS")
use(Cars93)
alpha(vars=c(Min.Price:MPG.highway, EngineSize))

data(Attitudes)
use(Attitudes)

alpha(qa1:qa18)  # Needs full screen of Rconsole
alpha(qa1:qa18, var.labels.trunc=30) 
                 # Fits in with default R console screen

alpha(qa1:qa18, reverse=FALSE)

alphaBest(qa1:qa18) -> best.alpha
best.alpha
tableStack(best.alpha$remaining, reverse=TRUE)
}
\keyword{database}
