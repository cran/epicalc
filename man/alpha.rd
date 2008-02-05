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
	\item{casewise}{whether only records with complete data will be used}
	\item{reverse}{whether item(s) negatively correlated with other majority will be reversed prior to computation}
	\item{decimal}{number of decimal places displayed}
	\item{var.labels}{presence of descriptions of variables in the last column of the output}
	\item{var.labels.trunc}{number of characters used for variable descriptions, long labels can be truncated}
	\item{vars.to.reverse}{variable(s) to reverse prior to computation}
  \item{standardized}{whether choosing the best subset of items is based on the standardized alpha coefficient, if FALSE then the unstandardized alpha coefficient is used}
}
\details{This function is based on the 'reliability' function from package 'Rcmdr', which computes Cronbach's alpha for a composite scale. 

There must be at least three items in 'vars' specified by their names or their index in the data frame.

The argument 'reverse' (default = TRUE) automatically reverses items negatively correlated with other majority into negative and reports the activities in the first column of the last result section. This can be overwritten by the argument 'vars.to.reverse'

Similar to the 'reliability' function, users can see the effect of removing each item on the coefficents and the item-rest correlation. 

'alphaBest' is a variant of 'alpha' for automatic selection of the subset of items (with appropriate items reversed) giving the highest possible Cronbach alpha. The resultant values include variable indices of excluded and remaining items, which can be forwarded to 'tableStack' to achieve total and mean scores of the best selected items.
}
\value{A list. 

'alpha' returns an object of class "alpha"
  \item{alpha}{unstandardized alpha coefficient}
  \item{std.alpha}{standardized alpha coefficient}
  \item{sample.size}{sample size}
  \item{use.method}{method for handling missing values}
  \item{rbar}{the average inter-item correlation}
  \item{items.selected}{names of variables included in the function}
  \item{alpha.if.removed}{a matrix of unstandardized and standardized alpha coefficients and correlation of each item with the rest of the items}
  \item{result}{as above but includes a column showing the items that were reversed (if TRUE) and a column of item description. As a matrix, it could be sent to a spreadsheet software using 'write.csv'}
  \item{decimal}{decimal places}
  \item{item.labels}{a character vector containing descriptions of the items}

'apha.Best' returns a list of the following elements
  \item{best.alpha}{the possible highest alpha obtained from the function}
  \item{removed}{indices of items removed by the function}
  \item{remaining}{indices of the remaining items}
  \item{items.reversed}{names of items reversed}
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
