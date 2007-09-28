\name{tableStack}
\alias{tableStack}
\title{Tabulation of variables in a stack form}
\description{One-way tabulate variables with the same possible range of distribution and stack into a new table with or without other descriptive statistics}
\usage{
tableStack (vars, minlevel = 1, maxlevel = 5, count = TRUE, means = TRUE, 
    medians = TRUE, sds = TRUE, decimal = 3, dataFrame = .data, 
    vars.to.reverse = NULL, var.labels = TRUE, reverse = FALSE) 
}
\arguments{
	\item{vars}{a vector of variables in the data frame}
	\item{minlevel}{possible minimum value of items specified by user}
	\item{maxlevel}{possible maximum value of items specified by user}
	\item{count}{whether number of valid records for each item should be displayed}
	\item{means}{whether means of all selected items should be displayed}
	\item{medians}{whether medians of all selected items should be displayed}
	\item{sds}{whether standard deviations of all selected items should be displayed}
	\item{decimal}{number of decimals displayed in the statistics}
	\item{dataFrame}{source data frame of the variables}
	\item{vars.to.reverse}{variable(s) to reverse}
	\item{var.labels}{presence of descriptions of variables on the last column of output}
	\item{reverse}{whether item(s) negatively correlated with other majority will be reversed}
}
\details{This function simultaneously explores several variables with a fixed integer rating scale. For non-factor variables, default values for tabulation are 1 to 5 but can be specified by user.

The classes of the variables can be 'integer', 'factor' or 'logical but not any mixture.

Unlike, 'alpha', the argument 'reverse' default value is FALSE. It is also overwritten by 'vars.to.reverse'. Both command is ineffective when the variables are not of 'integer'. It is advised to run 'unclassDataframe' before running 'tableStack' with 'reverse=TRUE' or 'vars.to.reverse=...' .
} 
\value{A data frame containing the frequency of each value for each variable with or without descriptive statistics.

}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'table', 'tab1', 'summ', 'alpha', 'unclassDataframe'}
\examples{
data(Oswego)
use(Oswego)
des()
tableStack(bakedham:fruitsalad)



expect1 <- c(3,4,3,2,5,3,2,5,2,4,4,3,2,4,4, 
   1,3,2,4,4,4,3,4,2,4,5,4,4,3,4)
expect2 <- c(3,2,4,3,5,3,4,5,4,4,5,5,3,4,4,
   3,4,2,3,5,3,4,4,2,4,5,4,4,3,5)
found1  <- c(1,3,4,3,4,3,3,2,2,4,5,4,3,4,3,
   1,1,2,3,4,4,1,1,3,4,5,4,1,4,2)
found2  <- c(1,1,2,1,3,1,1,2,2,4,3,3,1,1,3,
   3,1,1,2,1,1,1,1,1,3,5,4,4,1,1)
data1 <- data.frame(expect1, expect2, found1, found2)
tableStack(vars=1:4, dataFrame=data1)

level.lab <- list("Excellent"=1, "Good"=2,
   "Fair"=3, "Poor"=4, "Very poor"=5)
for (i in 1:4) {
   data1[,i] <- factor(data1[,i])
   levels(data1[,i]) <- level.lab
}
tableStack(vars=1:4, dataFrame=data1)

data(Attitudes)
use(Attitudes)

## Please use full screen of Rconsole
## for better display of the labels.
tableStack(qa1:qa18)
tableStack(qa1:qa18, reverse=TRUE)

}
\keyword{aplot}
