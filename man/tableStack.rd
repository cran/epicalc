\name{tableStack}
\alias{tableStack}
\title{Tabulation of variables in a stack form}
\description{One-way tabulate variables with the same possible range of distribution and stack into a new table with or without other descriptive statistics}
\usage{
tableStack (vars, minlevel = "auto", maxlevel = "auto", count = TRUE, 
    means = TRUE, medians = FALSE, sds = TRUE, decimal = 3, dataFrame = .data, 
    total = TRUE, vars.to.reverse = NULL, var.labels = TRUE, 
    reverse = FALSE, by = NULL, chisqTest=TRUE)
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
	\item{total}{availability of means and sd of total and mean scores}
	\item{vars.to.reverse}{variable(s) to reverse}
	\item{var.labels}{presence of descriptions of variables on the last column of output}
	\item{reverse}{whether item(s) negatively correlated with other majority will be reversed}
  \item{by}{a variable for column breakdown}
  \item{chisqTest}{whether P values of chi-squared test should be computed}
}
\details{This function simultaneously explores several variables with a fixed integer rating scale. For non-factor variables, default values for tabulation are 1 to 5 but can be specified by user.

The classes of the variables can be 'integer', 'factor' or 'logical but not any mixture.

Unlike, 'alpha', the argument 'reverse' default value is FALSE. This argument is overwritten by 'vars.to.reverse'.

Options for 'reverse', 'vars.to.reverse' and statistics of 'means', 'medians', 'sds' and 'total' are available only if the items are integer. To obtain statistics of factor items, user need to use 'unclassDataframe' to turn them into integer.

On a variable is specified to 'by' argument, all the output gives only cross tabulations between each variables of 'vars' and the 'by' variable. Unless switched to 'FALSE', P value from chi-squared test of each table is displayed without warning of expected sample size for each cell where user must be careful.

} 
\value{a list of elements of the output results.
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
tableStack(bakedham:fruitsalad, by= ill)

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

level.lab <- list("Very poor"=1, "poor"=2,
   "Fair"=3, "good"=4, "Very good"=5)
for (i in 1:4) {
   data1[,i] <- factor(data1[,i])
   levels(data1[,i]) <- level.lab
}
rm(expect1, expect2, found1, found2, level.lab)
use(data1)
tableStack(vars=1:4)
unclassDataframe(vars=1:4)
tableStack(vars=1:4) -> output
output


data(Attitudes)
use(Attitudes)

## Please use full screen of Rconsole
## for better display of the labels.
tableStack(qa1:qa18)
tableStack(qa1:qa18, reverse=TRUE)

}
\keyword{aplot}
