\name{tableStack}
\alias{tableStack}
\title{Tabulation of variables in a stack form}
\description{One-way tabulate variables with the same possible range of distribution and stack into a new table with or without other descriptive statistics}
\usage{
tableStack (vars, minlevel = "auto", maxlevel = "auto", count = TRUE, 
    means = TRUE, medians = FALSE, sds = TRUE, decimal = 1, dataFrame = .data, 
    total = TRUE, vars.to.reverse = NULL, var.labels = TRUE, 
    reverse = FALSE, by = NULL, vars.to.factor = NULL, iqr = "auto", 
    prevalence = TRUE, percent = c("column", "row", "none"), 
    test = TRUE, name.test = TRUE) 
}
\arguments{
	\item{vars}{a vector of variables in the data frame}
	\item{minlevel}{possible minimum value of items specified by user}
	\item{maxlevel}{possible maximum value of items specified by user}
	\item{count}{whether number of valid records for each item will be displayed}
	\item{means}{whether means of all selected items will be displayed}
	\item{medians}{whether medians of all selected items will be displayed}
	\item{sds}{whether standard deviations of all selected items will be displayed}
	\item{decimal}{number of decimals displayed in the statistics}
	\item{dataFrame}{source data frame of the variables}
	\item{total}{availability of means and sd of total and mean scores}
	\item{vars.to.reverse}{variable(s) to reverse}
	\item{var.labels}{presence of descriptions of variables on the last column of output}
	\item{reverse}{whether item(s) negatively correlated with other majority will be reversed}
  \item{by}{a variable for column breakdown}
  \item{vars.to.factor}{variable(s) to be converted to factor for tabulaton}
  \item{iqr}{variable(s) to display median and inter-quartile range}
  \item{prevalence}{for logical variable, whether prevalence in each subgroup will be displayed}
  \item{percent}{type of percentage displayed when the variable is categorical. Default is column}
  \item{test}{whether statistical test(s) will be computed}
  \item{name.test}{display name of the test and relevant degrees of freedom}
}
\details{This function simultaneously explores several variables with a fixed integer rating scale. For non-factor variables, default values for tabulation are the mininum and the maximum of all variables but can be specified by user.

The classes of the variables can be 'integer', 'factor' or 'logical but should not any mixture.

Unlike, 'alpha', the argument 'reverse' default value is FALSE. This argument is overwritten by 'vars.to.reverse'.

Options for 'reverse', 'vars.to.reverse' and statistics of 'means', 'medians', 'sds' and 'total' are available only if the items are integer. To obtain statistics of factor items, users need to use 'unclassDataframe' to turn them into integer.

When the 'by' argument is given, 'reverse' and 'vars.to.reverse' do not apply. Instead, columns of the 'by' variable will be formed. A table will be created against each selected variable. If the variable is a factor or coerced to factor with 'vars.to.factor=', cross-tabulation will result with percent as specified ie. "column", "row", or "none" (or FALSE). For a dichotomous variable, prevalence in form of a fraction and percentage is the default form of presentation but can be set to 'FALSE'. For continuous variables, means with S.D. will be displayed. For a variables the residuals of which are not normally distributed or the variance of subgroups are significantly not normally distributed (using a significant level of 0.01), median and inter-quartile range will be presented if the argument 'iqr' is set as "auto" (by default). User may specify a subset of the selected variables (of 'vars' argument) to be presented in such a form. Otherwise, the argument could be set as other character string such as "none" or any others, to insist to use mean(S.D.) presentation. 

When 'test = TRUE' (default), chi-squared test (or two-sided Fisher's exact test, if sample size is small) will be carried out for a categorical variable or a factor. Two-sample t-test (or ANOVA F-test, when there are more than 2 levels of 'by') will be computed for a numeric variable. If the numeric variable is included in 'iqr' argument, (manually or automatically), Wilcoxson's ranksum test or Kruskal-Wallis test will be performed instead.

} 
\value{a list of elements of the output results, when 'by = NULL', otherwise, a table
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
tableStack(bakedham:fruitsalad, by= ill, prevalence=FALSE)
tableStack(bakedham:fruitsalad, by= ill, prevalence=FALSE, percent=FALSE)
tableStack(bakedham:fruitsalad, by= ill, prevalence=FALSE, 
  percent=FALSE, name.test=FALSE)

data(Cars93, package="MASS")
use(Cars93)
des()
tableStack(vars=4:25, by=Origin)

data(Attitudes)
use(Attitudes)
## Please use full screen of Rconsole
## for better display of the labels.
tableStack(qa1:qa18)
tableStack(qa1:qa18, reverse=TRUE) -> a
a
## Components of 'a' have appropriate items reversed
a$mean.score -> mean.score 
a$total.score -> total.score
pack()
tableStack(c(qa1,qa13:qa18,mean.score,total.score), by=sex, test=FALSE)
tableStack(c(qa15, qa17, mean.score:total.score), by=sex, iqr=c(qa17,total.score))
tableStack(c(qa15, qa17, mean.score:total.score), by=dep, iqr=c(qa17,total.score))
## 'vars' can be mixture of different classes of variables
highscore <- mean.score > 4
label.var(highscore, "high score")
tableStack(mean.score:highscore, by=sex, iqr=total.score)

data(Ectopic)
use(Ectopic)
des()
tableStack(vars=3:4, by=outc)
tableStack(vars=3:4, by=outc, percent="none")
tableStack(vars=3:4, by=outc, prevalence = FALSE)
tableStack(vars=3:4, by=outc, name.test = FALSE)

## Beware of small sample sizes in the last command}
\keyword{aplot}
