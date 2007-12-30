\name{tableStack}
\alias{tableStack}
\title{Tabulation of variables in a stack form}
\description{To tabulate variables with the same possible range of distribution and stack into a new table with or without other descriptive statistics or to breakdown distribution of more than one row variables against a column variable}
\usage{
tableStack (vars, minlevel = "auto", maxlevel = "auto", count = TRUE, 
    means = TRUE, medians = FALSE, sds = TRUE, decimal = 1, dataFrame = .data, 
    total = TRUE, vars.to.reverse = NULL, var.labels = TRUE, var.labels.trunc =150,
    reverse = FALSE, by = NULL, vars.to.factor = NULL, iqr = "auto", 
    prevalence = FALSE, percent = c("column", "row", "none"), 
    test = TRUE, name.test = TRUE, total.column = FALSE) 
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
	\item{var.labels.trunc}{number of characters used for variable description}
	\item{reverse}{whether item(s) negatively correlated with other majority will be reversed}
  \item{by}{a variable for column breakdown. If a single character (with quotes) is given, only the 'total column' will be displayed}
  \item{vars.to.factor}{variable(s) to be converted to factor for tabulaton}
  \item{iqr}{variable(s) to display median and inter-quartile range}
  \item{prevalence}{for logical variable, whether prevalence of the dichotomous row variable in each column subgroup will be displayed}
  \item{percent}{type of percentage displayed when the variable is categorical. Default is column}
  \item{test}{whether statistical test(s) will be computed}
  \item{name.test}{display name of the test and relevant degrees of freedom}
  \item{total.column}{whether to add 'total column' to the output or not}
}
\details{This function simultaneously explores several variables with a fixed integer rating scale. For non-factor variables, default values for tabulation are the mininum and the maximum of all variables but can be specified by user.

The classes of the variables can be 'integer', 'factor' or 'logical but should not be any mixture of these.

Unlike 'alpha', the argument 'reverse' has a default value of FALSE. This argument is overwritten by 'vars.to.reverse'.

Options for 'reverse', 'vars.to.reverse' and statistics of 'means', 'medians', 'sds' and 'total' are available only if the items are integer. To obtain statistics of factor items, users need to use 'unclassDataframe' to convert them into integer.

When the 'by' argument is given, 'reverse' and 'vars.to.reverse' do not apply. Instead, columns of the 'by' variable will be formed. A table will be created against each selected variable. If the variable is a factor or coerced to factor with 'vars.to.factor=', cross-tabulation will result with percent as specified ie. "column", "row", or "none" (FALSE). For a dichotomous row variable, if set to 'TRUE', the prevalence of row variale in the form of a fraction is displayed in each subgroup column. For continuous variables, means with S.D. will be displayed. For variables with residuals that are not normally distributed or where the variance of subgroups are significantly not normally distributed (using a significant level of 0.01), medians and inter-quartile ranges will be presented if the argument 'iqr' is set as "auto" (by default). Users may specify a subset of the selected variables (from the 'vars' argument) to be presented in such a form. Otherwise, the argument could be set as any other character string such as "none" or any others, to insist to present means and S.D. 

When 'test = TRUE' (default), Pearson's chi-squared test (or a two-sided Fisher's exact test, if the sample size is small) will be carried out for a categorical variable or a factor. The two-sample t-test (or ANOVA F-test, when there are more than 2 levels of 'by') will be computed for a numeric variable. If the numeric variable is included in the 'iqr' argument, (manually or automatically), Wilcoxson's ranksum test or Kruskal-Wallis test will be performed instead.

When 'by' is specified as a single character object (such as 'by="none"'), there will be no breakdown and all tests will be omitted. Only the 'total' column is shown.

If this 'total column' is to accompany the 'by' breakdown, the argument 'total.column=TRUE' should be specified.

By default, Epicalc sets 'var.labels=TRUE' in order to give nice output. However, 'var.labels=FALSE' can sometimes be more useful during data exploration. Variable numbers as well as variable names are displayed instead of variable labels. Names and numbers of abnormally distributed variables, especially factors with too many levels can be quickly picked up for further relevelling or recoding.
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
tableStack(bakedham:fruitsalad, by= ill, prevalence=TRUE)
tableStack(bakedham:fruitsalad, by= ill, percent=FALSE)
tableStack(bakedham:fruitsalad, by= ill, percent=FALSE, name.test=FALSE)

data(Cars93, package="MASS")
use(Cars93)
des()
tableStack(vars=4:25, by=Origin)
tableStack(vars=4:25, by="none")
tableStack(vars=4:25, by=Origin, total.column=TRUE)
tableStack(vars=4:25, by=Origin, total.column=TRUE, test=FALSE)


data(Attitudes)
use(Attitudes)
tableStack(qa1:qa18)  # Needs full screen of Rconsole
tableStack(qa1:qa18, var.labels.trunc=35) 
                      # Fits in with default R console screen
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
tableStack(vars=3:4, by=outc, prevalence = TRUE)
tableStack(vars=3:4, by=outc, name.test = FALSE)

## Beware of small sample sizes in the last command}
\keyword{aplot}
