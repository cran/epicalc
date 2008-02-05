\name{matchTab}
\alias{matchTab}
\title{Matched tabulation}
\description{Tabulation of outcome vs exposure from a matched case control study}
\usage{
matchTab (case, exposed, strata)
}
\arguments{
	\item{case}{Outcome variables where 0 = control and 1 = case}
	\item{exposed}{Exposure variable where 0 = non-exposed and 1 = exposed}
	\item{strata}{Identification number for each matched set}
}
\details{Tabulation for an unmatched case control study is based on individual records classified by outcome and exposure variables.

Matched tabulation is tallying based on each matched set. The simplest form is McNemar's table where only one case is matched with one control. 'matchTab' can handle 1:m matching where m can vary from 1 to m. A MLE method is then used to compute the conditional odds ratio. 
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'table', 'cc' and 'clogit'}
\examples{
use(infert)

## Not run:

# matchTab(case, induced, stratum)
# Tabulation successful but OR not computed
# because 'induced' is not binary

## End(Not run)

ia <- induced > 0
matchTab(case, ia, stratum)

# See also
library(survival)
clogit(case ~ ia + strata(stratum), data=infert)

}
\keyword{array}
