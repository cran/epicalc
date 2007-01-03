\name{cc}
\alias{cc}
\alias{cci}
\alias{cs}
\alias{csi}
\alias{mhor}
\alias{make2x2}
\alias{graph.casecontrol}
\alias{graph.prospective}
\alias{labelTable}
\title{Odds ratio calculation and graphing}
\description{Odds ratio calculation and graphing}
\usage{
cc(outcome, exposure, decimal = 2, cctable = NULL, graph = TRUE, design = "cohort") 
cci(caseexp, controlex, casenonex, controlnonex, cctable = NULL, decimal = 2, graph = TRUE, design = "cohort") 
cs(outcome, exposure, cctable = NULL, decimal = 2)
csi(caseexp, controlex, casenonex, controlnonex, cctable = NULL, decimal = 2) 
graph.casecontrol(caseexp, controlex, casenonex, controlnonex, decimal=2) 
graph.prospective(caseexp, controlex, casenonex, controlnonex, decimal=2) 
labelTable(outcome, exposure, cctable = NULL, cctable.dimnames = NULL) 
mhor(..., mhtable = NULL, decimal=2, graph = TRUE, design = "cohort") 
make2x2(caseexp, controlex, casenonex, controlnonex)
}
\arguments{
	\item{...}{Variables. Three for 'mhor'.}
	\item{cctable.dimnames}{Dimension names of the variables, usually omitted}
	\item{decimal}{number of decimal places displayed}
	\item{outcome, exposure}{two dichotomous variables}
	\item{cctable}{A 2-by-2 table. If specified, will supercede the outcome and exposure variables}
	\item{graph}{If TRUE (default), produces an odds ratio plot}
	\item{design}{Specification for graph; can be "case control","case-control", "cohort" or "prospective"}
	\item{mhtable}{a 2-by-2-by-s table, where s (strata) is more than one}
	\item{caseexp}{Number of cases exposed}
	\item{controlex}{Number of controls exposed}
	\item{casenonex}{Number of cases not exosed}
	\item{controlnonex}{Number of controls not exposed}
}
\details{'cc' and 'cci' compute odds ratios and 95 percent confidence intervals from a dataset or from four numbers entered manually. The results are based on the exact method.

'cs' and 'csi' are for cohort and cross-sectional studies. They compute absolute risks, risk difference, risk ratio. When the exposure is a risk factor, additional results are attributable fraction exposure and attributable fraction population. When the exposure is a protective factor, protective efficacy and number needed to treat (NNT) are given instead. 

'make2x2' creates a 2-by-2 table using the above orientation.

'graph.casecontrol' and 'graph.prospective' draw a graph comparison of odds of exposure between cases and controls or odds of diseased between exposed and non-exposed.

These two graphic commands are automatically called by 'cc' and 'cci'.

Alternatively, a table saved from 'make2x2' can be supplied as the 'cctable' argument for the 'cc' command. The squares on the values of each group (case vs control and exposed vs unexposed) represent the relative sample sizes for each group.

'mhor' computes stratum-specific odds ratios and 95 percent confidence intervals and the Mantel-Hanzsel odds ratio and chi-squared test is given as well as the homogeneity test. A stratified odds ratio graph is displayed. 
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'fisher.test', 'chisq.test' and 'mhor'}
\examples{
data(Oswego)
.data <- Oswego
attach(.data)

# The above lines generate a hypothetical data frame. 
# In reality, one just exploits 'use("Oswego.rec"), if the file is available.
cc(ill, chocolate)
cc(ill, chocolate, design="case-control")
cs(ill, chocolate) # The outcome variable should come before the exposure.
mhor(ill, chocolate, sex)

mht1 <- table(ill, chocolate, sex)
dim(mht1)
mhor(mhtable=mht1) # same results

#    For the following table
#          chocolate
#    ill     FALSE TRUE
#     FALSE     7   22
#     TRUE     20   25
#
cci(25, 22, 20, 7)
graph.casecontrol(25, 22, 20, 7)
graph.prospective(25, 22, 20, 7)

#Alternatively
table1 <- make2x2(25,70,22,7)
cc(outcome=NULL, exposure=NULL, cctable=table1)
cs(outcome=NULL, exposure=NULL, cctable=table1)

}
\keyword{array}
