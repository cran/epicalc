\name{cc}
\alias{cc}
\alias{cci}
\alias{cs}
\alias{csi}
\alias{make2x2}
\alias{graph.casecontrol}
\alias{graph.prospective}
\alias{labelTable}
\title{Odds ratio calculation and graphing}
\description{Odds ratio calculation and graphing}
\usage{
cc(outcome, exposure, decimal = 2, cctable = NULL, graph = TRUE, 
	original = TRUE, design = "cohort", main, xlab = "auto", ylab) 
cci(caseexp, controlex, casenonex, controlnonex, cctable = NULL, 
	decimal = 2, graph = TRUE, design = "cohort", main, xlab, ylab,
  xaxis, yaxis) 
cs(outcome, exposure, cctable = NULL, decimal = 2, method="Newcombe.Wilson", 
   main, xlab, ylab, cex, cex.axis)
csi(caseexp, controlex, casenonex, controlnonex, cctable = NULL, 
	decimal = 2, method="Newcombe.Wilson") 
graph.casecontrol(caseexp, controlex, casenonex, controlnonex,
	 decimal=2) 
graph.prospective(caseexp, controlex, casenonex, controlnonex,
	 decimal=2) 
labelTable(outcome, exposure, cctable = NULL, cctable.dimnames = NULL) 
make2x2(caseexp, controlex, casenonex, controlnonex)
}
\arguments{
	\item{cctable.dimnames}{Dimension names of the variables, usually omitted}
	\item{decimal}{number of decimal places displayed}
	\item{outcome, exposure}{two dichotomous variables}
	\item{cctable}{A 2-by-2 table. If specified, will supercede the outcome and exposure variables}
	\item{graph}{If TRUE (default), produces an odds ratio plot}
	\item{design}{Specification for graph; can be "case control","case-control", "cohort" or "prospective"}
	\item{caseexp}{Number of cases exposed}
	\item{controlex}{Number of controls exposed}
	\item{casenonex}{Number of cases not exosed}
	\item{controlnonex}{Number of controls not exposed}
	\item{original}{should the original table be displayed instead of standard outcome vs exposure table}
	\item{main}{main title of the graph}
	\item{xlab}{label on X axis}
	\item{ylab}{label on Y axis}
	\item{xaxis}{two categories of exposure in graph}
	\item{yaxis}{two categories of outcome in graph}
  \item{method}{method of computation for 95 percent limits of risk difference}
  \item{cex.axis}{character expansion factor for graph axis}
  \item{cex}{character expansion factor for text in the graph}
}
\details{'cc' computes odds ratios and 95 percent confidence intervals from outcome and exposure variables. The results are based on the exact method.

'cci' is a variant of 'cc' where four numbers are entered manually.  

'cs' is for cohort and cross-sectional studies. It computes the absolute risk, risk difference, and risk ratio. When the exposure is a risk factor, the attributable fraction exposure, attributable fraction population and number needed to harm (NNH) are also displayed in the output. When the exposure is a protective factor, protective efficacy or percent of risk reduced and number needed to treat (NNT) are displayed instead. 

If there are more than 2 exposure categories and the sample size is large enough, a graph will be plotted.

'csi' is a variant of 'cs' where four numbers are entered manually.

'method' chooses whether 95 percent ci of risk difference should be computed by Newcomb-Wilson method. Both this and the standard method may give non-sensible values if the risk difference is not statistically significant.

'make2x2' creates a 2-by-2 table using the above orientation.

'graph.casecontrol' and 'graph.prospective' draw a graph comparing the odds of exposure between cases and controls or odds of diseased between exposed and non-exposed.

These two graphic commands are automatically called by 'cc' and 'cci'.

Alternatively, a table saved from 'make2x2' can be supplied as the 'cctable' argument for the 'cc' command. The squares on the values of each group (case vs control and exposed vs unexposed) represent the relative sample sizes for each group.

}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'fisher.test', 'chisq.test' and 'mhor'}
\examples{
data(Oswego)
use(Oswego)
cc(ill, chocolate)
cc(ill, chocolate, design="case-control")
cs(ill, chocolate) # The outcome variable should come first.

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
table1 <- make2x2(25,22,20,7)
cc(outcome=NULL, exposure=NULL, cctable=table1)
cs(outcome=NULL, exposure=NULL, cctable=table1)
agegr <- pyramid(age, sex, bin=30)$ageGroup
cs(ill, agegr, main="Risk ratio by age group", xlab="Age (years)")
}
\keyword{array}
