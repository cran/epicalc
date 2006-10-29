\name{Sampsize}
\alias{n.for.survey}
\alias{n.for.2means}
\alias{n.for.2p}
\alias{n.for.lqas}
\title{Sample size calculation}
\description{Sample size calculation for epidemiological studies}
\usage{
n.for.survey(p, delta = 0.5 * min(c(p, 1 - p)), popsize = FALSE, 
    deff = 1, alpha = 0.05) 
n.for.2means (mu1, mu2, sd1, sd2, ratio = 1, alpha = 0.05, power = 0.8) 
n.for.2p (p1, p2, alpha = 0.05, power = 0.8, ratio = 1) 
n.for.lqas (p0, q = 0, N = 10000, alpha = 0.05, exact = FALSE) 
}
\arguments{
	\item{p}{estimated prevalence}
	\item{delta}{difference between the estimated prevalence and one side of the 95 percent confidence limit}
	\item{popsize}{size of the finite population}
	\item{deff}{design effect for cluster sampling}
	\item{alpha}{significance level}
	\item{mu1, mu2}{estimated means of the two populations}
	\item{sd1, sd2}{estimated standard deviations of the two populations}
	\item{ratio}{n2/n1}
	\item{p1, p2}{estimated probabilities of the two populations}
	\item{power}{power of the study}
	\item{p0}{critical proportion beyond which the lot will be rejected}
	\item{q}{critical number of faulty pieces found in the sample, beyond which the lot will be rejected}
	\item{N}{lot size}
	\item{exact}{whether the exact probability is to be computed}
}
\details{'n.for.survey' is used to compute the sample size required to conduct a survey. 
When cluster sampling is employed, the design effect (deff) has to be taken into account.

'n.for.2means' is used to compute the sample size for testing a hypothesis of difference of two population means.

'n.for.2p'  is used to the compute the sample size for testing a hypothesis of difference of two population proportions.

For a case control study, p1 and p2 are the proportions of exposure among cases and controls.

For a cohort study, p1 and p2 are proportions of positive outcome among the exposed and non-exposed groups.

'ratio' in a case control study is controls:case. In cohort and cross-sectional studies, it is non-exposed:exposed.

LQAS stands for Lot Quality Assurance Sampling. The sample size n is determined to test whether the lot of a product has a defective proportion exceeding a critical proportion, p0. Out of the sample tested, if the number of defective specimens is greater than q, the lot is considered not acceptable. This concept can be applied to quality assurance 
processes in health care. 
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'power.for.2means', 'power.for.2p'}
\examples{
# In a standard survey to determine the coverage of immunization using a cluster sampling technique
# on the population of approximately 500000, the estimated prevalence is 70 percent, 
# design effect is assumed to be 2.

n.for.survey( p = .8, delta = .1, popsize = 500000, deff =2) # 123

# In a case control study testing the efficacy of measles vaccine. 
# The coverage in the non-diseased population is estimated at 80 percent. That in the diseased is 60 percent.

n.for.2p(p1=.8, p2=.6) # n1=n2=91

# A randomized controlled trial testing cure rate of a disease of 90 percent by new drugs 
# and 80 percent by the old one.

n.for.2p(p1=.9, p2=.8) # 219 subjects in each arm.

# A quality assurance to check whether the coding of ICD-10 is faulty by no more than 2 percent.
# The minimum sample is required. Thus any faulty coding in the sample is not acceptable.

n.for.lqas(p0 = .02, q=0, exact=TRUE) # 148 non-faulty checks is required to support the assurance process.
 
}
\keyword{math}