\name{pyramid}
\alias{pyramid}
\title{Population pyramid}
\description{Create a population pyramid from age and sex}
\usage{
pyramid (age, sex, binwidth=5, age.sex.table=NULL, output.table=FALSE, 
  percent=c("none","each","total"), ...)
}
\arguments{
	\item{age}{a numeric variable for age}
	\item{sex}{a variable of two levels for sexes, can be numeric but preferrably factor with labelled levels or characters}
	\item{binwidth}{bin width of age for each bar}
	\item{age.sex.table}{a table to read in with two columns of sexes and rows of age groups}
	\item{output.table}{output table from tabulation of age and sex}
	\item{percent}{whether the lengths of the bars should be calculated from freqencies (default), percentages of each sex or total percentages}
	\item{...}{graph options for the bars e.g. col}
}
\details{'pyramid' draws a horizontal bar graph of age by sex. 

Parameters of graph (par) options can be applied to 'font.lab' and those of the bars e.g. 'col' but not of others.

Other lower level graph commands should be only for adding a 'title'.
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'barplot', 'levels', 'table'}
\examples{
data(oswego)
.data <- oswego
attach(.data)

# The above lines generate a hypothetical data frame. 
# In reality, one just exploits 'use("oswego.rec"), if the file is available.

pyramid(age, sex)
pyramid(age, sex, col="red")
pyramid(age, sex, col=1:16) # Too colorful!
pyramid(age, sex, output.table=TRUE)
pyramid(age, sex, binwidth = 10, output.table=TRUE, percent="each")
title(main="Frequency of age group (years) by sex") 

pyramid(age.sex.table=VADeaths[,1:2], font.lab=4)
title("Death rates per 100 in rural Virginia in 1940")
 

}
\keyword{aplot}