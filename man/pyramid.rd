\name{pyramid}
\alias{pyramid}
\title{Population pyramid}
\description{Create a population pyramid from age and sex}
\usage{
pyramid (age, sex, binwidth=5, inputTable=NULL, printTable=FALSE, 
  percent=c("none","each","total"), decimal=3, ...)
}
\arguments{
	\item{age}{a numeric variable for age}
	\item{sex}{a variable of two levels for sexes, can be numeric but preferrably factor with labelled levels or characters}
	\item{binwidth}{bin width of age for each bar}
	\item{inputTable}{a table to read in with two columns of sexes and rows of age groups}
	\item{printTable}{whether the output table would be displayed on the console}
	\item{percent}{whether the lengths of the bars should be calculated from freqencies (default), percentages of each sex or total percentages}
	\item{decimal}{number of decimals displayed in the percent output table}
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
\value{When the variables age and sex are input arguments, the return objects include age group variable and the output table. The argument 'decimal' controls only decimals of the output displayed on the console but not the returned table.}
\examples{
data(Oswego)
.data <- Oswego
attach(.data)

# The above lines generate a hypothetical data frame. 
# In reality, one just exploits 'use("Oswego.rec"), if the file is available.

pyramid(age, sex)
pyramid(age, sex, printTable=TRUE)
pyramid(age, sex, col="red")
pyramid(age, sex, col=1:16) # Too colorful!
output <- pyramid(age, sex, binwidth = 10, percent="each", decimal=2)
title(main="Frequency of age group (years) by sex") 
output
tabpct(output$ageGroup, chocolate)

pyramid(inputTable=VADeaths[,1:2], font.lab=4)
title("Death rates per 100 in rural Virginia in 1940")
 

}
\keyword{aplot}