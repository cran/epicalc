\name{use}
\alias{use}
\title{Quick command to read in data}
\description{Quick command to read in data in Stata, SPSS, EpiInfo and .csv formats in addition to }
\usage{use(filename, clear = TRUE)
}
\details{'use' reads in datasets from Dbase (.dbf), Stata (.dta), SPSS(.sav), EpiInfo(.rec) and Comma separated value(.csv) formats as well as those come with 'pakage:datasets' in R. The data frame is saved in memory as '.data' and automatically attached to the search path. This setting is the basis for other commands of 'epicalc' including 'des', 'summ', 'recode', 'labelVar' etc. 

The next 'use' command replaces the default data frame '.data' with the new one. If required it should be copied to a new data frame object for further use or 'save'd into an R image file. }
\arguments{
	\item{filename}{a character object ended with one of the following: .dbf, .dta, .sav, .rec, .csv (file withh comma and header); data set in R package requires no quote}
	\item{clear}{equal to 'detachAllData()' before reading in the data set and attaching it to the search path}
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'read.table', 'read.dta', 'read.SPSS', etc and 'detachAllData'}
\examples{
# data(BOD)
library(foreign)
write.dta(BOD, file="BOD.dta")
rm(list=ls())
ls()
use("BOD.dta", clear=FALSE)

# The above lines write Stata format from R data frame. 
# In reality, one just exploits 'use("filename.dta")', if the file is available.
des()
file.remove("BOD.dta")

# A better way to read R dataset for exploration with Epicalc is
 use(BOD, clear=FALSE)
 des()
 summ()
}
\keyword{database}
