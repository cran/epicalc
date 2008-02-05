\name{use}
\alias{use}
\title{Command to read in and attach data}
\description{Command to read in data from Stata, SPSS, EpiInfo and .csv formats in addition to any R data frame }
\usage{use(filename, dataFrame = .data, clear = TRUE, tolower = TRUE)
}
\details{'use' reads in datasets from Dbase (.dbf), Stata (.dta), SPSS(.sav), EpiInfo(.rec) and Comma separated value (.csv) formats as well as R data frames. The destination data frame is saved in memory, by default as '.data', and automatically attached to the search path. This setting is the basis for other commands of 'epicalc' including 'des', 'summ', 'recode', 'label.var' etc. 

The 'use' command overwrites the destination data frame ('.data') with the new one.}
\arguments{
	\item{filename}{a character object ending with one of the following: .dbf, .dta, .sav, .rec, .csv (file with comma and header); data frames in R requires no quote}
	\item{dataFrame}{destination data frame where the read object is store}
	\item{clear}{equal to 'detachAllData()' before reading in the data set and attaching it to the search path}
	\item{tolower}{whether all the names of the variables should be forced to lower case (only if the original file has one the following extensions: '.dbf', 'rec' and '.sav')}
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
# In reality, one just types 'use("filename.dta")', if the file is available.
des()
file.remove("BOD.dta")

# A better way to read an R dataset for exploration with Epicalc is
 use(BOD, clear=FALSE)
 des()
 summ()
}
\keyword{database}
