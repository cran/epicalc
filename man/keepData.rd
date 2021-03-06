\name{Keep data}
\alias{keepData}
\title{Keep a subset of variables or records}
\description{Keep only subset of variables or records in the default data frame '.data'}
\usage{                                       
keepData (dataFrame = .data, sample=NULL, exclude=NULL, subset, select, 
		 drop = FALSE, refactor = c("subset.vars", "all", "none"), ...)  
}
\arguments{
	\item{dataFrame}{a data frame}
	\item{sample}{an integer indicating the size of random sample or a value < 1 indicating fraction of records to be extracted}
	\item{exclude}{an expression, indicating columns to remove from '.data'.}
	\item{subset}{a logical expression indicating elements or rows to keep: missing values are taken as false.}
	\item{select}{an expression indicating columns to select from a data frame.} 
	\item{drop}{passed on to [ indexing operator.} 
	\item{refactor}{after subsetting, whether the levels of variable(s) with zero count should be removed}
	\item{...}{further arguments to be passed to or from other methods.} 
}
\details{'keepData' is the Epicalc version of 'subset.data.frame' which is a standard R function. It reduces the data frame to the specified subset and updates the search path accordingly.

Using 'keepData' will retain descriptions of the data, and the remaining variables, ready to be used by other Epicalc functions that can exploit them such as 'des', 'codebook', 'summ', 'tab1' etc ...

Since this command only affects the specified data frame (usually '.data'), any new variables created as free vectors will not be changed. The difference in length of variables may occur from the 'subset' argument. To avoid this, 'pack' or 'label.var' should be used to incoporate any relevant free vectors into the default data frame, '.data' so that all variables can be subsetted simultaneously, thus reducing the complications of the difference in variable lengths.

The argument 'refactor' is effective only when the argument 'subset' is specified. By default, 'refactor' is set to "subset.vars" indicating that the levels of the variables used in subset criteria will be revised to eliminate levels with zero count. If refactor="all", all factor variables in the dataFrame will be affected.
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'des', 'subset', 'sample'}
\examples{

## Record sampling
data(ANCdata)
use(ANCdata)
des()
keepData(sample=500)
des() # Note reduction of sample size to 500
use(ANCdata)
keepData(sample=.1) # Select 10% or 75 records
des()

## Selecting specific record numbers
data(Compaq)
use(Compaq)
keepData(subset = 1:nrow(.data) <= 50) #First 50 records
summ()
use(Compaq)
every.seventh <- is.element(1:nrow(.data), seq(1, nrow(.data), 7))
keepData(subset = every.seventh) 
head(.data)

## Selecting records under certain conditions
data(Familydata)
use(Familydata)
des()
.data
bmi <- wt/(ht/100)^2
label.var(bmi, "Body mass index (kg/m2)")
keepData(subset = ht > 120)
.data # Which record is missing?

use(Compaq)
des()
tab1(ses)
ses1 <- ses 
pack()
keepData(subset=ses=="Rich")
tab1(ses)
tab1(ses1) # 'refactor' set to 'subset.vars' thus levels of ses1 not affected

## Reduction of variables
## Removal of consecutive variables
use(Familydata)
keepData(select = -(age:ht)) # Variables from 'age' to 'ht' removed
des() 
## A better alternative would be:
use(Familydata)
keepData(exclude = age:ht) 
des() 
keepData(select = -c(1,3)) # Further removal of the first and 
			               # the third variables 
des()
codebook()
## Targeting only a certain variables
data(Oswego)
use(Oswego)
des()
keepData(select = c(age, sex, ill, cakes:fruitsalad))
des() 
keepData(select = c(1,2,5:7)) # Retain all variables except the third 
	                          #the the fourth
des()
# Note the repetition of '(subset)'



## Wildcard
use(Oswego)
des()
keepData(select = "c*") # The wildcard must be enclosed in quotes
des()

use(Oswego)
des()
keepData(exclude = "on*") # Variables having names starting with "on" removed
keepData(exclude = "???") # Variables having names with 3 characters removed
des() # Which are missing?

}
\keyword{database}