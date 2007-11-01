\name{des}
\alias{des}
\title{Desription of a data frame or a variable}
\description{Description of a data frame or a variable or wildcard for variable names}
\usage{
des(x=.data, select, exclude)
}
\arguments{
	\item{x}{an object such as a vector (variable), a matrix, a table, a list or a data frame}
	\item{select}{expression, indicating columns to select from '.data.'}
	\item{exclude}{expression, indicating columns to exclude}
}
\details{The default value of x (ie if no argument is supplied) is '.data'. If 'x' is a data frame, its variable names will be listed with class and the description of each variable. 

If 'x' is a variable, the environment and attached data frame containing 'x' will be described.

For a data frame containing too many variables, 'select' and 'exclude' can be specified to display fewer variable descriptions at a time. Unlike 'keepVar', these two arguments do not have any permanent effect on the data frame.}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'use', 'summ', 'label.var', 'subset' and 'keepData'}
\examples{
data(Oswego)
use(Oswego)
# In the tutorial, when "oswego.rec" which is an EpiInfo file is available,
# instead of typing the above two lines, one can directly type:
# use("oswego.rec")

des() # This is one of the most useful Epicalc functions!

#### Detection of variables of the same name in different data frames.
# Note that 'age' is a variable in '.data' due to the function 'use'.
des(Oswego) # Same results. Note that 'age' is also in 'Oswego'.
des(infert) # The third 'age' is in another data frame,
	        # from the datasets package in R, 'infert'.
attach(infert)
search() # Show all data frames that are in the search path
des(sex) # 'sex' is found only in '.data'
des(induced)
age <- "abc" # Just a silly example for a variable
des(age)     # Shows all occurrences of 'age', wherever it is
rm(age)
detachAllData()

#### Wildcard for variables
use(Oswego)
des("c*")     # Show all variables starting with 'c'
des("?????")  # Show all variables with 5 characters in the name

agegr <- cut(age, breaks=c(0,20,40,60,80))
label.var(agegr, "age group")
# Note that the above line incoperates 'agegr' into '.data
# making it eligible to be included in the group under the following wildcard
des("age*") 

#### Subset of variables in .data
des(select = 1:5) # First five variables
des(select = age:onsetdate) # Same results

des(select = c(1,2,5,20))
des(select = c(age, sex, onsetdate, fruitsalad))

des(select = sex:chocolate)

## The following six lines give the same results
des(select = -(sex:chocolate))
des(select = -sex:-chocolate) 
des(select = -(2:19))
des(select = -19:-2)
des(exclude = sex:chocolate)
des(exclude = 2:19)

#### Wildcard: same effects with or without 'select'
des(select = "c*")
des("c*")

## Exclusion using wildcard, however, needs an 'exclude' argument.
des(exclude = "c*")
}
\keyword{database}
