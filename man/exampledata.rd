\name{Data for cleaning}
\docType{data}
\alias{exampledata}
\title{Dataset for practicing cleaning, labelling and recoding}
\description{
The data come from clients of a family planning clinic.

id = ID code                

age = age in years

relig = religion   code: 1 = 'Buddhist', 2 = 'Islam' 

ped   = patient's eduction  code: 1='no education', 2='primary school', 
3='secondary school', 4='high school', 5='vocational school', 
6='bachelor degree',  7='other'.

income    monthly income in Thai baht 
code: 1='Nil',  2='<1,000', 3='1,000-4,999',  4='5,000-9,999', 6='10,000+'

am=age(yr) at 1st marriage 

reason = reason for family planning. code: 1='Birth spacing', 2='Enough children',
3='Others'

bps = systolic BP          

bpd = diastolic BP         

wt  = weight (kg)          

ht  = height (cm)          

For all variables except id: 9, 99, 99.9, 888, 999 are missing values
}
\usage{data(exampledata)}
\format{A data frame containing 251 observations and 11 variables.}
\examples{
data(exampledata)
des(exampledata)
# Change var. name to lowercase
names(exampledata) <- tolower(names(exampledata)) 
use(exampledata)
des()
# Check for duplication of 'id'
table(id)
names(table(id))[table(id) > 1]
}
\keyword{datasets}
    