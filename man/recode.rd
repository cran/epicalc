\name{recode}
\alias{recode}
\alias{recode.default}
\alias{recode.is.na}
\title{Recode variable(s)}
\description{Change value(s) of variable(s) in the default data frame}
\usage{
recode(vars, ...)

\method{recode}{default}(vars, old.value, new.value, dataFrame = .data, ...)  

\method{recode}{is.na}(vars, new.value = 0, dataFrame = .data, ...)
}
\arguments{
	\item{vars}{a variable or variables with the same recoding scheme}
	\item{old.value}{original values or a condition}
	\item{new.value}{new values for all variables listed}
  \item{dataFrame}{a data frame}
  \item{...}{further arguments passed to or used by other methods.}
}
\details{'recode' is very useful for recoding missing values but can also be used for other purposes.

'vars' can be a single variable or a list of variables in the format of list(var1, var2, var3) or c(var1, var2, var3), which will be recoded simultaneously under the same scheme.

Both 'old.value' and 'new.value' can be vectors of equal length. The elements of old.value and new.value will be matched by corresponding orders. However, 'new.value' can have a single element into which all the old values are recoded.

The argument 'old.value' can be also be a condition for recoding the 'vars' into the single new.value regardless of the old value.

Note that changing the value label of a variable's levels can be done with 'levels(var)[levels(var)=="old name"] <- "new name"'. However, Epicalc 'recode' is more efficient in changing several factors using the same scheme. See example. 

All the 'recode'd vars are automatically 'pack'ed into the default data frame which is synchronize with the one in the search path.

'recode.is.na' is used to recode any missing value of one or more variable to a common 'new.value', which is zero by default.
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'replace', 'lookup'}
\examples{
age       <- c( 37,  99,  24,  33,  31,  30,  26,  25) 
systolic  <- c(120, 120, 110, 120, 130, 120, 888, 999) 
diastolic <- c( 80,  80,  70,  80,  70, 999,  70,  60)
sick      <- c(  1,   2,   2,   1,   2,   2,   2,   NA)
treated   <- c(  2,   1,   2,   2,   1,   2,   2,   1)
yesno     <- c("Y", "N")
sick      <- factor(sick, labels=yesno)
treated   <- factor(treated, labels=yesno)
.data     <- data.frame(age, systolic, diastolic, sick, treated)
use(.data)
pack()
# 'pack() integate all variables into .data
# to avoid confusion with free vectors.


# The above lines generate a hypothetical data frame. 
# In reality, one just exploits 'use("datafile")', if the "datafile" exists.
.data
summ()
recode(age, old.value=99, new.value=NA)
summ()
recode(vars=c(systolic, diastolic), 999, NA) # The value 888 is not recoded.
summ()
recode(systolic, systolic > 250, NA)
summ()
table(sick, treated)
recode(vars=c(sick, treated), old.value="Y", new.value="yes")
table(sick, treated)

# Recode both sick and treated to "N" if sick status is missing. 
recode(vars=c(sick,treated), is.na(sick), new.value="N") 
table(sick, treated) 

# Recode more than one old values
data(VCT)
use(VCT)
des()
table(A16); table(A17); table(A18)
recode(vars=A16:A18,  c("willing","willing if have money"), "willing")
table(A16); table(A17); table(A18)
# Recode two last categories to missing
recode(A16:A18, c("not relevant","not answer"), NA)
table(A16); table(A17); table(A18)

# Use 'recode.is.na' to recode NA to "missing data"
recode.is.na(vars=A16:A18, "missing data")
table(A16); table(A17); table(A18)
recode(vars=A4:A5, 999, NA)
summ()
# recode back from NA to 0
recode.is.na(vars=A4:A5) # Note that new value is 0 by default

# Swaping
data(Hakimi)
use(Hakimi)
des()
summ()
table(treatment)
recode(treatment, c(1,2), c(2,1))
table(treatment)

}
\keyword{database}