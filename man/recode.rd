\name{recode}
\alias{recode}
\title{Recode variable(s)}
\description{Change value(s) of variable(s) in the default data frame}
\usage{
recode (vars, old.value, new.value, dataFrame = .data)  
}
\arguments{
	\item{vars}{a variable or variables with the same recoding scheme}
	\item{old.value}{original value or condition}
	\item{new.value}{new value for all variables listed}
  \item{dataFrame}{a data frame}
}
\details{'recode' is very useful for recoding missing values but can also be used for other purposes.

'vars' can be a single variable or a list of variables in the format of list(var1, var2, var3) or c(var1, var2, var3), which will be recoded simultaneously under the same scheme.

The argument 'old.value' can be the original value or a condition for recoding regardless of the old value.

Changing the value label of a variable's levels can be done with 'levels(var)[levels(var)=="old name"] <- "new name"'. In this case, 'recode' changes several factors using the same scheme. 
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
}
\keyword{database}