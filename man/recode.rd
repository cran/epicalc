\name{recode}
\alias{recode}
\title{Recode variable(s)}
\description{Change value(s) of variable(s) in the default data frame}
\usage{
recode(vars, old.value, new.value, dataFrame = .data)  
}
\arguments{
	\item{vars}{a variable or variables with the same recoding scheme}
	\item{old.value}{original values or a condition}
	\item{new.value}{new values for all variables listed}
  \item{dataFrame}{a data frame}
}
\details{'recode' is very useful for recoding missing values but can also be used for other purposes.

'vars' can be a single variable or a list of variables in the format of list(var1, var2, var3) or c(var1, var2, var3), which will be recoded simultaneously under the same scheme.

Both 'old.value' and 'new.value' can be vectors of equal length. The elements of old.value and new.value will be matched by corresponding orders. However, 'new.value' can have a single element into which all the old values are recoded.

The argument 'old.value' can be also be a condition for recoding the 'vars' into the single new.value regardless of the old value.

Note that changing the value label of a variable's levels can be done with 'levels(var)[levels(var)=="old name"] <- "new name"'. However, Epicalc 'recode' is more efficient in changing several factors using the same scheme. See example. 

All the 'recode'd vars are automatically 'pack'ed into the default data frame which is synchronize with the one in the search path.
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