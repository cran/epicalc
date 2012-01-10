\name{Rename}
\alias{rename}
\alias{ren}
\alias{rename.default}
\alias{rename.var}
\alias{rename.pattern}
\title{Rename variable(s) in the default data frame}
\description{Rename a variable or change a pattern of variable names.}
\usage{
rename(x1, x2, dataFrame = .data, ...)

\method{rename}{default}(x1, x2, dataFrame = .data, ...)

\method{rename}{var}(x1, x2, dataFrame = .data, ...)

\method{rename}{pattern}(x1, x2, dataFrame = .data, printNote=TRUE, ...)

ren(x1, x2, dataFrame = .data, ...)

}
\arguments{
       \item{x1}{a variable or a pattern among the names of the variables inside .data.}
       \item{x2}{new name or new pattern of the variable(s).

		\tabular{llll}{
        	\tab FUNCTION       \tab 'x1'          \tab 'x2'          \cr
        	\tab 'rename.var'     \tab old variable\tab new variable\cr
         	\tab 'rename.pattern' \tab old pattern \tab new pattern \cr
       }

}
       \item{dataFrame}{a data frame, the variable(s) of which will be renamed}
       \item{printNote}{whether the table of old names and new names of the variables(s) should be printed out.}
       \item{...}{further arguments passed to or used by other methods.}
}

\details{'rename.var' renames variable 'x1' to 'x2'. Both arguments may have the quotes omitted.

'rename.pattern' changes substring 'x1' in any names of variables inside .data to 'x2'. With 'printNote=TRUE', a table with columns of old and new variables will be displayed.

'rename.var' is called if 'x1' perfectly matches with a variable name. 'rename.pattern' is called if the pattern 'x1' is found as a substring among the variable names. Otherwise, an error will occur.

Finally, 'ren' is the abbreviated form of 'rename' without any suffix}
\author{Virasakdi Chongsuvivatwong
       \email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'recode' and 'label.var'}
\examples{
data(Oswego)
use(Oswego)
des()
rename.var("ill", "sick")
des()
# Note change of the 4th variable name

rename(timesupper, time.of.supper) 
# Note that '.var' and the quotes '"' can be omitted.
# But not 'rename(timesupper, "time of supper")'. Why? 

# Even shorter with 'ren'
ren(sex, gender)
des()

rename.pattern("ll", "LL")  
des()
rename("onset", "onset_") 
# '.pattern' can be omitted but not the quotes.
des()
}
\keyword{database}
