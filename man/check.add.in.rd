\name{Check.add.in}
\alias{check.add.in}
\title{List of functions added in by Epicalc}
\description{Check what functions have been added in by Epicalc.}
\usage{
check.add.in()
}
\details{
This function is not useful if the 'library(epicalc)' command has been issued without any error. 

In addition to listing the added in functions 'check.add.in()' provides the arguments and a brief description for the functions. 

If 'library(epicalc)' could not be called properly for any reason, the alternative is to install it by typing 'source("epicalc.R")' at the R command prompt. The functions stored in this source file 'Epicalc.r' are saved as objects in the environment, which can be shown by 'ls.str()'. The functions are also displayed as object names when the command 'ls()' is used. All these functions will be removed by 'rm(list=ls())'. To avoid these problems, a function in Epicalc, 'ls.nofunction()' is used instead of 'ls()' and another one, 'zap()', is used instead of 'rm(list=ls())'.  

On the other hand, if 'library(epicalc)' is working well, all the functions will be protected. 'ls.str()' will give no indication of function objects of Epicalc in the environment. 'rm(list=ls())' is safer to use since it has no effect on the functions in the library. 'check.add.in()' will still work but it is more convenient to explore the list of functions (commands) of Epicalc by 'help.start()', then choose 'package' and choose 'epicalc'.  For any specific command, it is more convinient to use 'help', such as 'help(cc)' because information from the help window is much more comprehensive than that from 'check.add.in()'.

In all cases, to check the arguments for a function such as 'cc' the simple standard R command, such as 'args(cc)' will work. 
}

\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{args','library', 'ls', 'ls.nofunction', 'ls.str', 'rm', 'zap'}
\keyword{environment}