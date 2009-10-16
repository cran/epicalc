\name{Add missing records to a longitudinal data set)}
\alias{addMissingRecords}
\title{Add missing records to a longitudinal data set}
\description{Add missing records to a longitudinal data set, complete the fixed parts of covariates those missings and add a variable to indicate whether the subject was present in that schedule visit}
\usage{
addMissingRecords(dataFrame = .data, id, visit, check.present = TRUE, 
    present.varname = "present", update.visit.related.vars = TRUE) 
}
\arguments{
	\item{dataFrame}{Source data frame}
  \item{id}{identification variable}
  \item{visit}{index visit}
  \item{check.present}{whether a new variable should be added to indicate the presence of the subject in the particular visit}
  \item{present.varname}{name of the new variable indicating the presence of the subject}
  \item{update.visit.related.vars}{whether visit related variables among the added records should be updated}
}
\details{This function is used with a longitudinal data set where id and visit must be specified.

If there is any duplicated visit, the function will prompt error and stop.

The records of missing visits are added together with the fixed covariates (which do not change over time in each subject) filled up. By default, a new variable "present" is annexed on the last column of the output data frame.

Like all other Epicalc data management functions, variable descriptions are kept.
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'fillin'}
\examples{

data(bacteria, package="MASS")
des(bacteria)
head(bacteria, 10) # week 6 X01 and week 4 of X02 were missing
addMissingRecords(dataFrame=bacteria, id=ID, visit=week) -> bacteria.new
head(bacteria.new, 10)
# Note that the missing weeks are now added 'ap', 'hilo' and 'trt' which are fixed to id
# were automatically updated.
# A variable 'present' is also added.
# Columns are reordered to have ID and week leading others variables
rm(bacteria.new)

data(Xerop)
Xerop$time[500:501] <- 5:6  # Correct the error in the dataset
Xerop[1:25,] # Note Record 19 & 20, id 121140 had only two visits
Xerop.new <- addMissingRecords(dataFrame=Xerop, id=id, visit=time)
des(Xerop.new)
Xerop.new[19:24,]
rm(Xerop.new) 
# Note that 4 new records where this subject missed the followup were added.
# Id relatee variable ie. 'sex', and visit related variable ie. 'season' are updated 
# and 'present; is addeded 
}
\keyword{database}