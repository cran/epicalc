\name{Add missing records to a longitudinal data set)}
\alias{addMissingRecords}
\title{Add missing records to a longitudinal data set}
\description{Add missing records to a longitudinal data set, complete the fixed parts of covariates those missings and add a variable to indicate whether the subject was present in that schedule visit}
\usage{
addMissingRecords(dataFrame = .data, id, visit, check.present = TRUE, 
    present.varname = "present") 
}
\arguments{
	\item{dataFrame}{Source data frame}
  \item{id}{identification variable}
  \item{visit}{index visit}
  \item{check.present}{whether a new variable should be added to indicate the presence of the subject in the particular visit}
  \item{present.varname}{name of the new variable indicating the presence of the subject}
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
head(bacteria, 10)
addMissingRecords(dataFrame=bacteria, id=ID, visit=week) -> data1
head(data1, 10)
# Note that the 6th week of the first ID and the 4th of the second ID were added.
# A variable 'present' is also added.
# Columns are reordered to have ID and week leading others variables
}
\keyword{database}