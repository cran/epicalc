\name{BE to AD}
\alias{be2ad}
\title{Change year in B.E. to A.D.}
\description{Convert Buddhist era date to Christian era date}
\usage{be2ad(Date.in.BE)
}
\details{This function may be useful in countries where dates are (wrongly) commonly entered in the Buddhist Era (BE). The function subtracts 543 from the year component of the argument 'Date.in.BE'. See 'note' below.}
\arguments{
	\item{Date.in.BE}{an object of class Date}
}
\note{
Although this function is useful in converting dates in BE to AD, there is still a serious limitation. 

All computers validate a date field based on the Gregorian calendar (AD). Since AD is BE less 543 years and the leap year is always with AD being a multiple of 4 (and not a multiple of 100, except if it is a multiple of 400), the computer will return an invalid date for any record with 29 February and the year in BE. Thus, any candidate dataset for this function should not have any date of 29 February. The function be2ad \strong{cannot} retrospectively solve this problem. 

If a user wants to enter data using BE, the above limitation can only be overcome by separating the three fields of BE year, month and day during data entry and then using either the existing data entry software, such as Epidata, or a statistical software, such as R, to change BE years to AD years before incorporating them into a new date variable. Thus, this date variable would have year in AD only and will not need be2ad. In order to display the correct date variable in BE format, locale must be in Thai and appropriate format must be chosen. See example.

Despite the above limitation, this function is kept in Epicalc. The reason is that there would still be a lot of (those type of faulty) datasets around in the countries that use BE that require changing BE to AD before any analysis of date variables can proceed. In doing so, the analyst must be aware of this potential problem in the dataset. It is advisable to check the data first to see whether there are any dates that fall on 29 February.
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\examples{
Date1 <- as.Date("2543-2-28")
be2ad(Date1)

## Not run:
## One would never have to

# be2ad(as.Date("2551-2-29"))

## because as.Date("2551-2-29") is an invalid Date
## End(Not run)

# To display date and time in BE under Thai Window OS
format(Sys.Date(), "\%x")
format(Sys.time(), "\%c")

}
\keyword{database}
