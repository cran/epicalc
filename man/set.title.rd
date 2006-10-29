\name{set.title}
\alias{set.title}
\title{Setting language of Epicalc graph title}
\description{Setting locale and Internationalizing Epicalc graph title}
\usage{
set.title(locale)
}
\arguments{
	\item{locale}{A string denoting international language of choice}
}
\details{On calling 'library(epicalc)', '.locale' has an inital value of FALSE ie. the titles of Epicalc's automatic graphs are given in the English language. 'set.title' has two effects. It selects the locale and resets the hidden object '.locale' to TRUE. The command internationalizes the title of automatic graphs created by Epicalc according to 'locale' given in the function's argument. 

If '.locale' is TRUE, then the automatic graphs produced by Epicalc commands, such as 'summ(var)' or 'tab1(var)' or 'tabpct(var1,var2)', will lookup a language conversion table for the graph title and the title will be changed accordingly.

Internationalization can be disabled by typing '.locale <- FALSE' or the locale reset to English by issuing the command 'set.title("English")'.

}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'Sys.setlocale', 'Sys.getlocale' and 'title.string'}
\examples{
.data <- iris
attach(.data)
summ(Sepal.Length, by=Species)
set.title("English")
dotplot(Sepal.Length, by=Species)
set.title("Malay")
dotplot(Sepal.Length, by=Species)
set.title("Italian")
dotplot(Sepal.Length, by=Species)
detach(.data)
rm(.data)
}
\keyword{database}
