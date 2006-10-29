\name{ROC}
\alias{lroc}
\alias{roc.from.table}
\title{ROC curve}
\description{Receiver Operating Characteristic curve of a logistic regression model and a diagnostic table}
\usage{
lroc(logistic.model, table=FALSE)
roc.from.table(table, graph = TRUE) 
}
\arguments{
	\item{logistic.model}{A model from logistic regression}
	\item{table}{A cross tabulation of the levels of a test (rows) vs a gold standard positive and negative (columns)}
	\item{graph}{Draw ROC curve}
}
\details{
'lroc' graphs the ROC curve of a logistic regression model. If `table=TRUE', the diagnostic table based on the regression will be printed out.
'roc.from.table' computes the change of sensitivity and specificity of each cut point and uses these for drawing the ROC curve.
In both cases, the area under the curve is computed.
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'glm'}
\examples{
model1 <- glm(case ~ induced + spontaneous, family=binomial, data = infert)
lroc(model1)

table1 <- as.table(cbind(c(1,27,56,15,1),c(0,0,10,69,21)))
colnames(table1) <- c("Non-diseased", "Diseased")
rownames(table1) <- c("(0,15]","(15,30]","(30,45]","(45,60]","60+")
table1
roc.from.table(table1, graph=TRUE)
}
\keyword{array}
