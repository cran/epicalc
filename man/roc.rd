\name{ROC}
\alias{lroc}
\alias{roc.from.table}
\title{ROC curve}
\description{Receiver Operating Characteristic curve of a logistic regression model and a diagnostic table}
\usage{
lroc(logistic.model, table=FALSE, add=FALSE, title=FALSE, line.col="red", auc.label=FALSE)
roc.from.table(table, graph = TRUE) 
}
\arguments{
	\item{logistic.model}{A model from logistic regression}
	\item{table}{A cross tabulation of the levels of a test (rows) vs a gold standard positive and negative (columns)}
	\item{add}{Whether the line is drawn on the existing ROC curve}
	\item{title}{If true, the model will be displayed as main title}
	\item{line.col}{Color of the line}
	\item{auc.label}{Whether the value of area under the curve should be displayed (only for 'add=FALSE')}
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
# Single ROC curve from logistic regression
model1 <- glm(case ~ spontaneous, family=binomial, data = infert)
lroc(model1, title=TRUE, auc.label=TRUE)

# Comparing two model
model2 <- glm(case ~ induced * spontaneous, family=binomial, data = infert)
lroc1 <- lroc(model1)
lroc1 # Note the returned list
lroc2 <- lroc(model2, add=TRUE, line.col="black")
legend("bottomright",legend=c(lroc1$model.description,lroc2$model.description), 
	lty=1, col=c("red","black"),bg="white")
title(main="Comparison of two logistic regression models")

# ROC from a diagnostic table
table1 <- as.table(cbind(c(1,27,56,15,1),c(0,0,10,69,21)))
colnames(table1) <- c("Non-diseased", "Diseased")
rownames(table1) <- c("(0,15]","(15,30]","(30,45]","(45,60]","60+")
table1
roc.from.table(table1, graph=TRUE)
}
\keyword{array}
