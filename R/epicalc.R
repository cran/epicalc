# This file is written to make simple epidemiological calculator available on R.

# Prepared by 
#	Virasakdi Chongsuvivatwong, Epidemiology Unit,                                                
#	Prince of Songkla University
#	Hat Yai, Thailand 90110
#   License: GPL version 2 or newer


# Work started in 2002-October

# Lastupdated.Epicalc <- "22:05 2006-November-1"

### Display add-in functions
check.add.in <- function() {
cat("Add-in functions in Epicalc.R :", "\n")
cat("----","\n")
cat("cc (outcome, exposure, decimal = 2, cctable = NULL, graph = TRUE, design = \"cohort\")","\n", sep="")
cat("          compute OR, 95%CI, Chi2, & graphs (if=TRUE) in different designs","\n",sep="")
cat("cci (caseexp, controlex, casenonex, controlnonex, cctable = NULL, decimal = 2, graph = TRUE, design = \"cohort\")", "\n")
cat("          same as cc but with 2x2 table input from keyboard","\n")
cat("cs (outcome, exposure, cctable = NULL, decimal = 3)","\n", sep="")
cat("          compute RR, Rdiff, 95%CI for a cohort and cross-sectional study","\n",sep="")
cat("csi(caseexp, controlex, casenonex, controlnonex, cctable = NULL, decimal = 3) ", "\n")
cat("          same as cs but with 2x2 table input from keyboard","\n")
cat("des(x = .data, search.position = 2)", "\n")
cat("          display variables in the loaded object and their description","\n")
cat("detach.all.data()", "\n")
cat("          detach all data frame from the search path","\n")
cat("dotplot(x, bin = 40, by = NULL, ...)", "\n")
cat("          dotplot a continuous variable (by group)","\n")
cat("followup.plot(id, time, outcome, by=NULL, n.of.lines=NULL, legend=TRUE)", "\n")
cat("          Draw lines of longitudinal value of a continuous variable (by group)","\n")
cat("IDR.display(count.model, decimal = 3, alpha = 0.05)","\n") 
cat("          compute incidence density ratios from a poisson regression", "\n")
cat("kap (kaptable, wttable = NULL)", "\n")
cat("          compute kappa statistic for two raters agreement from observed and weight tables", "\n")
cat("label.var (var, label, pack = TRUE)", "\n")
cat("          label a variable and pack into .data", "\n")
cat("label.table (outcome, exposure, cctable = NULL, cctable.dimnames = NULL)","\n")
cat("          labeling table mainly for cc and cs","\n")
cat("logistic.display (logistic.model, alpha = 0.05, decimal = 3)", "\n")
cat("          logistic regression with odds ratio","\n")
cat("lookup(x, lookup.array)", "\n")
cat("          returns numeric vector using a pre-defined lookup array, `tx'", "\n")
cat("lroc(logistic.model)", "\n")
cat("          ROC curve of a logistic regression model", "\n")
cat("lrtest (model1, model2)", "\n")
cat("          likelihood ratio test between 2 models","\n")
cat("ls.nofunction()", "\n")
cat("          list all objects in memory excluding functions", "\n")
cat("make2x2(caseexp, controlex, casenonex, controlnonex)", "\n")
cat("          prepare a 2 by 2 table from input of 4 cell values", "\n")
cat("match.tab  (case, exposed, strata)", "\n")
cat("          matched tabulation case control study", "\n")
cat("mhor(..., mhtable = NULL, graph = TRUE, design = \"cohort\")","\n",sep="")
cat("          Mantel-Haenszel OR, chi2 and homogeneity test & graph (if=TRUE)","\n", sep="")
cat("mlogit.display (multinom.model, decimal = 2, alpha = 0.05)", "\n")
cat("          Display multinomial regression results", "\n")
cat("n.for.2p (p1, p2, alpha = 0.05, power = 0.8, ratio = 1) ","\n")	
cat("          sample size for hypothesis testing of two proportions", "\n")
cat("n.for.2means (mu1, mu2, sd1, sd2, ratio = 1, alpha = 0.05, power = 0.8)", "\n")
cat("          sample size for hypothesis testing of two means", "\n")
cat("n.for.lqas (p0, q = 0, N = 10000, alpha = 0.05, exact = FALSE) ","\n")
cat("          sample size for lot quality assurance sampling", "\n")
cat("n.for.survey (p, delta = 0.5 * min(c(p, 1 - p)), popsize = FALSE, deff = 1, alpha = 0.05)","\n")	
cat("          sample size for survey", "\n")
cat("ordinal.or.display  (ordinal.model, decimal = 3, alpha = 0.05)", "\n")
cat("          compute ordinal odds ratio from an ordinal logistic regression", "\n")
cat("pack (data.frame = .data)", "\n")
cat("          pack current new variable(s) into the existing .data", "\n")
cat("poisgof(model)","\n")
cat("          goodness-of-fit test for Poisson assumption","\n")
cat("power.for.2means (mu1, mu2, n1, n2, sd1, sd2, alpha = 0.05)", "\n")
cat("          power in testing significant difference of two means","\n")
cat("power.for.2p (p1, p2, n1, n2, alpha = 0.05)", "\n")
cat("          power in testing significant difference of two proportions","\n")
cat("pyramid (age, sex, binwidth = 5, age.sex.table = NULL, ..., percent = c(FALSE, \"each\", \"total\"))", "\n")
cat("          drawing a pyramid barplot for age-sex distribution","\n")
cat("recode (vars, old.value, new.value)", "\n")
cat("          recode value of variables", "\n")
cat("roc.from.table (table, graph = TRUE)", "\n")
cat("           ROC curve from a diagnostic table","\n")
cat("shapiro.qqnorm (x, ...)", "\n")
cat("          draw a quantile-normal plot with Shapiro-Wilk test P value", "\n")
cat("sort.by(...)", "\n")
cat("          sort hidden data frame (.data) and related vector(s) by argument order", "\n")
cat("summ  (x = .data, by = NULL, graph = TRUE, box = FALSE)", "\n")
cat("          summary Statistics of the dataset or a variable with graph", "\n")
cat("tabpct  (row, col, ..., graph = TRUE, las = 0)", "\n")
cat("          cross-tabulation with row and col percents & graph","\n")
cat("tab1(x0, decimal = 1, sort.group = c(FALSE, \"decreasing\",\"increasing\")","\n")
cat("graph = TRUE, missing = TRUE, bar.values = TRUE)","\n")
cat("          one-way tabulation of a vector","\n")
cat("title.string (distribution.of, by, frequency, locale=.locale)","\n")
cat("          set the string for automatic graph title","\n")
cat("use (filename, clear = TRUE)", "\n")
cat("          read in Stata or EpiInfo file and hide as `.data'","\n")
cat("zap()", "\n")
cat("          remove all non-function objects and detach all data frames","\n")
cat("\n")
cat("Last modification at", .Lastupdated, "\n") 
cat("\n")
cat("[Parameters in bracket with a `=' sign are default values.]","\n")
cat("Use <Ctrl> + <up arrow> to scroll up.","\n")
cat("\n")
cat("If the library of Epicalc has been properly installed, it is better to type `help.start()'.", "\n")
cat("Then click `packages' and then `epicalc'.","\n")
}

# Setting locale and automatic graph titles
.locale <- FALSE # All automatic graphs will initially have English titles
.distribution.of <- "Distribution of"
.by <- "by"
.frequency <- "Frequency"

set.title <- function(locale){
  Sys.setlocale("LC_ALL",locale)
  print(Sys.getlocale())
  .locale <<- TRUE 
  # With `set title' command the language of title will change with locale
  # listed in the array of the title string.
}

title.string <- function(distribution.of=.distribution.of,by=.by,frequency=.frequency, locale=.locale, return.look.up.table=FALSE){
	# title.array can be changed or added rows
    title.array <- rbind( c("Distribution of","by","Frequency"),
                      c("Pembahagian","mengikut","Kekerapan"),
                      c("Phan bo","theo","Tan so"),
					  c("Verteilung von","nach","frequenz"),
					  c("Distribution de","par","frequence"),
					  c("distribuzione di","per","frequenza"),
					  c("distribucion de", "por", "frecuencia"))
   colnames(title.array) <- c("Distribution of","by","Frequency")
   rownames(title.array) <- c("English", "Malay", "Vietnamese",
	"German", "French", "Italian", "Spanish")
  if(locale){
  i <- 1
  while(length(grep(rownames(title.array)[i],Sys.getlocale("LC_ALL")))!=1){
   i <- i+1
  }
  row.chosen <- i
  if(i <= nrow(title.array)){
      distribution.of <- title.array[row.chosen,1]
      by <- title.array[row.chosen,2]; frequency <- title.array[row.chosen,3]
    }
  }else{.locale<<-FALSE
	.distribution.of <<- distribution.of
	.by <<- by
	.frequency <<- frequency
	}
	if(return.look.up.table){
		return(list(distribution.of=distribution.of,by=by,frequency=frequency, look.up.table=title.array))
	}else{
		return(list(distribution.of=distribution.of,by=by,frequency=frequency))
	}
}




library(foreign)
### Display variables and their description
des <- function(x=.data, search.position=2) { 
# search.positon mean position of the data in the search path 'search()'
if(!is.data.frame(x)) {cat("\n")
	a<-cbind(as.character(substitute(x)),(class(x))[1],(attr(get(search()[search.position]), "var.labels")[attr(get(search()[search.position]), "names")==substitute(x)]))
	if(dim(a)[2]==2){# |is.null(attr(get(search()[search.position]), "var.labels"))){
		colnames(a) <- c("Variable     ","Class          ")
	}
	else{
		colnames(a) <- c("Variable     ","Class          ", "Description")
	}
	rownames(a) <- ""
	if(length(x)==nrow(get(search()[search.position]))){
		cat("Parental data.frame is probably",search()[search.position],"\n")
	cat(attr(get(search()[search.position]), "datalabel"), "\n")
	cat("Variable order =", (1:ncol(get(search()[search.position])))[colnames(get(search()[search.position]))==substitute(x)], "\n","\n")
	}
	cat("No. of observations = "); cat(length(x), "\n", "\n")
	print.noquote(a)
	cat("\n")
}
else{
if(is.null(attr(x, "var.labels")))  {
	b <- " " 
	} else {
	b <- attr(x, "var.labels")
	if(length(b) < length(colnames(x))) {options(warn=-1)}
}
class.a <- NULL
for(i in 1:ncol(x)){class.a <- c(class.a, class(x[,i])[1])}
a <- cbind(colnames(x),class.a, b)
colnames(a) <- c("Variable     ", "Class          ", "Description")
rownames(a) <-1:nrow(a)
cat ("\n")
cat(attr(x, "datalabel"), "\n")
cat("No. of observations ="); cat(nrow(x), "\n")
print.noquote(a)
cat ("\n")
options(warn=0)
}
}
### Detaching all data frame from the search path
detach.all.data <- function(){
pos.to.detach <- (1:length(search()))[substring(search(),first=1,last=8)!="package:" &
	search()!=".GlobalEnv" & search()!="Autoloads" & search()!="CheckExEnv"]
for(i in 1:length(pos.to.detach)){
	if(length(pos.to.detach)>0){
		detach(pos=pos.to.detach[1])
		pos.to.detach <- (1:length(search()))[substring(search(),first=1,last=8)!="package:" &
		search()!=".GlobalEnv" & search()!="Autoloads" & search()!="CheckExEnv"]
	}
}
}
### Getting percentage from the tabulation
tabpct <- function(row, col, ..., decimal=1, graph=TRUE, las=0) {
tab <- table(row, col,..., deparse.level=1, dnn=list(substitute(row),substitute(col)))
# column percent
cpercent <-tab
for(i in 1:ncol(tab)) { cpercent[,i] <-paste("(",format(round(tab[,i]/colSums(tab)[i]*100, digits=decimal),trim=TRUE),")", sep="")}
cpercent <- rbind(cpercent, rep("(100)", ncol(tab)))
col.1.1 <- cbind(format(c(tab[,1],sum(tab[,1])), trim=TRUE), cpercent[,1])
for(i in 2:ncol(tab)){
col.1.1 <- cbind(col.1.1, c(format(tab[,i], trim=TRUE), format(sum(tab[,i]), trim=TRUE)), cpercent[,i])
}
cpercent <- col.1.1
cnames <- character(0)
for(i in 1:ncol(tab) ){ cnames <- c(cnames, colnames(tab)[i], "%")}
colnames(cpercent) <- cnames
rownames(cpercent)[nrow(cpercent)] <- "Total"

# rowpercent
rpercent <-tab
for(i in 1:nrow(tab)) { rpercent[i,] <-paste("(",round(tab[i,]/rowSums(tab)[i]*100, digits=1),")", sep="")}
rpercent <- cbind(rpercent,c(rep("(100)",nrow(tab))))
row.1.1 <- rbind(format(c(tab[1,],sum(tab[1,])), trim=TRUE), rpercent[1,])
for(i in 2:nrow(tab)){
row.1.1 <- rbind(row.1.1, c(format(tab[i,], trim=TRUE), format(sum(tab[i,]), trim=TRUE)), rpercent[i,])
}
rpercent <- row.1.1
rnames <- character(0)
for(i in 1:nrow(tab) ){ rnames <- c(rnames, rownames(tab)[i], "")}
rownames(rpercent) <- rnames
colnames(rpercent)[ncol(rpercent)] <- "Total"

		var1 <- as.character(substitute(row))
		if(length(var1)>1){
			string2 <- var1[length(var1)]	
		}else
		if(substring(search()[2],first=1,last=8)!="package:"){
			string2 <-  attr(get(search()[2]), "var.labels")[attr(get(search()[2]), "names")==substitute(row)]
			if(length(string2)==0){
				string2 <- as.character(substitute(row))
			}
			if(string2==""){
				string2 <- as.character(substitute(row))
			}
		}else{
			string2 <- as.character(substitute(row))
		}
		if(substring(search()[2],first=1,last=8)!="package:"){
			string4 <-  attr(get(search()[2]), "var.labels")[attr(get(search()[2]), "names")==substitute(col)]
			if(length(string4)==0){
				string4 <- as.character(substitute(col))
			}else{
				if(string4==""){
					string4 <- as.character(substitute(col))
				}
			}
		}else{
			string4 <- as.character(substitute(col))
		}
names(attr(tab,"dimnames")) <-c(ifelse(nchar(string2)>15,substr(string2,1,15) ,string2), string4)
cat( "\n")
cat("Original table", "\n")
tabtotal <- addmargins(tab)
colnames(tabtotal)[ncol(tabtotal)] <- "Total"
rownames(tabtotal)[nrow(tabtotal)] <- "Total"
print(tabtotal, print.gap=2)
cat( "\n")

cat("Row percent", "\n")
names(attr(rpercent,"dimnames")) <- c(ifelse(nchar(string2)>15,substr(string2,1,15) ,string2), string4)
print.table(rpercent, right=TRUE, print.gap=2)
cat( "\n")

cat("Column percent", "\n")
names(attr(cpercent,"dimnames")) <- c(ifelse(nchar(string2)>15,substr(string2,1,15) ,string2), string4)
print.table(cpercent, right=TRUE, print.gap=2)
cat( "\n")

if(graph==TRUE){
	rownames(tab)[is.na(rownames(tab))] <- "missing"
	colnames(tab)[is.na(colnames(tab))] <- "missing"
	las.value <- las
	plot(as.table(tab),xlab=string2, ylab=string4, main=paste(title.string()$distribution.of,string4,title.string()$by,string2),
	col=c("white",2:length(col)), las=las.value)}
} 
####
cci <- function(caseexp, controlex, casenonex, controlnonex, cctable=NULL, decimal=2, graph=TRUE, design="cohort") {

if(is.null(cctable)){
	frame <- cbind(Outcome<-c(1,0,1,0),Exposure<- c(1,1,0,0),Freq <- c(caseexp,controlex, casenonex, controlnonex))
	Exposure <- factor(Exposure)
	expgrouplab <- c("Non-exposed","Exposed") 
	levels(Exposure) <- expgrouplab
	Outcome <- factor(Outcome)
	outcomelab <- c("Negative", "Positive")
	levels(Outcome) <- outcomelab
	table <- xtabs(Freq ~ Outcome + Exposure, data=frame)
}else{
	table <- as.table(get("cctable"))
}

fisher.test(table) -> fisher
cat( "\n")
table2 <- addmargins(table)
rownames(table2)[nrow(table2)] <- "Total"
colnames(table2)[ncol(table2)] <- "Total"
print(table2)
cat( "\n")
cat(c("OR = ", round(fisher$estimate, decimal)), "\n");
cat(c("95% CI =", round(fisher$conf.int,decimal)), "\n");
cat(c("Chi-squared =", round(summary(table)$statistic,decimal), ", ",
	summary(table)$parameter, "d.f. ,", "P value =", 
	round(summary(table)$p.value,decimal+1),  "\n"))
cat(c("Fisher's exact test (2-sided) P value =", 
	round(fisher$p.value,decimal+1),  "\n"))
cat( "\n")
	if(graph==TRUE)
	{
	if(design=="prospective"||design=="cohort"||design=="cross-sectional"){
	
	graph.prospective(caseexp, controlex, casenonex, controlnonex)
	}
	else
	graph.casecontrol(caseexp, controlex, casenonex, controlnonex)
	
}
}

########## case control study from a data file or cctable
cc <- function(outcome, exposure,  decimal=2, cctable=NULL, graph=TRUE, design="cohort") {
if(is.null(cctable)){
	cctable <- table(outcome, exposure, deparse.level=1, dnn=list(substitute(outcome),substitute(exposure)))
	cctable.dimnames <- names(attr(cctable,"dimnames"))
}else{
	cctable.dimnames <- names(attr(cctable,"dimnames"))
}

if(ncol(cctable) > 2 & nrow(cctable) ==2){
or <- rep(NA, ncol(cctable))
lowci <- rep(NA, ncol(cctable))
hici <-rep(NA, ncol(cctable))
or[1] <- 1
for(i in 2:ncol(cctable)){or[i] <- fisher.test(cctable[,c(1,i)])$estimate}
for(i in 2:ncol(cctable)){lowci[i] <- fisher.test(cctable[,c(1,i)])$conf.int[1]}
for(i in 2:ncol(cctable)){hici[i] <- fisher.test(cctable[,c(1,i)])$conf.int[2]}
row4 <- as.character(round(or,decimal)); row4[1] <- "1"
row5 <- as.character(round(lowci,decimal)); row5[1] <- " "
row6 <- as.character(round(hici,decimal)); row6[1] <- " "
table2 <-rbind(cctable, "", row4, row5, row6)
rownames(table2)[4] <- "Odds ratio"
rownames(table2)[5] <- "lower 95% CI  "
rownames(table2)[6] <- "upper 95% CI  "
names(attr(table2, "dimnames"))<- names(attr(cctable,"dimnames"))
cat("\n")
print.noquote(table2)
cat("\n")
cat("Chi-squared =", round(chisq.test(cctable)$statistic,3), ",",
	chisq.test(cctable)$parameter,"d.f.,",
	"P value =",round(chisq.test(cctable, correct=FALSE)$p.value,decimal+1),"\n")
cat("Fisher's exact test (2-sided) P value =",round(fisher.test(cctable)$p.value,decimal+1),"\n")
cat("\n")
if(graph==TRUE & design=="cohort"){


	if(any(cctable<5)) {
		cat("One of more cells is/are less than 5, not appropriate for graphing","\n","\n")
	}else{
		y <- rep(NA, ncol(cctable))
		x <- 1:ncol(cctable)
		x.left <- x-0.02
		x.right <- x+0.02
		plot(x,or, ylab="Odds ratio", 
			xlab=paste(names(attr(cctable,"dimnames")[2])), xaxt="n",
			main="Odds ratio from prospective/X-sectional study",pch=" ",
			xlim=c(min(x.left)-.2, x.right[ncol(cctable)]+0.2), 
			ylim=c(min(c(1,min(lowci,na.rm=TRUE),min(hici,na.rm=TRUE))), 
				max(c(1,max(hici,na.rm=TRUE,max(lowci,na.rm=TRUE))))),
			log="y", type="l")
		for(i in 1:ncol(cctable)){
			lines(x=c(x[i],x[i]), y=c(lowci[i],hici[i]))
			lines(x=c(x.left[i],x.right[i]),y=c(lowci[i],lowci[i]))
			lines(x=c(x.left[i],x.right[i]),y=c(hici[i],hici[i]))
			points(x[i],or[i], pch=22, cex=sum(cctable[,i])*(5/sum(cctable)))
		}
	axis(1,at=x, labels=colnames(cctable))

	text(1,1,labels="1", pos=4, font=4, col="brown")	
	text(x[-1],or[-1],labels=row4[-1],col="brown", pos=1, font=4)
	text(x,or,labels=ifelse(x==1,"",paste("(",row5,",",row6,")")),
		pos=1, font=4, offset=1.5, col="brown")


	}
}
}else{
label.table(outcome, exposure, cctable=cctable, cctable.dimnames=cctable.dimnames) -> a
cci (caseexp=a$caseexp, controlex= a$controlex, casenonex=a$casenonex, controlnonex=a$controlnonex, 
	cctable=a$cctable, decimal=decimal, graph=graph, design=design)
}
}
##### graph for a case control study

graph.casecontrol <- function(caseexp, controlex, casenonex, controlnonex, decimal=2){
if(any(c(caseexp, controlex, casenonex, controlnonex)<5)) {
	cat("One of more cells is/are less than 5, not appropriate for graphing","\n")
	}
	else
	{
	table <- c(caseexp, controlex, casenonex, controlnonex); dim(table)<- c(2,2)
	fisher <- fisher.test(table)
	logit0 <- log(controlex/controlnonex); se0 <-sqrt(1/controlex+1/controlnonex)
	logit1 <- log(caseexp/casenonex); se1 <- sqrt(1/caseexp+1/casenonex)
	x <- c(c(-1,0,1)*1.96*se0 + logit0,c(-1,0,1)*1.96*se1 + logit1 )
	y <- c(rep(0,3),rep(1,3))	
	plot(x[c(1,3,4,6)],y[c(1,3,4,6)], xlab="Odds of exposure",ylab="Outcome category", yaxt="n", xaxt="n",
		main="Odds ratio from case control study", pch=73)
	points(x[c(2,5)],y[c(2,5)], pch=22, cex=c((controlex+controlnonex),(caseexp+casenonex))/sum(table)*5)
	axis(2,at=c(0,1),labels=c("control","case"),las=1)
	x1 <- exp(x)
	a <- 2^(-10:10)
	if(length(a[a>min(x1) & a<max(x1)]) >2 & length(a[a>min(x1) & a<max(x1)]) <10){
	a1 <- a[a>min(x1) & a<max(x1)]
		if(any(a1>=1))axis(1,at=log(a1[a1>=1]),labels=as.character(a1[a1>=1]))
		if(any(a1<1))axis(1,at=log(a1[a1<1]),labels=paste(as.character(1),"/",
		as.character(trunc(1/a1[a1<1])), sep=""))
	}
	else
	{
	options(digit=2)
	at.x <-  seq(from=min(x),to=max(x),by=((max(x)-min(x))/5))
	labels.oddsx <- exp(at.x)
	axis(1,at=at.x, labels=as.character(round(labels.oddsx,digits=decimal)), las=1)
	}
	lines(x[1:3],y[1:3])
	lines(x[4:6],y[4:6])
	lines(x[c(2,5)],y[c(2,5)])
	arrows(x0=logit0 ,x1=logit1, y0 =0.1, y1=0.1, code=2, col="red")
	text(x=(max(x)+min(x))/2, y= 0.3, labels=paste("OR = ", round(fisher$estimate, decimal)))
	text(x=(max(x)+min(x))/2, y= 0.2, labels=paste("95% CI =", round(fisher$conf.int,2)[1],",", round(fisher$conf.int,2)[2]))
	abline(v=c(logit0, logit1), lty=3, col="blue") 
	}
}

##### graph for a cohort study

graph.prospective <- function(caseexp, controlex, casenonex, controlnonex, decimal=2){
if(any(c(caseexp, controlex, casenonex, controlnonex)<5)) {
	cat("One of more cells is/are less than 5, not appropriate for graphing","\n","\n")
	}
	else
	{
	table <- c(caseexp, controlex, casenonex, controlnonex); dim(table)<- c(2,2)
	fisher <- fisher.test(table)
	logit0 <- log(casenonex/controlnonex); se0 <-sqrt(1/casenonex+1/controlnonex)
	logit1 <- log(caseexp/controlex); se1 <- sqrt(1/caseexp+1/controlnonex)
	y <- c(c(-1,0,1)*1.96*se0 + logit0,c(-1,0,1)*1.96*se1 + logit1 )
	x <- c(rep(0,3),rep(1,3))
	plot(x[c(1,3,4,6)],y[c(1,3,4,6)], ylab="Odds of outcome",xlab="Exposure category", yaxt="n", xaxt="n",
		main="Odds ratio from prospective/X-sectional study",pch=" ")
	lines(x=c(-.02,.02),y=c(y[1],y[1]))
	lines(x=c(-.02,.02),y=c(y[3],y[3]))
	lines(x=c(.98,1.02),y=c(y[4],y[4]))
	lines(x=c(.98,1.02),y=c(y[6],y[6]))
	points(x[c(2,5)],y[c(2,5)], pch=22, cex=c((controlnonex+casenonex),(caseexp+controlex))/sum(table)*5)
	axis(1,at=c(0,1),labels=c("non-exposed","exposed"))
	y1 <- exp(y)
	a <- 2^(-10:10)
	if(length(a[a>min(y1) & a<max(y1)]) >2 & length(a[a>min(y1) & a<max(y1)]) <10){
	a1 <- a[a>min(y1) & a<max(y1)]
		if(any(a1>=1))axis(2,at=log(a1[a1>=1]),labels=as.character(a1[a1>=1]), las=1)
		if(any(a1<1))axis(2,at=log(a1[a1<1]),labels=paste(as.character(1),"/",
		as.character(trunc(1/a1[a1<1])), sep=""),las=1)
	}
	else
	{
	options(digit=2)
	at.y <-  seq(from=min(y),to=max(y),by=((max(y)-min(y))/5))
	labels.oddsy <- exp(at.y)
	axis(2,at=at.y, labels=as.character(round(labels.oddsy,digits=decimal)), las=1)
	}
	lines(x[1:3],y[1:3])
	lines(x[4:6],y[4:6])
	lines(x[c(2,5)],y[c(2,5)])
	arrows(y0=logit0 ,y1=logit1, x0 =0.25, x1=0.25, code=2, col="red")
	text(y = min(y) + .55*(max(y)-min(y)), x= 0.5, labels=paste("OR = ", round(fisher$estimate, decimal)))
	text(y = min(y) + .45*(max(y)-min(y)), x= 0.5, labels=paste("95% CI =", round(fisher$conf.int,decimal)[1],",", round(fisher$conf.int,decimal)[2]))
	abline(h=c(logit0, logit1), lty=3, col="blue") 
	}
}
#### Create a `cctable' in global environment and label row and column with the variable descriptions
label.table <- function(outcome, exposure, cctable=NULL , cctable.dimnames=NULL){
if(is.null(cctable)){
	cctable <- table(outcome, exposure, deparse.level=1, dnn=list(substitute(row),substitute(col)))
}
if(is.null(names(attr(cctable,"dimnames")))){
	dimnames(cctable) <- list(Outcome=c("Non-diseased","Diseased"),Exposure=c("Non-exposed","Exposed"))	
}

if(is.null(cctable.dimnames)){
	cctable.dimnames <- names(attr(cctable,"dimnames"))
}
		if(substring(search()[2],first=1,last=8)!="package:"){
			string2 <-  attr(get(search()[2]), "var.labels")[attr(get(search()[2]), "names")==cctable.dimnames[1]]
			if(length(string2)==0){
				string2 <- cctable.dimnames[1]
			}else{
			if(string2==""){
				string2 <- cctable.dimnames[1]
			}else{
				string2 <- cctable.dimnames[1]
			}
			}}
		if(substring(search()[2],first=1,last=8)!="package:"){
			string4 <-  attr(get(search()[2]), "var.labels")[attr(get(search()[2]), "names")==cctable.dimnames[2]]
			if(length(string4)==0){
				string4 <- cctable.dimnames[2]
			}else{
				if(string4==""){
					string4 <- cctable.dimnames[2]
				}
			}
		}else{
			string4 <- cctable.dimnames[2]
			string2 <- cctable.dimnames[1]
		}
names(attr(cctable,"dimnames")) <-c(ifelse(nchar(string2)>15,substr(string2,1,15) ,string2), string4)
suppressWarnings(return(cctable, caseexp=cctable[2,2], controlex=cctable[1,2], casenonex=cctable[2,1], controlnonex=cctable[1,1]))
}

#### Cohort tabulation from a dataset
cs <- function(outcome, exposure, cctable = NULL, decimal=2) {
if(is.null(cctable)){
	cctable <- table(outcome, exposure, deparse.level=1, dnn=list(substitute(outcome),substitute(exposure)))
	cctable.dimnames <- names(attr(cctable,"dimnames"))
}else{
	cctable.dimnames <- names(attr(cctable,"dimnames"))
}

if(ncol(cctable) > 2 & nrow(cctable) ==2){
r     <- rep(NA, ncol(cctable))
rr    <- rep(NA, ncol(cctable))
lowci <- rep(NA, ncol(cctable))
hici  <-rep(NA, ncol(cctable))
for(i in 1:ncol(cctable)) {r[i] <- cctable[2,i]/colSums(cctable)[i]}
rr[1] <- 1
for(i in 2:ncol(cctable)){rr[i] <- (cctable[2,i]/colSums(cctable)[i])/(cctable[2,1]/colSums(cctable)[1])}
for(i in 2:ncol(cctable)){lowci[i] <- rr[i]^(1-qnorm(1-.05)/sqrt(suppressWarnings(chisq.test(cbind(cctable[,1],cctable[,i])))$statistic))}
for(i in 2:ncol(cctable)){hici[i] <- rr[i]^(1+qnorm(1-.05)/sqrt(suppressWarnings(chisq.test(cbind(cctable[,1],cctable[,i])))$statistic))}
row4 <- as.character(round(r,decimal))
row5 <- as.character(round(rr,decimal)); row5[1] <- "1"
row6 <- as.character(round(lowci,decimal)); row6[1] <- " "
row7 <- as.character(round(hici,decimal)); row7[1] <- " "
table2 <-rbind(cctable,"", row4, row5, row6, row7)
rownames(table2)[4] <- "Absolute risk"
rownames(table2)[5] <- "Risk ratio"
rownames(table2)[6] <- "lower 95% CI  "
rownames(table2)[7] <- "upper 95% CI  "
names(attr(table2, "dimnames"))<- names(attr(cctable,"dimnames"))
cat("\n")
print.noquote(table2)
cat("\n")
cat("Chi-squared =", round(chisq.test(cctable)$statistic,3), ",",
	chisq.test(cctable)$parameter,"d.f.,",
	"P value =",round(chisq.test(cctable, correct=FALSE)$p.value,decimal+1),"\n")
cat("Fisher's exact test (2-sided) P value =",round(fisher.test(cctable)$p.value,decimal+1),"\n")
cat("\n")

	if(any(cctable<5)) {
		cat("One of more cells is/are less than 5, not appropriate for graphing","\n","\n")
	}else{
		x <- 1:ncol(cctable)
		x.left <- x-0.02
		x.right <- x+0.02
		plot(x,rr, ylab= "Risk ratio", xaxt="n",
			xlab=paste(names(attr(cctable,"dimnames")[2])),
			main="Risk ratio from a cohort study",pch=" ",
			xlim=c(min(x.left)-.2, x.right[ncol(cctable)]+0.2), 
			ylim=c(min(c(1,min(lowci,na.rm=TRUE),min(hici,na.rm=TRUE))), 
				max(c(1,max(hici,na.rm=TRUE,max(lowci,na.rm=TRUE))))),
			log="y", type="l")
		for(i in 1:ncol(cctable)){
			lines(x=c(x[i],x[i]), y=c(lowci[i],hici[i]))
			lines(x=c(x.left[i],x.right[i]),y=c(lowci[i],lowci[i]))
			lines(x=c(x.left[i],x.right[i]),y=c(hici[i],hici[i]))
			points(x[i],rr[i], pch=22, cex=sum(cctable[,i])*(5/sum(cctable)))
		}
	axis(1,at=x, labels=colnames(cctable))
	text(1,1,labels="1", pos=4, font=4, col="brown")	
	text(x[-1],rr[-1],labels=row5[-1],col="brown", pos=1, font=4)
	text(x,rr,labels=ifelse(x==1,"",paste("(",row6,",",row7,")")),
		pos=1, font=4, offset=1.5, col="brown")
	}

}else{
label.table(outcome, exposure, cctable=cctable, cctable.dimnames=cctable.dimnames) -> a
csi (caseexp=a$caseexp, controlex= a$controlex, casenonex=a$casenonex, controlnonex=a$controlnonex, 
	cctable=a$cctable, decimal=decimal)
}
}

#### Cohort tabulation from keyboard
csi <- function(caseexp, controlex, casenonex, controlnonex, cctable=NULL, decimal=2) {
if(is.null(cctable)){
	frame <- cbind(Outcome<-c(1,0,1,0),Exposure<- c(1,1,0,0),Freq <- c(caseexp,controlex, casenonex, controlnonex))
	Exposure <- factor(Exposure)
	expgrouplab <- c("Non-exposed","Exposed") 
	levels(Exposure) <- expgrouplab
	Outcome <- factor(Outcome)
	outcomelab <- c("Negative", "Positive")
	levels(Outcome) <- outcomelab
	table <- xtabs(Freq ~ Outcome + Exposure, data=frame)
}else{
	table <- get("cctable")
}
cat( "\n")
table2 <- addmargins(table)
rownames(table2)[nrow(table2)] <- "Total"
colnames(table2)[ncol(table2)] <- "Total"
risk <- table2[2,]/table2[3,]
table2 <- rbind(table2,c("","",""), c("Rne","Re","Rt"), round(risk,decimal), deparse.level=1)
rownames(table2)[c(4:6)] <- c("","","Risk")
names(attr(table2,"dimnames")) <- names(attr(table,"dimnames")) 
print.noquote(table2)

# Computing chi-squared
	a <- table[1,1]
        A <- sum(table[,1])*sum(table[1,])/sum(table[,])
        Vara <- sum(table[, 1]) / (sum(table[, ]) - 1) * 
		sum(table[1, ]) * 
		sum(table[, 2]) * 
		sum(table[2, ]) /
		sum(table[, ])^2
# MH corrected chisquared: 
chi2 <-abs(a-A)^2/Vara


risk.diff  <- risk[2]-risk[1]
risk.diff.lower <- round(risk.diff*(1-(qnorm(1-.05/2)/sqrt(chi2))),decimal)
risk.diff.upper <- round(risk.diff*(1+(qnorm(1-.05/2)/sqrt(chi2))),decimal)
risk.ratio <- round(risk[2]/risk[1], decimal)
risk.ratio.lower <- round(risk.ratio^(1-(qnorm(1-.05/2)/sqrt(suppressWarnings(chisq.test(table)$statistic)))),decimal)
risk.ratio.upper <- round(risk.ratio^(1+(qnorm(1-.05/2)/sqrt(suppressWarnings(chisq.test(table)$statistic)))),decimal)
if(risk.ratio < 1) {
	protective.efficacy <- round(-risk.diff/risk[1]*100, decimal-1)
	nnt <- round(-1/risk.diff,decimal) 	
	risk.names <- c("Risk difference (absolute risk changed)","Risk ratio","Protective efficacy (%)", 
		"Number needed to treat (NNT)"  )
	risk.table <- cbind(risk.names, c(round(risk.diff,decimal), risk.ratio, protective.efficacy, nnt),
		c(risk.diff.lower,risk.ratio.lower,"",""),c(risk.diff.upper,risk.ratio.upper,"",""))
}else{
	attributable.frac.exp <- round(risk.diff/risk[2], decimal) 
	pop.risk.diff <- risk[3] - risk[1]
	attributable.frac.pop <- round((risk[3] - risk[1])/risk[3]*100, decimal)
	cat("\n")
	risk.names <- c("Risk difference (attributable risk)","Risk ratio","Attr. frac. exp. -- (Re-Rne)/Re",
		"Attr. frac. pop. -- (Rt-Rne)/Rt*100 %  ")
	risk.table <- cbind(risk.names, c(round(risk.diff,decimal), risk.ratio, attributable.frac.exp,
	attributable.frac.pop), c(risk.diff.lower,risk.ratio.lower,"",""),c(risk.diff.upper,risk.ratio.upper,"",""))
}
	row.names(risk.table) <- rep("",4)
	colnames(risk.table) <- c("","Estimate","Lower95ci","Upper95ci")
	print.noquote(risk.table)
	cat("\n")
suppressWarnings(rm(cctable,caseexp, controlex, casenonex, controlnonex, pos=1))
}

### IDR display for poisson regression
idr.display <- function(count.model, decimal=3, alpha=.05){
model <- count.model
s1 <- summary(model)
if(model$family$family !="poisson" & substr(model$family$family, 1,12) !="Negative Bin") {
	stop("IDR is only for Poisson regression model")
}

idrci <- as.data.frame(s1$coefficients)
colnames(idrci) <- c("IDR", paste("lower",100-100*alpha,"ci",sep=""), paste("upper",100-100*alpha,"ci",sep=""),"P value")
idrci[,3] <- exp(idrci[,1]+qnorm(1-alpha/2)*idrci[,2])
idrci[,2] <- exp(idrci[,1]-qnorm(1-alpha/2)*idrci[,2])
idrci[,1] <- exp(idrci[,1])
cat("\n")                                           
a <- idrci[rownames(idrci) !="(Intercept)",];         
print(round(a, decimal))                                 
cat("\n") 
cat(c("Log-likelihood = ", round(logLik(model),decimal+2)), "\n")
cat("No. of observations = ", length(model$y), "\n")                            
cat("AIC value =", s1$aic, "\n" )       
cat("\n")                                           }

### MH- stratified analysis
mhor <- function(..., mhtable=NULL, decimal=2, graph=TRUE, design="cohort") {
if(is.null(mhtable)) {mhtable <- table(...)}else{mhtable <- as.table(mhtable)}
a <-0
A <-0
Vara <-0
numerator <- 0
denominator <-0
or <- c(1:dim(mhtable)[3])
logse <- c(1:dim(mhtable)[3])
lowlim <- c(1:dim(mhtable)[3])
uplim <- c(1:dim(mhtable)[3])
p.value <- c(1:dim(mhtable)[3])
stratlab <- levels(as.data.frame(mhtable)[,3]) # Vector labelling strata
tabodds <- c(1:(4*length(stratlab)))
dim(tabodds) <- c(length(stratlab), 4)
p <-0; q <-0; r <-0; s <-0; pr <-0; ps <-0; qr <-0; qs <-0; psqr <-0
for (i in 1:dim(mhtable)[3]) 
	{
# OR, ln(SE) and 95 ci for each staratum
	or[i] <- fisher.test(as.table(mhtable[,,i]))$estimate
	lowlim[i] <- fisher.test(as.table(mhtable[,,i]))$conf.int[1]
	uplim[i] <- fisher.test(as.table(mhtable[,,i]))$conf.int[2]
	p.value[i] <- fisher.test(as.table(mhtable[,,i]))$p.value
# Computing MH odds ratio and standard error
	numerator <- numerator+ mhtable[1,1,i]*mhtable[2,2,i]/sum(mhtable[,,i])
	denominator <- denominator+mhtable[1,2,i]*mhtable[2,1,i]/sum(mhtable[,,i])
	p <- p+(mhtable[1,1,i]+mhtable[2,2,i])/sum(mhtable[,,i])
	q <- q+(mhtable[1,2,i]+mhtable[2,1,i])/sum(mhtable[,,i])
	r <- numerator
	s <- denominator
	pr <- pr+(mhtable[1,1,i]+mhtable[2,2,i])/sum(mhtable[,,i])*
		mhtable[1,1,i]*mhtable[2,2,i]/sum(mhtable[,,i])
	ps <- ps+(mhtable[1,1,i]+mhtable[2,2,i])/sum(mhtable[,,i])*
		mhtable[1,2,i]*mhtable[2,1,i]/sum(mhtable[,,i])
	qr <- qr+(mhtable[1,2,i]+mhtable[2,1,i])/sum(mhtable[,,i])*
		mhtable[1,1,i]*mhtable[2,2,i]/sum(mhtable[,,i])
	qs <- qs+(mhtable[1,2,i]+mhtable[2,1,i])/sum(mhtable[,,i])*
		mhtable[1,2,i]*mhtable[2,1,i]/sum(mhtable[,,i])
	psqr <- psqr+(mhtable[1,1,i]+mhtable[2,2,i])/sum(mhtable[,,i])*
		mhtable[1,2,i]*mhtable[2,1,i]/sum(mhtable[,,i])+
		(mhtable[1,2,i]+mhtable[2,1,i])/sum(mhtable[,,i])*
		mhtable[1,1,i]*mhtable[2,2,i]/sum(mhtable[,,i])
# Computing chi-squared
	a <- a+ mhtable[1,1,i]
        A <- A+sum(mhtable[,1,i])*sum(mhtable[1,,i])/sum(mhtable[,,i])
        Vara <- Vara +  sum(mhtable[, 1, i]) / (sum(mhtable[, , i]) - 1) * 
		sum(mhtable[1, , i]) * 
		sum(mhtable[, 2, i]) * 
		sum(mhtable[2, , i]) /
		sum(mhtable[, , i])^2

# Individual stratum
	tabodds[i,] <- c(or[i], lowlim[i], uplim[i], p.value[i])
}
cat("\n")
colnames(tabodds) <- c("OR", "lower lim.", "upper lim.", "P value")
collab <- colnames(as.data.frame(mhtable))[3]
cat("Stratified analysis by ",(collab), "\n")
rownames(tabodds) <- paste(collab, stratlab, "")
mhor <- numerator/denominator
mhlogse <- sqrt(pr/2/r^2 + psqr/2/r/s + qs/2/s^2)
mhlolim <- exp(log(mhor)-qnorm(0.975)*mhlogse)
mhhilim <- exp(log(mhor)+qnorm(0.975)*mhlogse)
chi2 <- abs(a-A)^2/Vara # If needs corrected chisquare: chi2 <-(abs(a-A)-1/2)^2/Vara
mh.p.value <-pchisq(chi2,1, lower.tail=FALSE)
het <- sum((log(or)-log(mhor))^2/(1/mhtable[1,1,]+ 1/mhtable[1,2,]+ 1/ mhtable[2,1,]+ 1/
		mhtable[2,2,]))
p.value.het <- pchisq(het, length(or)-1, lower.tail=FALSE)
tabodds1 <- rbind(tabodds, c(mhor, mhlolim, mhhilim, mh.p.value))
rownames(tabodds1)[dim(tabodds1)[1]] <- "M-H combined"
print(tabodds1, digit=3)
cat("\n")
cat("M-H Chi2(1) =", round(chi2,decimal), ", P value =", round(mh.p.value, decimal+1), "\n")
	mhresults <- list(strat.table=mhtable, mh.or=mhor, ci95=c(mhlolim, mhhilim))
if (any(mhtable==0)){
  cat(paste("\n","One or more cells of the stratified table == 0.","\n",
    "Homogeneity test not computable.","\n","\n"))
  if(graph==TRUE){
    graph <- FALSE
    cat(paste(" Graph not drawn","\n","\n"))
    }
  }else{
    cat("Homogeneity test, chi-squared", dim(tabodds)[1]-1, "d.f. =", round(het,decimal),",",
	  "P value =", round(p.value.het, decimal+1), "\n")
    cat("\n")
  }
# mhresults
if (graph==TRUE){
	caseexp      <- rep(0, dim(mhtable)[3])
	controlex    <- rep(0, dim(mhtable)[3])
	casenonex    <- rep(0, dim(mhtable)[3])
	controlnonex <- rep(0, dim(mhtable)[3])
	logit0       <- rep(0, dim(mhtable)[3])
	se0          <- rep(0, dim(mhtable)[3])
	logit1       <- rep(0, dim(mhtable)[3])
	se1          <- rep(0, dim(mhtable)[3])
	x            <- rep(0, 6*dim(mhtable)[3])
	y            <- rep(0, 6*dim(mhtable)[3])
	for(i in 1:dim(mhtable)[3]){
		caseexp[i] 	<- mhtable[2,2,i]
		controlex[i] 	<- mhtable[1,2,i]
		casenonex[i]	<- mhtable[2,1,i]
		controlnonex[i]	<- mhtable[1,1,i]
	}
	if(design=="case control"||design=="case-control"||design=="casecontrol"){
		for(i in 1:dim(mhtable)[3]){
			logit0[i] <- log(controlex[i]/controlnonex[i]) 
			se0[i]    <- sqrt(1/controlex[i]+1/controlnonex[i])
			logit1[i] <- log(caseexp[i]/casenonex[i])
			se1[i]    <- sqrt(1/caseexp[i]+1/casenonex[i])
			x[(1:6)+(i-1)*6] <- c(c(-1,0,1)*1.96*se0[i] + logit0[i],
				c(-1,0,1)*1.96*se1[i] + logit1[i] )
			y[(1:6)+(i-1)*6] <- c(rep(0+0.025*(i-1),3),rep(1+0.025*(i-1),3))
		}
	
		plot(x,y, xlab="Odds of exposure",yaxt="n", xaxt="n", 
			main="Stratified case control analysis", 
			ylab=paste("Outcome=",colnames(as.data.frame(mhtable))[1],
				", Exposure=",colnames(as.data.frame(mhtable))[2]),pch=" ")
		for(i in 1:dim(mhtable)[3]){
			lines(x[c(1,3)+(i-1)*6],y[c(1,3)+(i-1)*6], col=i+1)
			lines(x[c(4,6)+(i-1)*6],y[c(4,6)+(i-1)*6], col=i+1)
			lines(x[c(2,5)+(i-1)*6],y[c(2,5)+(i-1)*6], col=i+1, lty=2)
			points(x[c(1,3)+(i-1)*6],y[c(1,3)+(i-1)*6], col=i+1, pch="I")
			points(x[c(4,6)+(i-1)*6],y[c(4,6)+(i-1)*6], col=i+1, pch="I")
			points(x[c(2,5)+(i-1)*6],y[c(2,5)+(i-1)*6], col=i+1,pch=22,
				cex=c(controlex[i]+controlnonex[i],caseexp[i]+casenonex[i])/sum(mhtable[,,])*8)
				text(x=(max(x)+min(x))/2, y=0.3+0.1*i, col=dim(mhtable)[3]+2-i, 
					labels=paste(collab, stratlab[dim(mhtable)[3]+1-i],": OR= ",
					round(or[dim(mhtable)[3]+1-i],decimal)," (",round(lowlim[dim(mhtable)[3]+1-i],decimal),", ",
					round(uplim[dim(mhtable)[3]+1-i],decimal),")",sep=""))
		}
		x1 <- exp(x)
		a <- 2^(-10:10)
		if(length(a[a>min(x1) & a<max(x1)]) >2 & length(a[a>min(x1) & a<max(x1)]) <10){
			a1 <- a[a>min(x1) & a<max(x1)]
				if(any(a1>=1))axis(1,at=log(a1[a1>=1]),labels=as.character(a1[a1>=1]))
				if(any(a1<1))axis(1,at=log(a1[a1<1]),labels=paste(as.character(1),"/",
				as.character(trunc(1/a1[a1<1])), sep=""))
		}
		else
		{
			options(digit=2)
			at.x <-  seq(from=min(x),to=max(x),by=((max(x)-min(x))/5))
			labels.oddsx <- exp(at.x)
			axis(1,at=at.x, labels=as.character(round(labels.oddsx,digits=decimal)))
		}
		text(x=(max(x)+min(x))/2, y=.3, labels=paste("MH-OR"," = ",
			round(mhor,decimal)," (",round(mhlolim,decimal),", ",
			round(mhhilim,decimal),")",sep=""))
		text(x=(max(x)+min(x))/2, y=.2, labels=paste("homogeneity test P value"," = ",
			round(p.value.het, decimal+1),sep=""))
		axis(2, at=0.025*(dim(mhtable)[3]-1)/2, labels="Control", las=1)
		axis(2, at=1+0.025*(dim(mhtable)[3]-1)/2, labels="Case", las=1)
	}
	if(design=="cohort" || design=="prospective"){
		for(i in 1:dim(mhtable)[3]){
			logit0[i] <- log(casenonex[i]/controlnonex[i]); se0[i] <-sqrt(1/casenonex[i]+1/controlnonex[i])
			logit1[i] <- log(caseexp[i]/controlex[i]); se1[i] <- sqrt(1/caseexp[i]+1/controlnonex[i])
			y[(1:6)+(i-1)*6] <- c(c(-1,0,1)*1.96*se0[i] + logit0[i],c(-1,0,1)*1.96*se1[i] + logit1[i] )
			x[(1:6)+(i-1)*6] <- c(rep(0+0.025*(i-1),3),rep(1+0.025*(i-1),3))
		}
		plot(x,y, ylab="Odds of outcome",yaxt="n", xaxt="n",
			main="Stratified prospective/X-sectional analysis",
			xlab=paste("Outcome=",colnames(as.data.frame(mhtable))[1],
				", Exposure=",colnames(as.data.frame(mhtable))[2]),pch=" ")
		for(i in 1:dim(mhtable)[3]){
			lines(x[(1:3)+(i-1)*6],y[(1:3)+(i-1)*6], col=i+1)
			lines(x[(4:6)+(i-1)*6],y[(4:6)+(i-1)*6], col=i+1)
			lines(x[c(2,5)+(i-1)*6],y[c(2,5)+(i-1)*6], col=i+1, lty=2)
			lines(x=c(-.02,.02)+0.025*(i-1),y=c(y[1+(i-1)*6],y[1+(i-1)*6]), col=i+1)
			lines(x=c(-.02,.02)+0.025*(i-1),y=c(y[3+(i-1)*6],y[3+(i-1)*6]), col=i+1)
			lines(x=c(.98,1.02)+0.025*(i-1),y=c(y[4+(i-1)*6],y[4+(i-1)*6]), col=i+1)
			lines(x=c(.98,1.02)+0.025*(i-1),y=c(y[6+(i-1)*6],y[6+(i-1)*6]), col=i+1)
			points(x[c(2,5)+(i-1)*6],y[c(2,5)+(i-1)*6], col=i+1, pch=22,
				cex=c((controlnonex[i]+casenonex[i]),(caseexp[i]+controlex[i]))/sum(mhtable[,,])*8)
			text(x=.5, y=0.3*(max(y)-min(y))+min(y)+ 0.1*i*(max(y)-min(y)), col=dim(mhtable)[3]+2-i, 
				labels=paste(collab,stratlab[dim(mhtable)[3]+1-i],": OR = ",
				round(or[dim(mhtable)[3]+1-i],decimal)," (",round(lowlim[dim(mhtable)[3]+1-i],decimal),", ",
				round(uplim[dim(mhtable)[3]+1-i],decimal),")",sep=""))
		}
		text(x=.5, y=.3*(max(y)-min(y))+min(y), labels=paste("MH-OR"," = ",
			round(mhor,decimal)," (",round(mhlolim,decimal),", ",
			round(mhhilim,decimal),")",sep=""))
		text(x=.5, y=.2*(max(y)-min(y))+min(y), labels=paste("homogeneity test P value"," = ",
			round(p.value.het, decimal+1),sep=""))

		axis(1, at=0.025*(dim(mhtable)[3]-1)/2, labels="Non-exposed")
		axis(1, at=1+0.025*(dim(mhtable)[3]-1)/2, labels="Exposed")
		y1 <- exp(y)
		a <- 2^(-10:10)
		if(length(a[a>min(y1) & a<max(y1)]) >2 & length(a[a>min(y1) & a<max(y1)]) <10){
			a1 <- a[a>min(y1) & a<max(y1)]
		if(any(a1>=1)) {axis(2,at=log(a1[a1>=1]),labels=as.character(a1[a1>=1]),las=1)}
		if(any(a<1)) {axis(2,at=log(a1[a1<1]),labels=paste(as.character(1),"/",
			as.character(trunc(1/a1[a1<1])), sep=""),las=1)}
		}
		else
		{
		options(digit=2)
		at.y <-  seq(from=min(y),to=max(y),by=((max(y)-min(y))/5))
		labels.oddsy <- exp(at.y)
		axis(2,at=at.y, labels=as.character(round(labels.oddsy,digits=decimal+1)),las=1)
		}

	}
}
}
#### Logistic regression display

logistic.display <- function(logistic.model, alpha=.05, decimal=3) {
model <- logistic.model
if(class(model)[1]!="glm" | class(model)[2]!="lm" | model$family$family != "binomial"){
	stop("model not from logistic regression")
}
s1 <- summary(model)
orci <- as.data.frame(s1$coefficients)
colnames(orci) <- c("OR", paste("lower",100-100*alpha,"ci",sep=""), paste("upper",100-100*alpha,"ci",sep=""),"P value")
orci[,3] <- exp(orci[,1]+qnorm(1-alpha/2)*orci[,2])
orci[,2] <- exp(orci[,1]-qnorm(1-alpha/2)*orci[,2])
orci[,1] <- exp(orci[,1])
cat("\n")                                           
a <- orci[rownames(orci) !="(Intercept)",];         
print(round(a, decimal))                                 
cat("\n") 
cat(c("Log-likelihood = ", round(logLik(model),decimal+2)), "\n")
cat("No. of observations = ", length(model$y), "\n")                            
cat("AIC value =", s1$aic, "\n" )       
cat("\n")                                           
#return(model)                                       
}

if(1==2){
#### Longitudinal graphing
longitudinal <- function(id,time,var){
y.span <- max(na.omit(var))-min(na.omit(var))
plot(time, var, pch=" ", ylim=c(min(na.omit(var))-0.1*y.span,
	max(na.omit(var))+0.1*y.span), xaxt="n",
	main=paste("Longitudinal values of",as.character(substitute(var))),
	ylab=as.character(substitute(var)) )
axis(side=1,at=unique(time))
# Special lines
mean.var <- aggregate(var, by=list(subject=id), FUN=mean)
mean.var <- na.omit(mean.var)
mean.var <- mean.var[order(mean.var$x,mean.var$subject),]
if(nrow(mean.var)>100){
	if(nrow(mean.var)>150){
		sample.id <- sample(unique(id),size=100)
		for(i in sample.id){
			lines(x=time[id==i], y= var[id==i], lty=2, col="blue")
		}
	} else {
		for(i in unique(id)){
			lines(x=time[id==i], y= var[id==i], lty=2, col="blue")
		}
	}
	id.p.1 <- mean.var$subject[trunc(nrow(mean.var)*.1)]
	id.p.5 <- mean.var$subject[trunc(nrow(mean.var)*.5)]
	id.p.9 <- mean.var$subject[trunc(nrow(mean.var)*.9)]
	lines(x=time[id==id.p.9], y=var[id==id.p.9], lwd=3, col="blue")
	lines(x=time[id==id.p.5], y=var[id==id.p.5], lwd=3, col="red")
	lines(x=time[id==id.p.1], y=var[id==id.p.1], lwd=3, col="brown")
	leg.x <- c(min(time)+.2*(max(time)-min(time)),
		min(time)+.5*(max(time)-min(time)),
		min(time)+.8*(max(time)-min(time)))
	leg.txt <- c("P90","median","P10")
	leg.col <- c("blue","red","brown")
	legend(x=leg.x[1],y=min(na.omit(var))+.03*y.span,
		legend=leg.txt[1], col=leg.col[1], lwd=3, bty="n", pch=" ", xjust=0.5)
	legend(x=leg.x[2],y=min(na.omit(var))+.03*y.span,
		legend=leg.txt[2], col=leg.col[2], lwd=3, bty="n", pch=" ", xjust=0.5)
	legend(x=leg.x[3],y=min(na.omit(var))+.03*y.span,
		legend=leg.txt[3], col=leg.col[3], lwd=3, bty="n", pch=" ", xjust=0.5)
}
if(nrow(mean.var)<10){
	for(i in unique(id)){
	lines(x=time[id==i], y= var[id==i], lty=2, col=i)
	}
}
}
}

#### Likelihood ratio test
lrtest <- function(model1, model2) {
# Check class of model
if(any(class(model1)!=class(model2))){stop("Two models have different classes")}

# conditional logistic regression & Cox regression
if(any(class(model1)=="coxph") & any(class(model2)=="coxph")){
if(model1$n != model2$n){stop("Two models has different sample sizes")}
cat("\n")
df1 <- length(model1$coefficients)
# print(df1)
df2 <- length(model2$coefficients)
# print(df2)
lrt <- 2*(model2$loglik[2] - model1$loglik[2])
diff.df <- df2-df1
if(lrt <0){
	lrt <- -lrt
	diff.df <- -diff.df
}
if(lrt*diff.df <0){stop("Likelihood gets worse with more variables. Test not executed")}
cat("Likelihood ratio test for Cox regression & conditional logistic regression","\n")
cat("Chi-squared", diff.df, "d.f. = ", lrt,",",
	"P value = ", round(pchisq(lrt, diff.df, lower.tail = FALSE),4), "\n") 
cat("\n")
}

# Multinomial logistic regression & ordinal regression
if(any(class(model1)=="multinom") & any(class(model2)=="multinom")){
if(any(dim(model1$residuals)!=dim(model2$residuals))){stop("Two models have different outcomes or different sample sizes")}
cat("\n")
df1 <- model1$edf
# print(df1)
df2 <- model2$edf
# print(df2)
lrt <- model2$deviance - model1$deviance
diff.df <- df1-df2
if(lrt <0){
	lrt <- -lrt
	diff.df <- -diff.df
}
if(lrt*diff.df <0){stop("Likelihood gets worse with more variables. Test not executed")}
cat("Likelihood ratio test for multinomial logistic regression","\n")
cat("Chi-squared", diff.df, "d.f. = ", lrt,",",
	"P value = ", round(pchisq(lrt, diff.df, lower.tail = FALSE),4), "\n") 
cat("\n")
}

# Ordinal regression
if(any(class(model1)=="polr") & any(class(model2)=="polr")){
if(model1$n != model2$n){stop("Two models have different outcomes or different sample sizes")}
cat("\n")
df1 <- model1$edf
# print(df1)
df2 <- model2$edf
# print(df2)
lrt <- model2$deviance - model1$deviance
diff.df <- df1-df2
if(lrt <0){
	lrt <- -lrt
	diff.df <- -diff.df
}
if(lrt*diff.df <0){stop("Likelihood gets worse with more variables. Test not executed")}
cat("Likelihood ratio test for ordinal regression","\n")
cat("Chi-squared", diff.df, "d.f. = ", lrt,",",
	"P value = ", round(pchisq(lrt, diff.df, lower.tail = FALSE),4), "\n") 
cat("\n")
}



# unconditional logistic regression
if(all(class(model1)==c("glm","lm")) & all(class(model1)==c("glm","lm"))){
if(sum(model1$df.null) != sum(model2$df.null)) stop("Number of observation not equal!!") 
df1 <- attributes(logLik(model1))$df
df2 <- attributes(logLik(model2))$df
lrt <- 2*(as.numeric(logLik(model2) - logLik(model1)))
diff.df <- df2-df1
if(lrt <0){
	lrt <- -lrt
	diff.df <- -diff.df
}
if(lrt*diff.df <0){stop("Likelihood gets worse with more variables. Test not executed")}
cat("Likelihood ratio test for MLE method","\n")
cat("Chi-squared", diff.df, "d.f. = ", lrt,",",
	"P value = ", round(pchisq(lrt, diff.df, lower.tail = FALSE),4), "\n") 
cat("\n")
}
}
### List objects excluding function
ls.nofunction <- function() {
	y <- ls(envir= .GlobalEnv)
	vector1 <- character(0)
if(length(y)==0){vector1 <- character(0)}else{
	for (i in 1:length(y)){
	if(substring(deparse(get(y[i]))[1],first=1,last=8)!="function") {
		vector1 <- c(vector1,y[i])
		}
	}
}
	return(vector1)
}

### Ordinal odds ratio display
ordinal.or.display <- function(ordinal.model, decimal=3, alpha=.05){
model <- ordinal.model
if(class(model) !="polr") stop("The model is not an ordinal logistic regression model")
s1 <- summary(model)
t <- s1$coefficients[,3]
df <- s1$df.residual
p.value <- pt(abs(t), df, lower.tail=FALSE)
coeff <- t(t(model$coefficients))
coeff.95ci <- cbind(coeff, confint(model, level=1-alpha))
oor.95ci <- round(exp(coeff.95ci),decimal)
len.p <- length(p.value) 
oor.95ci <- cbind(oor.95ci, format(p.value[-(len.p:(len.p-1))],digits=decimal))
colnames(oor.95ci) <- c("Ordinal OR", paste("lower",100-100*alpha,"ci",sep=""), paste("upper",100-100*alpha,"ci",sep=""),"P value")
print.noquote(oor.95ci)
}


### Summarize continous variable in the loaded data set
summ <- function (x=.data, by=NULL, graph=TRUE, box=FALSE) {
###	Graph function here
	if(!is.atomic(x)) {graph=FALSE}
	if(graph==TRUE){
		if(typeof(x)=="character"){stop(paste(as.character(substitute(x)),"is a character vector"))}
		var1 <- as.character(substitute(x))
		if(length(var1)>1){
			string2 <- var1[length(var1)]	
		}else
		if(substring(search()[2],first=1,last=8)!="package:"){
			string2 <-  attr(get(search()[2]), "var.labels")[attr(get(search()[2]), "names")==substitute(x)]
			if(length(string2)==0){
				string2 <- as.character(substitute(x))
			}
			if(string2==""){
				string2 <- as.character(substitute(x))
			}
		}else{
			string2 <- as.character(substitute(x))
		}
		string3 <- paste(title.string()$distribution.of,string2)
		if(substring(search()[2],first=1,last=8)!="package:"){
			string4 <-  attr(get(search()[2]), "var.labels")[attr(get(search()[2]), "names")==substitute(by)]
			if(length(string4)==0){
				string4 <- as.character(substitute(by))
			}else{
				if(string4==""){
					string4 <- as.character(substitute(by))
				}
			}
		}else{
			string4 <- as.character(substitute(by))
		}
		string5 <- paste(string3,title.string()$by,string4)
#		if(length(grep("Thai",Sys.getlocale("LC_ALL")))==1){string5<-paste(string3,"¨Óá¹¡µÒÁ",string4)}
#		if(length(grep("People's Republic of China",Sys.getlocale("LC_ALL")))==1){string5<-paste(string3,"Óë",string4)}
#		if(length(grep("Malay",Sys.getlocale("LC_ALL")))==1){string5<-paste(string3,"mengikut",string4)}
#		if(length(grep("Korean",Sys.getlocale("LC_ALL")))==1){string5<-paste(string3,"Ëæ Åà¶£²÷",string4)}

		## Defining pretty x ticking for date and time
		if(any(class(x)=="Date")) {
			range.date <- difftime(summary(x)[6], summary(x)[1])
			numdate <- as.numeric(range.date)
			if(numdate <1){stop(paste("Only one day ie.",format(x,"%Y-%m-%d"),"not suitable for plotting"))}
			if(numdate >=1  & numdate <10){date.pretty <- seq(from=summary(x)[1],to=summary(x)[6],by="day"); format.time <- "%a%d%b"}
#			if(numdate >=10 & numdate <30){date.pretty <- seq(from=summary(x)[1],to=summary(x)[6],by="7 day"); format.time <- "%d%b"}
			if(numdate >=10 & numdate <60){date.pretty <- seq(from=summary(x)[1],to=summary(x)[6],by="week"); format.time <- "%d%b"}
			if(numdate >=60 & numdate <700){
				date.pretty <- seq(from=(summary(x)[1]-as.numeric(substr(as.character(summary(x)[1]),9,10))+1),
				to=summary(x)[6],by="month"); format.time <- "%b%y"
			}
		}
		if(any(class(x)=="POSIXt")){
			range.time <- difftime(summary(x)[6],summary(x)[1])
			numeric.time <- as.numeric(range.time)
			units <- attr(range.time, "units")
			if(units=="secs")  {step <- "sec"; format.time <- "%M:%S";  scale.unit <- "min:sec"}
			if(units=="mins")  {step <- "min"; format.time <- "%H:%M";  scale.unit <- "HH:MM"}
			if(units=="hours") {step <- ifelse(numeric.time<2,"20 mins","hour");format.time <- "%H:%M";  scale.unit <- "HH:MM"}
			if(units=="days")  {
				if(numeric.time <2){
					step <- "6 hour"; format.time <- "%a %H:%M";  scale.unit <- "HH:MM"
				}else{
					step <- "day"; format.time <- "%d%b%y"; scale.unit <- "Date"
				}
			}
			if(units=="weeks") {step <- "week";format.time <- "%b%y";   scale.unit <- " "}
			time.pretty <- seq(from=summary(x)[1],to=summary(x)[6],by=step)
		}

		if(!is.null(by)){
			x1 <- x[order(by,as.numeric(x))] 
			by1 <- by[order(by,as.numeric(x))]; 
			by2 <- factor(by1, exclude=NULL)
			character.length <- ifelse(max(nchar(levels(by2)))>8, max(nchar(levels(by2)))*(60-max(nchar(levels(by2))))/60, max(nchar(levels(by2)))*1.2)
			left.offset <- max(c(0.76875+.2, .1+par()$cin[1]*character.length))
			par(mai=c(0.95625, left.offset, 0.76875, 0.39375))
			by3 <- max(as.numeric(by2))- as.numeric(by2) +1
			y0 <- 1:length(x1)
			y <- suppressWarnings(y0 + as.numeric(by2)-1)#Note that y0 and by2 may have different length
			if(is.factor(x)){
				plot(as.numeric(x1),y, pch=18, col=by3, main=string5, ylim=c(-1,max(y)),
				xlab=" ",ylab=" ", yaxt="n", xaxt="n" )
				axis(1, at=1:length(levels(x1)),labels=levels(x1))
			} else
			if(any(class(x)=="POSIXt")){
				plot(x1,y, pch=18, col=by3, main=string5, ylim=c(-1,max(y)),
					xlab=" ",ylab=" ", yaxt="n", xaxt="n")
				axis(1, at=time.pretty, labels=as.character(time.pretty,format=format.time))
			}else
			if(class(x)=="Date"){
				if(numdate < 700){
					plot(x1,y, pch=18, col=by3, main=string5, ylim=c(-1,max(y)),
						xlab=" ",ylab=" ", yaxt="n", xaxt="n")
					axis(1, at=date.pretty, labels=as.character(date.pretty,format=format.time))
				}else{
					plot(x1,y, pch=18, col=by3, main=string5, ylim=c(-1,(summary(y))[6]),
						xlab=" ",ylab=" ", yaxt="n")
				}
			}else{
				plot(x1,y, pch=18, col=by3, main=string5, ylim=c(-1,max(y)),
					xlab=" ", ylab=" ", yaxt="n")
				if(class(x)=="difftime"){unit <-attr(x,"unit")} else {unit<-" "}
				title(xlab=unit)
			}
			if(length(x1)<20){abline(h=y, lty=3)}
			yline <- NULL
			if(any(is.na(levels(by2)))){levels(by2)[length(levels(by2))]<-"missing"}
 			for(i in 1:length(levels(by2))){
				yline <- c(yline, sum(as.numeric(by2)==i))
			}
			yline <- c(0,cumsum(yline)[1:(length(yline)-1)])+(0:(length(yline)-1)) 
				abline(h=yline, col="blue")
				axis(2,at=yline, labels=levels(by2), padj=0, las=1)
			par(mai=c(0.95625, 0.76875, 0.76875, 0.39375))
		}else{
			x1 <- x[order(x)]; y <- 1:length(x1)
			if(is.factor(x1)){
				plot(as.numeric(x1),y, pch=18, col="blue", main=string3,
					xlab=" ",ylab="Subjects sorted by X-axis values ",  xaxt="n")
				axis(1, at=1:length(levels(x1)),labels=levels(x1))
			} else
			if(any(class(x)=="POSIXt")){
				plot(x1,y, pch=18, col="blue", main=string3,
					xlab=" ",ylab="Subject sorted by X-axis values", xaxt="n")
				axis(1, at=time.pretty, labels=as.character(time.pretty,format=format.time))
			}else
			if(class(x)=="Date"){
				if(numdate < 700){
					plot(x1,y, pch=18, col="blue", main=string3,
						xlab=" ",ylab="Subject sorted by X-axis values", yaxt="n", xaxt="n")
					axis(1, at=date.pretty, labels=as.character(date.pretty,format=format.time))
				}else{
					plot(x1,y, pch=18, col="blue", main=string3,
						xlab=" ",ylab="Subject sorted by X-axis values", yaxt="n")
				}
			}else{
				plot(x1,y, pch=18, col="blue", main=string3,
					xlab=" ", ylab="Subject sorted by X-axis values", yaxt="n")
				if(class(x)=="difftime"){unit <-attr(x,"unit")} else {unit<-" "}
				title(xlab=unit)
			}
			
			if(length(x1)<30){abline(h=y,lty = 3)}
			if(box==TRUE){
				boxplot(unclass(x1), add=TRUE, horizontal=TRUE, 
					axes=FALSE, at=.8*length(sort(x1)),boxwex=.2*length(sort(x1)) )
			}
		}
	}
	if(is.data.frame(x)){
		cat ("\n")
		cat(attr(x, "datalabel"), "\n")
		cat("No. of observations = "); cat(nrow(x), "\n")
	}
	if(is.vector(x) | is.vector(unclass(x))|(is.factor(x))|any(class(x)=="POSIXt"|class(x)=="difftime")){
		if(typeof(x)=="character"){stop(paste(as.character(substitute(x)),"is a character vector"))}
		if(is.factor(x)) {x <- na.omit(as.numeric(x))}
	###	print out statistics
		if(!is.null(by)){
			by1 <- factor(by, exclude=NULL)
			if(any(is.na(levels(by1)))){levels(by1)[length(levels(by1))]<-"missing"}
			lev <- levels(by1)
			for(i in 1:length(lev)) {
				x1 <- subset(x, by1==lev[i])
				if(any(class(x1)=="POSIXt")) {
					cat(paste("For",as.character(substitute(by)),"=",levels(by1)[i]),"\n")
					print.noquote(format((summary(x1))[c(1,3,4,6)],"%Y-%m-%d %H:%M"))
					cat("\n")
				}else{
					a <- rep("",6); dim(a) <- c(1,6)
					if(class(x1)=="Date"){
						a[1,] <- c(length(x1),format(c(summary(x1)[4],summary(x1)[3],NA,summary(x1)[1],summary(x1)[6]),"%Y-%m-%d"))
					}else
					if(class(x)=="logical"){
					a[1,] <- round(c(length(na.omit(x1)),mean(na.omit(x1)),
						quantile(na.omit(x1), .5), ifelse(is.na(mean(na.omit(x1))), NA,round(sd(na.omit(x1)),2))  , 
					min(na.omit(x1)), max(na.omit(x1)) ),3 )
					}	
					else{
						a[1,] <- round( c(length(na.omit(x1)), summary(x1)[4], summary(x1)[3],
							ifelse(is.na(mean(na.omit(x1))), NA,sd(na.omit(x1)))  , 
						summary(x1)[1],summary(x1)[6]),3 )
					}
					colnames(a) <- c("Obs.  ", "mean  ", "median ", "s.d.  ", "min.  ", "max.  ")
					rownames(a) <- " "	
					cat(paste("For",as.character(substitute(by)),"=",levels(by1)[i]),"\n")
					print.noquote(a, row.names=NULL)
					cat("\n")
				}
			}
		}else{
			if(any(class(x)=="POSIXt")) {
				print.noquote(format((summary(x))[c(1,3,4,6)],"%Y-%m-%d %H:%M"))
			}else{
				a <- rep("",6); dim(a) <- c(1,6)
				if(class(x)=="Date"){
					a[1,] <- c(length(x),format(c(summary(x)[4],summary(x)[3],NA,summary(x)[1],summary(x)[6]),"%Y-%m-%d"))
				}else

				if(class(x)=="difftime"){
					a[1,] <- c(length(na.omit(x)), summary(x)[4],summary(x)[3],
						ifelse(is.na(mean(na.omit(x1))), NA,round(sd(na.omit(x1)),2)),
						summary(x)[1],summary(x)[6])
				}else{
					a[1,] <- round(c(length(na.omit(x)),mean(na.omit(x)),
						quantile(na.omit(x), .5), ifelse(is.na(mean(na.omit(x))), NA,round(sd(na.omit(x)),2))  , 
					min(na.omit(x)), max(na.omit(x)) ),3 )
				}
				colnames(a) <- c("Obs.  ", "mean  ", "median ", "s.d.  ", "min.  ", "max.  ")
				rownames(a) <- " "	
				print.noquote(a, row.names=NULL)
			}
		}
	}
else
if (is.recursive(x)&&length(x)==1) summary(x)
else
if (!is.recursive(x)&&!is.vector(x)&&!is.factor(x)) summary(x)
else
{
a <- rep("", (dim(x)[2])*7)
dim(a) <- c(dim(x)[2], 7)
colnames(a) <- c("Var. name", "Obs.  ", "mean  ", "median ", "s.d.  ",  "min.  ", "max.  ")
a[,1] <- attr(x, "names")
rownames(a) <- 1:nrow(a)
for(i in 1:(dim(x)[2])) {
	if ((typeof(x[i][1,])=="character")|| is.na(mean(na.omit(as.numeric(x[[i]]))))   )  
	{a[i,3:7] <- ""}
	else
####
	if (any(class(x[[i]])=="Date")){
	a[i,c(3,4,6,7)] <- format(c(summary(x[[i]])[4],summary(x[[i]])[3],
			summary(x[[i]])[1],summary(x[[i]])[6]), "%Y-%m-%d")
	a[i,5] <- NA 
	a[i,2] <- length((x[[i]])[!is.na(x[[i]])])

}
	else
	if (any(class(x[[i]])=="POSIXt")){
#if(1==0){
	a[i,c(3,4,6,7)] <- format(c(summary(x[[i]])[4],summary(x[[i]])[3],
			summary(x[[i]])[1],summary(x[[i]])[6]), "%Y-%m-%d %H:%M")
	a[i,5] <- NA 
	a[i,2] <- length((x[[i]])[!is.na(x[[i]])])

}
	else

	if (is.integer(x[[i]])||is.numeric(x[[i]])|is.logical(x[[i]])){
	a[i,3:7] <- round(c(mean(na.omit(x[[i]])),
			quantile(na.omit(x[[i]]), .5), sd(na.omit(x[[i]])), 
			min(na.omit(x[[i]])), max(na.omit(x[[i]]))),2)
	a[i,2] <- as.character(length(na.omit(as.numeric(x[[i]]))))

}
	else
	if (is.null(class(x[[i]]))) {
	a[i,3:7] <- round(c(mean(na.omit(as.numeric(x[[i]]))),
			quantile(na.omit(as.numeric(x[[i]])), .5), sd(na.omit(as.numeric(x[[i]]))), 
			min(na.omit(as.numeric(x[[i]]))), max(na.omit(as.numeric(x[[i]])))),2)
	a[i,2] <- as.character(length(na.omit(x[[i]])))
}
	else
	if  (is.factor(x[i][2,])){
	a[i,2] <- as.character(length(na.omit(x[[i]])))
	a[i,3:7] <- round(c(mean(na.omit(unclass(x[i][,]))), 
			median(na.omit(unclass(x[i][,]))), 
			sd(na.omit(unclass(x[i][,]))), 
			min(na.omit(unclass(x[i][,]))), 
			max(na.omit(unclass(x[i][,])))),3)
}
}
	
cat("\n")
print.noquote (a[,], digits=3)
cat("\n")
}
}

#### ROC curve from Logistic Regression
lroc <- function (logistic.model, table=FALSE) {
table(logistic.model$fitted.values,logistic.model$y) -> firsttable
cat("\n")
if(table) {cat("Table of observed group frequency by predicted probability")}
colnames(firsttable) <- c("Non-diseased","Diseased")
rownames(firsttable) <- substr(rownames(firsttable), 1,6)
if(table){print.noquote(firsttable)}
cat("\n")
secondtable <- firsttable
for(i in 1:length(secondtable[,1]))
	{
	secondtable[i,1]<-(sum(firsttable[,1])-sum(firsttable[(1:i),1]))/
			sum(firsttable[,1])
	secondtable[i,2]<-(sum(firsttable[,2])-sum(firsttable[(1:i),2]))/
			sum(firsttable[,2])
	}
secondtable <- rbind((c(1,1)),secondtable)
colnames(secondtable) <- c("1-Specificity","Sensitivity")
cat("\n")
if(table){print(round(secondtable, digits=4))}
cat("\n")
## Area under the curve
auc <- 0
for(i in 1:(nrow(secondtable)-1)) {
	auc <- auc+ (secondtable[i,1]-secondtable[(i+1),1])*
		.5*(secondtable[i,2]+secondtable[(i+1),2])
	}
cat("Number of observations =", length(logistic.model$y), "\n")
cat("Area under the curve   =", round(auc,4), "\n")
cat("\n")
plot(secondtable[,1],secondtable[,2], xlab="1-Specificity",
		ylab="Sensitivity", xlim=(c(0,1)), 
		ylim=(c(0,1)), asp=1, 
		main =paste("logit (", deparse(logistic.model$formula),")",sep=""),
		 )
lines(secondtable[,1],secondtable[,2], col="red")
lines(x=c(0,1),y=c(0,1), lty=2, col="blue")
abline(v=0, lty=2, col="blue")
abline(v=.2, lty=2, col="blue")
abline(v=.4, lty=2, col="blue")
abline(v=.6, lty=2, col="blue")
abline(v=.8, lty=2, col="blue")
abline(v=1, lty=2, col="blue")
abline(h=0, lty=2, col="blue")
abline(h=.2, lty=2, col="blue")
abline(h=.4, lty=2, col="blue")
abline(h=.6, lty=2, col="blue")
abline(h=.8, lty=2, col="blue")
abline(h=1, lty=2, col="blue")
auclabel <- paste("Area under the curve =", round(auc, 3))
text(1,.1, pos=2, auclabel)
returns <- list(auc=auc, formula=formula)
}
### ROC curve from a table
roc.from.table <- function(table, graph=TRUE) {
if (dim(table)[2] !=2) stop("There must be 2 columns")
if (table[1,1]/table[1,2] < table[nrow(table),1]/table[nrow(table),2]) {
	stop("At higher cut-off point, there should be more non-diseased")
	}
cat("Table of observed group frequency", "\n")
firsttable <- table
colnames(firsttable) <- c("Non-diseased","Diseased")
if(length(rownames(firsttable))==0) {
	rownames(firsttable) <- rep("", times=nrow(firsttable))
}
print.noquote(firsttable)
cat("\n")
secondtable <- firsttable
for(i in 1:length(secondtable[,1]))
	{
	secondtable[i,1]<-(sum(firsttable[,1])-sum(firsttable[(1:i),1]))/
			sum(firsttable[,1])
	secondtable[i,2]<-(sum(firsttable[,2])-sum(firsttable[(1:i),2]))/
			sum(firsttable[,2])
	rownames(secondtable)[i] <- paste(">",rownames(secondtable)[i])
	}
secondtable <- rbind((c(1,1)),secondtable)
colnames(secondtable) <- c("1-Specificity","Sensitivity")
cat("\n")
print(round(secondtable, digits=4))
cat("\n")
## Area under the curve
auc <- 0
for(i in 1:(nrow(secondtable)-1)) {
	auc <- auc+ (secondtable[i,1]-secondtable[(i+1),1])*
		.5*(secondtable[i,2]+secondtable[(i+1),2])
	}
cat("Number of observations =", sum(table), "\n")
cat("Area under the curve   =", round(auc,4), "\n")
cat("\n")
if(graph==TRUE){
	plot(secondtable[,1],secondtable[,2], xlab="1-Specificity",
		ylab="Sensitivity", xlim=(c(0,1)), 
		ylim=(c(0,1)), asp=1, 
		main = "ROC curve of the diagnostic table")
	lines(secondtable[,1],secondtable[,2], col="red")
	lines(x=c(0,1),y=c(0,1), lty=2, col="blue")
	abline(v=0, lty=2, col="blue")
	abline(v=.2, lty=2, col="blue")
	abline(v=.4, lty=2, col="blue")
	abline(v=.6, lty=2, col="blue")
	abline(v=.8, lty=2, col="blue")
	abline(v=1, lty=2, col="blue")
	abline(h=0, lty=2, col="blue")
	abline(h=.2, lty=2, col="blue")
	abline(h=.4, lty=2, col="blue")
	abline(h=.6, lty=2, col="blue")
	abline(h=.8, lty=2, col="blue")
	abline(h=1, lty=2, col="blue")
	auclabel <- paste("Area under the curve =", round(auc, 3))
	text(0,.95, pos=4, auclabel)
}
returns <- list(auc=auc, diagnostic.table=secondtable)
}

### Kappa statistics
kap <- function(kaptable,wttable=NULL) {
if (ncol(kaptable) != nrow(kaptable)) stop("Column & row not equal length")
if (is.null(wttable)) {
	wttable <- kaptable
	wttable[]<- 0
	for (i in 1:nrow(kaptable)) wttable[i,i]<- 1
	}
po <- 0; pe <- 0
exptable <- kaptable
bigbracket <- 0
wbari <- rep(0, ncol(kaptable)) # marginal mean weight of rows
wbarj <- rep(0, nrow(kaptable)) # marginal mean weight of columns
for(i in 1:nrow(kaptable)) {
	for(j in 1:ncol(kaptable)) {
	wbari[i] <- wbari[i] + wttable[i,j]*sum(kaptable[,j])/sum(kaptable)
	}
}
for(j in 1:ncol(kaptable)) {
	for(i in 1:nrow(kaptable)) {
	wbarj[j] <- wbarj[j] + wttable[i,j]*sum(kaptable[i,])/sum(kaptable)
	}
}
for(i in 1:nrow(kaptable)) {
	for (j in 1:ncol(kaptable)) {
		po <- po + wttable[i,j]*kaptable[i,j]/sum(kaptable)
		exptable[i,j] <- sum(kaptable[i,])*
			sum(kaptable[,j])/sum(kaptable)/sum(kaptable)

		pe <- pe + wttable[i,j]*exptable[i,j]


		bigbracket <- bigbracket + exptable[i,j]*
			( wttable[i,j] - (wbari[i]+ wbarj[j]))^2
		}
	}
kap <- (po-pe)/(1-pe)
if(length(colnames(kaptable)) == 0){
rownames(kaptable) <- paste("Group",as.character(1:nrow(kaptable)), sep="")
colnames(kaptable) <- rownames(kaptable)
attr(attr(kaptable, "dimnames"),"names") <- c("Rater A", "Rater B")
cat("\n")
print(kaptable)
}else{
print(kaptable)}
cat("\n")
cat("Observed agreement =",round(po*100,2),"%","\n")
cat("Expected agreement =",round(pe*100,2),"%","\n")
cat("Kappa =",round(kap,3), "\n")
sekap <- 1/(1-pe)/sqrt(sum(kaptable))*sqrt(bigbracket-pe^2)
z <- kap/sekap
p.value <- pnorm(z, lower.tail=FALSE)
if (p.value < 0.001) {P.value <- "< 0.001"} else
			{P.value <- as.character(round(p.value, 3))}
cat("Standard error =", round(sekap,digits = 3), 
	", Z =", round(z, digits = 3), 
	", P value =", P.value ,"\n","\n")
returns <- list(po=po, pe=pe, kappa=kap, std.error=sekap, z=z, p.value=p.value)
}
### Make 2 x 2 table
make2x2 <- function(caseexp, controlex, casenonex, controlnonex) {
table <- c(controlnonex, controlex, casenonex, caseexp)
dim(table) <- c(2,2)
rownames(table) <- c("Non-diseased", "Diseased")
colnames(table) <- c("Non-exposed","Exposed")
return <- as.table(table)
}

### Sample size calculation
n.for.2p <- function (p1, p2, alpha=0.05, power=.8, ratio=1) {
	if (p1 <1 & p2 <1) {
	cat("\n")
	cat("Estimation of sample size for testing Ho: p1==p2", "\n")
	cat("Assumptions:", "\n", "\n")
	cat("     alpha =", alpha, "\n")
	cat("     power =", power, "\n")
	cat("        p1 =", p1, "\n")
	cat("        p2 =", p2, "\n")
	cat("     n2/n1 =", ratio, "\n", "\n")
	cat("Estimated required sample size:", "\n", "\n")
	r1    <- ratio +1
	pbar  <- (p1+ratio*p2)/r1
	sqrt1 <- sqrt(r1 * pbar * (1-pbar))
	sqrt2 <- sqrt(ratio * (p1 * (1-p1))+ p2*(1-p2)  )
	n0    <- (((qnorm(1-alpha/2)*sqrt1) - (qnorm(1-power)*sqrt2))^2)/
		 (ratio*((p2-p1)^2))
	n1    <- (n0/4)* (1+sqrt(1+2*r1/(n0*ratio*abs(p1-p2))))^2
	n1    <- trunc(n1) +1
	n2    <- trunc(ratio * n1)
	cat("        n1 =",n1,"\n")
	cat("        n2 =",n2,"\n")
	cat("   n1 + n2 =",n1+n2,"\n","\n")
	}
} 

### sample size for survey
n.for.survey <- function(p, delta=.5*min(c(p,1-p)), popsize=FALSE, deff=1, alpha = .05 ){
	if(any(p >= 1) | any(delta >= 1) | any(popsize < 2 & popsize) ) 
		stop("Proportion and delta both must < 1. Popsize must be >=2")
	else {
	n1 <- qnorm(1-alpha/2)^2*p*(1-p)/delta^2
	cat("\n")
	cat("Sample size for survey.","\n")
	cat("Assumptions:", "\n")
	cat("  Proportion       =", p, "\n")
	cat("  Confidence limit =", round((1-alpha)*100), "%","\n") 
	cat("  Delta            =", round(delta, 3), "from the estimate.", "\n")
	}
	if (popsize != FALSE){
	n1 = n1/(1+n1/popsize)
	cat("  Population size  =", popsize, "\n")
	}
	if (deff != 1) {
	n1 = n1*deff
	cat("  Design effect    =", deff, "\n")
	}
	cat("\n")
	cat("  Sample size      =", round(n1), "\n")
	cat("\n")
}

### Sample size for lot quality assurance sampling

n.for.lqas <- function(p0, q=0, N=10000, alpha=.05, exact=FALSE){
if (exact) {
# Hypergeometric distribution 2-by-2 table : See `help("Hypergeometric")'
# 
#   q   n-x      n
# k-q   m-(k-q)  m
#   k   N-k      N
# where N = population
#       n = sample size
#       q = positive among sample
#       k = total positive in the population

	for(n in N:1){
	m = N-n
	k = trunc(p0*N)
	if (dhyper(q, n, m, k) > alpha) break	
	}
	method = "Exact"
}


else {
# For normal approximation calculation
# Formula: d=n*p0-z*sqrt(n*p0*(1-p0)*(N-n)/(N-1))
	for (n in N:1){
	if ((n*p0-(qnorm(p=1-alpha))*sqrt(n*p0*(1-p0)*(N-n)/(N-1))- q) < .001) break
	}
	method = "Normal approximation"

}
cat("\n")
cat("     Lot quality assurance sampling","\n","\n")
cat(c("                             Method =", method, "\n"))
cat(c("                    Population size =", N,"\n"))
cat("  Maximum defective sample accepted =", q, "\n")
cat("     Probability of defect accepted =", p0,"\n")
cat("                              Alpha =", alpha, "\n")
cat(c("               Sample size required =", trunc(n)+1, "\n","\n"))
}
### Sample size for test of two means
n.for.2means <- function (mu1, mu2, sd1, sd2, ratio=1, alpha=.05,
	power=.8) {
	cat("\n")
	cat("Estimation of sample size for testing Ho: mu1==mu2", "\n")
	cat("Assumptions:", "\n", "\n")
	cat("     alpha =", alpha, "\n")
	cat("     power =", power, "\n")
	cat("       mu1 =", mu1, "\n")
	cat("       mu2 =", mu2, "\n")
	cat("       sd1 =", sd1, "\n")
	cat("       sd2 =", sd2, "\n", "\n")
	cat("Estimated required sample size:", "\n", "\n")
	n1 <- (sd1^2+sd2^2/ratio)*(qnorm(1-alpha/2)-qnorm(1-power))^2/(mu1-mu2)^2
	n1 <- round(n1)
	n2 <- ratio * n1
	cat("        n1 =",n1+1,"\n")
	cat("        n2 =",n2+1,"\n")
	cat("   n1 + n2 =",n1+n2+2,"\n","\n")
}
### Pack all related variables into the existing .data
pack <- function(data.frame=.data){
data1 <- data.frame
j <- NULL
k <- attr(data1, "var.labels")
for(i in 1:length(ls.nofunction())){
	if(length(ls.nofunction())==0) stop("No related vector outside the default data frame")
	if(!is.list(get(ls.nofunction()[i])) && (length(get(ls.nofunction()[i]))==nrow(data1)) 
		&& ls.nofunction()[i]!=i){
		if(any(names(data1)==ls.nofunction()[i])){
			data1[,names(.data)==ls.nofunction()[i]] <- get(ls.nofunction()[i])
		}else{
			data1 <- as.data.frame(cbind(data1,get(ls.nofunction()[i])))
			names(data1)[ncol(data1)] <- ls.nofunction()[i]
			j <- c(j,i)
			if(!is.null(k)){ k <- c(k,"")} 
		}
		if(!is.null(attr(.data,"var.labels"))){attr(data1, "var.labels")<-k}
		}
	}
	detach(.data)
	rm(list=ls.nofunction()[j], pos=1)
	.data <<- data1
	attach(.data, warn.conflicts=FALSE)
}
### Power calcuation
power.for.2means <- function (mu1, mu2, n1, n2, sd1, sd2, alpha=.05) {
	if(mu1 >mu2) stop("Please make mu2 > m1")
	cat("\n")
	cat("     alpha =", alpha, "\n")
	cat("       mu1 =", mu1, "\n")
	cat("       mu2 =", mu2, "\n")
	cat("        n1 =", n1, "\n")
	cat("        n2 =", n2, "\n")
	cat("       sd1 =", sd1, "\n")
	cat("       sd2 =", sd2, "\n")
	pooled.sd <- sqrt(sd1^2/n1 + sd2^2/n2)
	power     <- pnorm((mu2-mu1)/pooled.sd - qnorm(1-alpha/2))
	cat("\n","     power =", round(power,4), "\n")
	diffmu <- seq(-2*pooled.sd,2*pooled.sd+(mu2-mu1), by=.01*(mu2-mu1))
	h0 <- dnorm(diffmu, mean=0, sd =	pooled.sd)
	ha <- dnorm(diffmu, mean=(mu2-mu1), sd = pooled.sd)
	plot(diffmu, h0, type = "l", xlim=c(-2*pooled.sd, 2*pooled.sd+(mu2-mu1)),
		main=paste("Power =", round(power,4)), ylab="", xlab="mu2-mu1")
	lines(diffmu, ha, type = "l")
	check.point <- qnorm(1-alpha/2)*pooled.sd
	for(i in seq(from=check.point, to=2*pooled.sd+(mu2-mu1), 
		by=(max(diffmu)-min(diffmu))/50)) {
	lines(c(i,i),
		c(0, dnorm(i, mean=(mu2-mu1), sd = pooled.sd)),
		col="blue")
	}
	text(max(diffmu),max(h0),paste("mu1 = ",mu1,", mu2 = ",mu2, sep=""),pos=2)
	text(max(diffmu),.9*max(h0),paste("sd1 = ",sd1,", sd2 = ", sd2, sep=""),pos=2)
	text(max(diffmu),.8*max(h0),paste("n1 = ",n1, ", n2 = ",n2, sep=""),pos=2)
	text(0,.5*max(h0), paste("Ho: mu2-mu1=0"), col="brown", font=4)
	text(mu2-mu1, .4*max(h0), paste("Ha: mu2 - mu1 =", mu2-mu1), col="brown", font=4)
}

power.for.2p <- function (p1, p2, n1, n2, alpha=.05) {
	cat("\n")
	cat("     alpha =", alpha, "\n")
	cat("        p1 =", p1, "\n")
	cat("        p2 =", p2, "\n")
	cat("        n1 =", n1, "\n")
	cat("        n2 =", n2, "\n", "\n")
	if(p1 >p2) { # Swapping
		p3 <- p1; p1 <- p2; p2 <- p3
		n3 <- n1; n1 <- n2; n2 <- n3
	}
	ratio <- n2/n1
	r1   <- ratio +1
	pbar <- (p1 + ratio*p2)/r1
	n0   <- (n1- r1/(2*ratio*(p2-p1)))^2/n1
	zb   <- ((p2-p1)*sqrt(ratio*n0) -
		qnorm(1-alpha/2)*sqrt(r1*pbar*(1-pbar)))/
		sqrt(ratio*p1*(1-p1)+p2*(1-p2))
	power<- pnorm(zb)
	cat("\n","     power =", round(power,4), "\n", "\n")
}

### Quantile normal plot with Shapiro-Wilk test result
shapiro.qqnorm <- function(x, ...){
shapiro <-shapiro.test(x)
if(shapiro$p.value<.001) {shapvalue<-"Shapiro-Wilk test P value <.001"} else 
	{shapvalue <-paste( "Shapiro-Wilk test P value = ", round(shapiro$p.value, 4), sep="")}
qqnorm(x) -> q
qqnorm(x, main= paste("Normal Q-Q plot of ",as.character(substitute(x)), sep=""), ...)
text(min(q$x, na.rm=TRUE), max(q$y, na.rm=TRUE), pos=4, shapvalue, col="brown", font=3)
qqline(x, col="blue", lty=2)
}

### Match tabulation
match.tab <- function(case, exposed, strata) {
cat("\n")
exposed1 <- exposed
if(is.factor(exposed1)){
	exposed1 <- exposed1==levels(exposed1)[2]
	cat(paste("Exposure status:", as.character(substitute(exposed)), "=", levels(exposed)[2],"\n"))
}
control <- 1-case
aggregate(control, list(strata=strata), sum) -> a
colnames(a)[2] <- "ncontrols"
case.exposed <- case*exposed1
aggregate(case.exposed, list(strata=strata), sum) -> b
colnames(b)[2] <- "ncase.exposed"
control.exposed <- control*exposed1
aggregate(control.exposed, list(strata=strata), sum) -> c
colnames(c)[2] <- "ncontrol.exposed"
aggregate(case, list(strata=strata), length) -> d
colnames(d)[2] <- "all.subjects"
aggregate(exposed1, list(strata=strata), sum) -> e
colnames(e)[2] <- "all.exposed"
merge(a,b,by.x="strata", by.y="strata") -> f
merge(f,c,by.x="strata", by.y="strata") -> g
merge(g,d,by.x="strata", by.y="strata") -> h
merge(h,e,by.x="strata", by.y="strata") -> i
sum.i <- rowSums(i[,2:6])
rowi0 <- nrow(i)
i <- subset(i, !is.na(sum.i))
rowi1 <- nrow(i)
if(rowi1 < rowi0){
cat (rowi0-rowi1,"match sets have incomplete information thus omitted in the tabulation","\n")
}
cat ("Total number of match sets in the tabulation =", rowi1,"\n")
attach(i, warn.conflicts=FALSE)
all.unexposed <- all.subjects-all.exposed
ncontrol.exposed1 <- factor(ncontrol.exposed, levels=as.character(0:max(ncontrol.exposed)))
table(ncase.exposed, ncontrol.exposed1, ncontrols, dnn=c("No. of cases exposed","No. of controls exposed","No. of controls per case"))->match.table
cat("\n")
for(i in 1:max(ncontrols)){
	cat(paste("Number of controls =",i,"\n"))
	print(match.table[,1:(i+1),i])
	cat("\n")
}
### computing M-H OR
numerator <- (ncontrols-ncontrol.exposed)*ncase.exposed/(ncontrols+1)
denominator <- ncontrol.exposed*(1-ncase.exposed)/(ncontrols+1)
mhor <- sum(numerator)/sum(denominator)
cat(paste("Odds ratio by Mantel-Haenszel method =", round(mhor,3), "\n", "\n"))
### computing MLE-OR using clogit
library(survival)
model <- clogit(case ~ exposed + strata(strata))
clogitor <- exp(model$coefficients)
lnci95 <- c(model$coefficients-qnorm(0.975)*sqrt(model$var),model$coefficients+qnorm(0.975)*sqrt(model$var))
ci95.mleor <- exp(lnci95)
cat(paste("Odds ratio by maximum likelihood estimate (MLE) method =", round(clogitor,3),"\n","95%CI=",round(ci95.mleor[1],3),",",round(ci95.mleor[2],3), "\n"))
cat("\n")
detach(i, pos=2)
} 
### Goodness-of-fit test for poisson assumption after regression
poisgof <- function(model) {
if (model$family$family != "poisson" & substr(model$family$family,1,12)!="Negative Bin") 
	stop("Not from Poisson regression!")
chisq <- model$deviance
df <- model$df.residual
p.value <- pchisq(chisq, df, lower.tail=FALSE)
return(list(results="Goodness-of-fit test for Poisson assumption",chisq=chisq, df=df, p.value=p.value))
}
### Sort data set and related vector
sort.by <- function(...) {
.data <<- .data[order(...),]
if (length(ls.nofunction())>0){
y <- ls.nofunction()
for(i in 1:length(ls.nofunction())){
	if(length(get(ls.nofunction()[i]))==nrow(.data)){
	nam <- ls.nofunction()[i]
	assign (nam, (get(ls.nofunction()[i]))[order(...)], env = .GlobalEnv)
	}
}
}
detach(.data)
attach(.data, warn.conflicts=FALSE)
}
### One-way tabulation
tab1 <- function (x0, decimal=1, sort.group=c(FALSE,"decreasing","increasing"), graph=TRUE, missing=TRUE, bar.values=TRUE) {
if(graph){
		var1 <- as.character(substitute(x0))
		if(length(var1)>1){
			string2 <- var1[length(var1)]	
		}else
		if(substring(search()[2],first=1,last=8)!="package:"){
			string2 <-  attr(get(search()[2]), "var.labels")[attr(get(search()[2]), "names")==substitute(x0)]
			if(length(string2)==0){
				string2 <- as.character(substitute(x0))
			}
			if(string2==""){
				string2 <- as.character(substitute(x0))
			}
		}else{
			string2 <- as.character(substitute(x0))
		}
	string3 <- paste(title.string()$distribution.of,string2)
	
	table.to.plot <- table(x0)
	if(missing==TRUE){table.to.plot <- table(x0,exclude=NULL)
	if(is.factor(x0)) {table.to.plot <- as.table(summary(x0))}
	if(is.na(names(table.to.plot)[length(names(table.to.plot))]) |
		names(table.to.plot)[length(names(table.to.plot))]=="NA's") 
	names(table.to.plot)[length(names(table.to.plot))] <-"Missing"}
	suppressWarnings(if(sort.group=="decreasing"){
		table.to.plot <- table.to.plot[order(table.to.plot,names(table.to.plot), decreasing=TRUE)]
		if(max(nchar(names(table.to.plot)))>8 & length(table.to.plot)>6){
			table.to.plot <- table.to.plot[order(table.to.plot,names(table.to.plot),decreasing=FALSE)]
		}
	})
	suppressWarnings(if(sort.group=="increasing"){
		table.to.plot <- table.to.plot[order(table.to.plot,names(table.to.plot), decreasing=FALSE)]
		if(max(nchar(names(table.to.plot)))>8 & length(table.to.plot)>6){
			table.to.plot <- table.to.plot[order(table.to.plot,names(table.to.plot), decreasing=TRUE)]
		}
	})
	if(max(nchar(names(table.to.plot)))>8 & length(table.to.plot)>6){
		par(mai=c(0.95625, 0.1, 0.76875, 0.39375)+.1+c(0,par()$cin[1]*max(nchar(names(table.to.plot))*.75),0,0))
		barplot(table.to.plot,main=string3, horiz=TRUE, las=1, xlim=c(0, max(table.to.plot)*1.2)) -> y.coordinates
		if(bar.values==TRUE){text(table.to.plot, y.coordinates, as.character(table.to.plot), pos=4, offset=0.3)}
		par(mai=c(0.95625, 0.76875, 0.76875, 0.39375))
		}else{
			barplot(table.to.plot, main=string3, ylab=as.character(title.string()$frequency), 
				ylim=c(0, max(table.to.plot)*1.1)) -> x.coordinates
			if(bar.values==TRUE){text(x.coordinates, table.to.plot, as.character(table.to.plot), pos=3)}
		}
}
if(any(is.na(x0))){
	if(is.factor(x0)){
		output0 <- t(t(as.table(summary(x0))))
		output1 <- (t(t(table(x0))))
	}
	else{
		output0 <- t(t(table(x0, exclude=NULL)))
		output1 <- (t(t(table(x0))))
	}	
		percent0 <- round(output0[,1]/sum(output0)*100,decimal)
		percent1 <- round(output1[,1]/sum(output1[,1],na.rm=TRUE)*100,decimal)
		output <- cbind(output0, percent0, c(percent1,as.integer(0)))
suppressWarnings(if(sort.group=="decreasing"){
	output <- output[order(output[,1],decreasing=TRUE),]
})
suppressWarnings(if(sort.group=="increasing"){
	output <- output[order(output[,1],decreasing=FALSE),]
})
		output <- rbind(output,c(sum(as.integer(output[,1])),100,100))
		colnames(output) <- c("Frequency","  %(NA+)","  %(NA-)")
		rownames(output)[nrow(output)] <- "  Total"
}
else{
	output <- (t(t(table(x0))))
suppressWarnings(if(sort.group=="decreasing"){
	output <- output[order(table(x0),names(table(x0)), decreasing=TRUE),]
})
suppressWarnings(if(sort.group=="increasing"){
	output <- output[order(table(x0),names(table(x0)), decreasing=FALSE),]
})
	percent <- round(output/sum(output)*100,decimal)
	output <- cbind(output,percent)
	output <- rbind(output,c(sum(output[,1]),100))
	colnames(output) <- c("Frequency","Percent")
	rownames(output)[length(rownames(output))] <- "  Total"
}
cat("\n")
if(substring(search()[2], first=1, last=8)!="package:"){
options(warn=-1)
	cat(c(as.character(substitute(x0)),":"),(attr(get(search()[2]), "var.labels")[attr(get(search()[2]), "names")==substitute(x0)]), "\n","\n")
options(warn=TRUE)
	print(output, justify="right")
	cat("\n")
}
else{
	print(output, justify="right")
	cat("\n")
}
}
### recode values of a vector from a lookup array  

lookup <- function (x, lookup.array) 
{
    if (any(table(lookup.array[, 1]) > 1)) {
        stop("Index value in lookup array not unique!!")
    }
    else{
		b <- rep("", length(x))
		for (i in 1:nrow(lookup.array)) {
			if(is.na(lookup.array[i,1]) & !is.na(lookup.array[i,2])){
				b[is.na(x)] <- lookup.array[i,2]
			}else{
				b[x == lookup.array[i, 1]] <- as.character(lookup.array[i, 2])
			}
		}
		if(is.numeric(lookup.array)){
			x[b != "" & !is.na(b)] <- as.numeric(b[b != "" & !is.na(b)])
		}else{
			x[b != "" & !is.na(b)] <- (b[b != "" & !is.na(b)])
		}
		x[is.na(b)] <- as.numeric(b[is.na(b)])
		xreturn <- x
		return(xreturn)
	}
}
### Use various file formats
use <- function(filename, clear=TRUE) {
library(foreign)
if(clear){
	detach.all.data()
}
if(is.character(filename)){
if(suppressWarnings(any(tolower(list.files())==tolower(filename), na.rm=TRUE))){
  ext <- tolower(substring(filename,first=nchar(filename)-3, last=nchar(filename)) )
  if( ext == ".dta") {
  	.data <<- read.dta(filename)}
  if( ext == ".dbf") {
 	  .data <<- read.dbf(filename)
	  names(.data) <<- tolower(names(.data))}
  if( ext == ".rec") {
	  .data <<- read.epiinfo(filename)
	  names(.data) <<- tolower(names(.data))}
  if( ext == ".sav") {
	 .data <<- read.spss(filename)
	 var.labels <- attr(.data, "variable.labels")
	 names(.data) <<- tolower(names(.data))
	 .data <<- as.data.frame(.data)
	 attr(.data, "var.labels") <<- var.labels}
  if( substring(filename,first=nchar(filename)-3, last=nchar(filename))==".csv") {
	 .data <<- read.table(filename, header=TRUE, sep=",")}
  }else{stop("File not found")}
}else{
if(is.data.frame(filename)){
  .data <<- filename
  }else{stop("The argument is not a data frame")}
}
attach(.data, warn.conflicts=FALSE)
}
### Dot plot
dotplot <- function(x, bin=40, by=NULL, ...){
if (is.null(by)){
	value <- subset(x, !is.na(x))
}else{
	data1 <- data.frame(by)
	data1$x <- x
	data2 <- subset(data1,!is.na(x) & !is.na(by))
	value <- data2$x
	by0 <- data2$by
	rm(data1, data2)
}
if (any(class(x)=="difftime")){
	unit.value <- attr(x, "units")
	value <- as.numeric(value)
}
if(any(class(x)=="POSIXt")){
	value <- as.numeric(value) 
}
xgr <- cut(value, breaks=bin, labels=FALSE)
xgr <- as.numeric(xgr)
		var1 <- as.character(substitute(x))
		if(length(var1)>1){
			string2 <- var1[length(var1)]	
		}else
if(substring(search()[2],first=1,last=8)!="package:"){
	string2 <-  attr(get(search()[2]), "var.labels")[attr(get(search()[2]), "names")==substitute(x)]
	byname <-  attr(get(search()[2]), "var.labels")[attr(get(search()[2]), "names")==substitute(by)]
	if(length(string2)==0){
		string2 <- as.character(substitute(x))
	}
	if(length(byname)==0){
		byname <- as.character(substitute(by))
	}else{
	if(byname==""){byname <- as.character(substitute(by))}
	}
	if(string2==""){
		string2 <- as.character(substitute(x))
	}
}else{
	string2 <- as.character(substitute(x))
	byname <- as.character(substitute(by))
}
string3 <- paste(title.string()$distribution.of,string2)
value.pretty <- pretty(value)
if(any(class(x)=="Date")) {
	range.date <- difftime(summary(x)[6], summary(x)[1])
	numdate <- as.numeric(range.date)
    if(numdate <1){stop(paste("Only one day ie.",format(x,"%Y-%m-%d"),"not suitable for plotting"))}
	if(numdate <10){date.pretty <- seq(from=summary(x)[1],to=summary(x)[6],by="day"); format.time <- "%a%d%b"}
	if(numdate >=10 & numdate <30){date.pretty <- seq(from=summary(x)[1],to=summary(x)[6],by="2 day"); format.time <- "%d%b"}
	if(numdate >=30 & numdate <60){date.pretty <- seq(from=summary(x)[1],to=summary(x)[6],by="week"); format.time <- "%a %d"}
	if(numdate >=60 & numdate <700){date.pretty <- seq(from=summary(x)[1],to=summary(x)[6],by="month"); format.time <- "%b%y"}
	if(numdate >=700){date.pretty <- seq(from=summary(x)[1],to=summary(x)[6],by="year"); format.time <- "%b%y"}
	value.pretty <- as.numeric(date.pretty)
}
if(any(class(x)=="POSIXt")){
	range.time <- difftime(summary(x)[6],summary(x)[1])
	numeric.time <- as.numeric(range.time)
	units <- attr(range.time, "units")
	if(units=="secs")  {step <- "sec"; format.time <- "%M:%S";  scale.unit <- "min:sec"}
	if(units=="mins")  {step <- "min"; format.time <- "%H:%M";  scale.unit <- "HH:MM"}
	if(units=="hours") {step <- ifelse(numeric.time<2,"20 mins","hour");format.time <- "%H:%M";  scale.unit <- "HH:MM"}
	if(units=="days")  {
		if(numeric.time <2){
			step <- "6 hour"; format.time <- "%a %H:%M";  scale.unit <- "HH:MM"
		}else{
			step <- "day"; format.time <- "%d%b%y"; scale.unit <- "Date"
		}
	}
	if(units=="weeks") {step <- "week";format.time <- "%b%y";   scale.unit <- " "}
	time.pretty <- seq(from=summary(x)[1],to=summary(x)[6],by=step)
	value.pretty <- as.numeric(time.pretty)
}

if(is.null(by)){
	
	xgr <- sort(xgr)
	freq <- rep(1, length(value))
	for(i in 1:max(xgr)){
		freq[xgr==i] <- 1:sum(xgr==i) 
	}
	glm(xgr~value[order(value)])->model1
	xgr.pretty <- model1$coefficient[1] + model1$coefficient[2]*value.pretty
	if(max(freq)<20){
		plot(xgr,freq, xaxt="n", xlab=" ",main=string3,	ylab=title.string()$frequency,ylim=c(0,20), ...)
	}else{
	plot(xgr,freq, xaxt="n", xlab=" ",main=string3,	ylab=title.string()$frequency, ...)
	}
	if(any(class(x)=="POSIXct")){
		axis(side=1, at=xgr.pretty, labels=as.character(time.pretty,format=format.time))	
		title(xlab=scale.unit)
	}
	if(any(class(x)=="difftime")){
		axis(side=1,at=xgr.pretty, labels=value.pretty)
		title(xlab=unit.value)
	}
	if(any(class(x)=="Date")){
		axis(side=1,at=xgr.pretty, labels=as.character(format(value.pretty+as.Date("1970-01-01"), format.time)))
	}
	if(class(x)=="numeric" || class(x)=="integer"){
		axis(side=1,at=xgr.pretty, labels=value.pretty)
	}
}else{ 
	order1 <- order(by0,value)
	xgr <- xgr[order1]
	value <-value[order1]
	by1 <- factor(by0)
	by1 <- by1[order1]
if(is.factor(by0)){
	character.length <- ifelse(max(nchar(levels(by0)))>8, max(nchar(levels(by0)))*(60-max(nchar(levels(by0))))/60, max(nchar(levels(by0)))*1.2)
	left.offset <- max(c(0.76875+.2, .1+par()$cin[1]*character.length))
	par(mai=c(0.95625, left.offset, 0.76875, 0.39375))
}
	y <- rep(0, length(value))
	add.i <- 0
	yline <- NULL
	for(i in 1:length(levels(by1))){
		yline <- c(yline, add.i)
		col.j <- NULL
		for(j in 1:max(xgr[by1==levels(by1)[i]])){
			y[xgr==j & by1==(levels(by1))[i]] <- (1:sum(xgr==j & by1==(levels(by1))[i])) + add.i 
		}
		add.i <- max(y, na.rm=TRUE) +2
	}
	glm(xgr~value)->model1
	xgr.pretty <- model1$coefficient[1] + model1$coefficient[2]*value.pretty 
	main.lab <- paste(string3,title.string()$by,byname)
	if(max(y)<20){
	plot(xgr,y, xaxt="n", yaxt="n",
		xlab=" ",main=main.lab, ylim=c(-1,20),
		ylab=" ", col=as.numeric(by1), pch=18, ...)
	}else{
	plot(xgr,y, xaxt="n", yaxt="n",
		xlab=" ",main=main.lab, ylim=c(-1,max(y)),
		ylab=" ", col=as.numeric(by1), pch=18, ...)
	}
	abline(h=yline, col="blue")
	axis(2,at=yline, labels=levels(by1), padj=0, las=1)
	if(any(class(x)=="POSIXct")){
		axis(side=1, at=xgr.pretty, labels=as.character(time.pretty,format=format.time))	
		title(xlab=scale.unit)
	}
	if(any(class(x)=="difftime")){
		axis(side=1,at=xgr.pretty, labels=value.pretty)
		title(xlab=unit.value)
	}
	if(any(class(x)=="Date")){
		axis(side=1,at=xgr.pretty, labels=as.character(format(value.pretty+as.Date("1970-01-01"), format.time)))
	}
	if(class(x)=="numeric" || class(x)=="integer"){
		axis(side=1,at=xgr.pretty, labels=value.pretty)
	}
	par(mai=c(0.95625, 0.76875, 0.76875, 0.39375))
	}
 
}
### Labeling variables
label.var <-function(var, label, pack=TRUE){
# Store list of variable labels, 
	#if exist, in a temporary vector
data1 <- .data
if(any(names(data1)==as.character(substitute(var)))){
	if(is.null(attributes(data1)$var.labels)){
		attributes(data1)$var.labels <- rep("", length(names(data1)))
	}
	attributes(data1)$var.labels[names(data1)==as.character(substitute(var))] <- label
}else{
	old.labels <-attributes(data1)$var.labels
	data1[,ncol(data1)+1]<- var
	names(data1)[length(names(data1))] <- as.character(substitute(var))
	if(is.null(old.labels)){
		attributes(data1)$var.labels <- c(rep("", length(names(data1))-1),label)
	}else{
	attributes(data1)$var.labels <- c(old.labels,label)
	}
}
	data1[,names(data1)==as.character(substitute(var))] <- var
	.data <<- data1
	detach(.data)
if(pack){
	suppressWarnings(rm(list=as.character(substitute(var)), pos=1))
}
	attach(.data, warn.conflicts=FALSE)
}
### Recoding a variable or set of variables for the same final value
recode <- function(vars, old.value, new.value){
var.names<-as.character(substitute(vars))
if(length(var.names)>1){var.names <- var.names[-1]}
var.order <- match(var.names, names(.data))
if(is.numeric(old.value) | is.integer(old.value) | any(class(vars)=="POSIXt")){
	.data[,var.order][.data[,var.order]==old.value] <<- new.value
}else
for(i in var.order){
if(is.factor(.data[,i])){
levels(.data[,i])[levels(.data[,i])==old.value] <<- new.value
}
}
if(length(old.value)==nrow(.data)){
	.data[,var.order] <<- replace(.data[,var.order], old.value, new.value)
}
detach(.data)
attach(.data, warn.conflicts=FALSE)
}


### Multinomial summary display
mlogit.display <- function(multinom.model, decimal=2, alpha=.05) {
s <- summary(multinom.model)
z <- s$coefficients/s$standard.errors
pnorm.z <- pnorm(z)
pgroup <- cut(pnorm.z, c(0,0.0005,0.005,0.025,0.975, 0.995, 0.9995,1))
stars <-c("***","**","*","","*","**","***")
x <-paste(round(s$coefficients,decimal),"/",round(s$standard.errors,decimal+1),stars[pgroup], sep="")
dim(x) <- dim(z)
colnames(x) <- colnames(s$coefficients)
rownames(x) <- rownames(s$coefficients)

x1 <- t( x)
x2 <- t(exp(s$coefficients))
x2 <- round(x2,decimal)
x2.1 <- t(exp(s$coefficients-qnorm(1-alpha/2)*s$standard.errors))
x2.1 <- round(x2.1,decimal)
x2.2 <- t(exp(s$coefficients+qnorm(1-alpha/2)*s$standard.errors))
x2.2 <- round(x2.2,decimal)
x2 <- paste(x2,"(", x2.1, ",", x2.2, ")",  sep="")
dim(x2) <- dim(x1)
x2[1,] <- "-"
x3 <- cbind(x1[,1],x2[,1])
for(i in 2: (length(s$lab)-1)){x3 <- cbind(x3, cbind(x1[,i],x2[,i]))}
x4 <- rbind(c("Coeff./SE",paste("RRR(",100-100*alpha,"%CI)   ",sep=""),"Coeff./SE",paste("RRR(",100-100*alpha,"%CI)   ",sep="")),x3)
colnames.x4 <- c(s$lab[2],"")
for(i in 3:(length(s$lab))){colnames.x4 <- c(colnames.x4,c(s$lab[i],""))}
colnames(x4) <- colnames.x4
rownames(x4) <- c("",s$coefnames)
cat("\n")
cat(paste("Outcome =",as.character(s$term)[2],"; ","Referent group = ",s$lab[1],sep=""), "\n")
print.noquote(x4)
cat("\n")
cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 ", "\n")
cat("\n")
cat(paste("Residual Deviance:", round(s$deviance,decimal), "\n"))
cat(paste("AIC =", round(s$AIC,digits=decimal), "\n"))
cat("\n")
}


### Zap
zap <- function() {
pos.to.detach <- (1:length(search()))[substring(search(),first=1,last=8)!="package:" &
	search()!=".GlobalEnv" & search()!="Autoloads" & search()!="CheckExEnv"]
for(i in 1:length(pos.to.detach)){
	if(length(pos.to.detach)>0){
		detach(pos=pos.to.detach[1])
		pos.to.detach <- (1:length(search()))[substring(search(),first=1,last=8)!="package:" &
		search()!=".GlobalEnv" & search()!="Autoloads" & search()!="CheckExEnv"]
	}
}
y <- ls(envir= .GlobalEnv)
if(length(y)>0){
	vector1 <- character(0)
	for (i in 1:length(y)){
		if(substring(deparse(get(y[i]))[1],first=1,last=8)!="function") {
			vector1 <- c(vector1,y[i])
		}
	}
	rm(list=vector1, pos=1)
	}
}

### Pyramid of age by sex
pyramid <- function(age, sex, binwidth=5, age.sex.table=NULL, percent=c(FALSE,"each","total"), ...){
if(is.null(age.sex.table)){
agegr <- cut(age, br = ( (min(age, na.rm=TRUE)%/%binwidth)
:(max(age,na.rm=TRUE)%/%binwidth+1)*binwidth))
	age.sex.table <- table(agegr, sex, deparse.level=1, dnn=list(substitute(age),substitute(sex)))
if(ncol(table(agegr,sex))!=2)stop("There must be two genders")
	age.sex.table.dimnames <- names(attr(age.sex.table,"dimnames"))
}else{
	age.sex.table.dimnames <- names(attr(age.sex.table,"dimnames"))
}



par(mfrow=c(1,2))
old.par.mai <- c(0.95625, 0.76875, 0.76875, 0.39375)
left.par.mai <- old.par.mai
right.par.mai <- c(old.par.mai[1],old.par.mai[4],old.par.mai[3],old.par.mai[2])
column.names <-colnames(age.sex.table)
suppressWarnings(if(percent=="each"){
  age.sex.table<-cbind(age.sex.table[,1]/colSums(age.sex.table)[1]*100,age.sex.table[,2]/colSums(age.sex.table)[2]*100)
  column.names -> colnames(age.sex.table)
})
suppressWarnings(if(percent=="total"){
  age.sex.table<-cbind(age.sex.table[,1]/sum(age.sex.table),age.sex.table[,2]/sum(age.sex.table))*100
  column.names -> colnames(age.sex.table)
})
par(mai=left.par.mai)
barplot(-age.sex.table[,1], horiz=TRUE,yaxt="n",xlab=colnames(age.sex.table)[1], xlim=c(-max(age.sex.table),0),xaxt="n",...)-> label.points
axis(side=1, at=pretty(c(-max(age.sex.table),0)),labels=-pretty(c(-max(age.sex.table),0)))
axis(side=4,at=label.points, labels=rownames(age.sex.table), tick=FALSE, col="blue", las=2)
par(mai=right.par.mai)
barplot(age.sex.table[,2], horiz=TRUE, yaxt="n",xlab=colnames(age.sex.table)[2], xlim=c(0,max(age.sex.table)), ...) 
par(mfrow=c(1,1))
par(mai=old.par.mai)
}


followup.plot <-function(id, time, outcome, by=NULL, n.of.lines=NULL, legend=TRUE, line.col="blue"){
plot(time, outcome, xlab=" ", ylab=" ", type="n")
id.factor <- factor(id)
if(is.null(n.of.lines)){
if(!is.null(by)){
id <- id[order(id, time, by)]
time <- time[order(id, time, by)]
outcome <-outcome[order(id, time,by )]
by <- by[order(id, time, by)]
by.factor <- factor(by)
  for(i in 1:length(levels(by.factor))){
    for(j in 1:length(levels(id.factor))){
       lines(time[id.factor==levels(id.factor)[j] & by.factor==levels(by.factor)[i]],
          outcome[id.factor==levels(id.factor)[j] & by.factor==levels(by.factor)[i]], col=i)
    }
  }
if(legend){legend("topright", legend=levels(factor(by)), col=1:length(levels(factor(by))), bg="white", lty=1)}
}else{
id <- id[order(id, time)]
time <- time[order(id, time)]
outcome <- outcome[order(id, time)]
if(length(levels(factor(id))) < 8){
    for(j in 1:length(levels(id.factor))){
       lines(time[id.factor==levels(id.factor)[j]],
          outcome[id.factor==levels(id.factor)[j]], col=j)
      }
      if(legend){
          legend("topright", legend=levels(factor(id[order(id)])), col=1:length(levels(factor(id))), bg="white", lty=1)
      }
    }else{
    for(j in 1:length(levels(id.factor))){
    if(line.col=="multicolor"){       
          lines(time[id.factor==levels(id.factor)[j]],
              outcome[id.factor==levels(id.factor)[j]], col=j)
        }else{
          lines(time[id.factor==levels(id.factor)[j]],
              outcome[id.factor==levels(id.factor)[j]], col=line.col)
        }

      }
    }
      
  }
}

else

{
order.id.selected <- sample(c(rep(TRUE, n.of.lines),
  rep(FALSE, length(levels(factor(id)))-n.of.lines)))
if(!is.null(by)){
id <- id[order(id, time, by)]
time <- time[order(id, time, by)]
by <- by[order(id, time, by)]
id.factor <- factor(id)
by.factor <- factor(by)
  for(i in 1:length(levels(by.factor))){
    for(j in 1:length(levels(id.factor))){
       lines(time[id.factor==levels(id.factor)[j] & by.factor==levels(by.factor)[i]]*order.id.selected[j],
          outcome[id.factor==levels(id.factor)[j] & by.factor==levels(by.factor)[i]]*order.id.selected[j], col=i)
    }
  }
if(legend){legend("topright", legend=levels(factor(by)), col=1:length(levels(factor(by))), lty=1, bg="white")}
}else{
id <- id[order(id, time)]
id.factor <- factor(id)
time <- time[order(id, time)]
    for(j in 1:length(levels(id.factor))){
      if(line.col=="multicolor"){
           lines(time[id.factor==levels(id.factor)[j]]*order.id.selected[j],
                outcome[id.factor==levels(id.factor)[j]]*order.id.selected[j], col=j)
           }else{
           lines(time[id.factor==levels(id.factor)[j]]*order.id.selected[j],
                outcome[id.factor==levels(id.factor)[j]]*order.id.selected[j], col=line.col)
           }
    }  
  }
}
}


### Remark to list the above functions if needed.
cat("\n")
cat("Add-in functions developed by Epidemiology Unit, PSU, Thailand", "\n")
cat("[Type `check.add.in()' to see what epi calculator commands are added in.]", "\n")
cat("\n")
