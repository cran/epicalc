# This file is written to make simple epidemiological calculator available on R.

# Prepared by 
#	Virasakdi Chongsuvivatwong, Epidemiology Unit,                                                
#	Prince of Songkla University
#	Hat Yai, Thailand 90110
#   License: GPL version 2 or newer


# Work started in 2002-October


# Setting locale and automatic graph titles
.locale <- FALSE # All automatic graphs will initially have English titles
.distribution.of <- "Distribution of"
.by <- "by"
.frequency <- "Frequency"
.frequency1 <- "Frequency"
.No.of.observations <- "No. of observations = "
.ylab.for.summ <- "Subject sorted by X-axis values"
.percent <- "Percent"
.cum.percent <- "Cum. percent"
.var.name <- "Var. name"
.obs <- "obs."
.mean <- "mean  "
.median <- "median "
.sd <- "s.d.  "
.min <- "min.  "
.max <- "max.  "

codebook <- function (dataFrame = .data) 
{
    cat("\n", attr(dataFrame, "datalabel"), "\n", "\n")
    x1 <- dataFrame[1, ]
    for (i in 1:ncol(dataFrame)) {
        cat(paste(names(dataFrame)[i], "\t", ":", "\t", attr(dataFrame, 
            "var.labels")[i]), "\n")
        if (all(is.na(dataFrame[, i]))) {
            cat(paste("All elements of ", names(dataFrame)[i], 
                " have a missing value", "\n"))
        }
        else {
            if (class(x1) == "data.frame") {
                x2 <- x1[, i]
                }
                else{
                    x2 <- x1
                }
            if (any(class(x2) == "character") | any(class(x2) == "AsIs")) {
                cat("A character vector", "\n")
            }
            else {
                if(is.logical(x2)) x2 <- as.factor(x2)
                if (any(class(x2) == "factor")) {
                  table1 <- (t(t(table(dataFrame[, i]))))
                  table1 <- cbind(table1, format(table1/sum(table1) * 
                    100, digits = 3))
                  colnames(table1) <- c(.frequency1, .percent)
                  if (is.null(attr(dataFrame, "val.labels")[i]) ) {
                    print.noquote(table1, right = TRUE)
                  }
                  else {
                  if(any(is.na(attr(dataFrame, "label.table")))){
                    print.noquote(table1, right = TRUE)
                  }else{
                    attr(dataFrame, "label.table")[which(is.na(attr(attr(dataFrame, "label.table"), "names")))] <- ""
                    index <- attr(attr(dataFrame, "label.table"), "names") ==
                      attr(dataFrame, "val.labels")[i]
                    index <- na.omit(index)                                              
                    if(suppressWarnings(!all(rownames(as.data.frame(attr(dataFrame, "label.table")[index]))==levels(x2)))){
                      print.noquote(table1, right = TRUE)
                    }else{
                      table2 <- data.frame(attr(dataFrame, "label.table")[index], table1)
                      colnames(table2) <- c("code", colnames(table1))
                      cat("Label table:", attr(dataFrame, "val.labels")[i], 
                      "\n")
                    print.noquote(table2, right = TRUE)
}                  }
                }
                }
                else {
                  summ(dataFrame[, i], graph = FALSE)
                }
            }
        }
        cat("\n", "==================", "\n")
    }
}


###################
setTitle <- function(locale){
  Sys.setlocale("LC_ALL",locale)
  print(Sys.getlocale())
  .locale <<- TRUE 
  # With `set title' command the language of title will change with locale
  # listed in the array of the title string.
}

titleString <- function(distribution.of=.distribution.of,by=.by,frequency=.frequency, locale=.locale, return.look.up.table=FALSE){
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
des <- function (x = .data, select, exclude) 
{
    if (!missing(select) | !missing(exclude)) {
        nl <- as.list(1:ncol(x))
        names(nl) <- names(x)
        if (!missing(select)) 
            vars.selected <- eval(substitute(select), nl, parent.frame())
        if (!missing(exclude)) 
            vars.excluded <- eval(substitute(exclude), nl, parent.frame())
        if ((length(grep(pattern = "[*]", as.character(substitute(select)))) == 
            1) | (length(grep(pattern = "[?]", as.character(substitute(select)))) == 
            1)) {
            vars.selected <- grep(pattern = glob2rx(as.character(substitute(select))), 
                names(x))
            if (length(vars.selected) == 0) {
                stop(paste(select, "not matchable with any variable name."))
            }
        }
        if ((length(grep(pattern = "[*]", as.character(substitute(exclude)))) == 
            1) | (length(grep(pattern = "[?]", as.character(substitute(exclude)))) == 
            1)) {
            vars.excluded <- grep(pattern = glob2rx(as.character(substitute(exclude))), 
                names(x))
            if (length(vars.excluded) == 0) {
                stop(paste(exclude, "not matchable with any variable name."))
            }
        }
        vars <- 1:ncol(x)
        if (exists("vars.selected")) 
            vars <- vars[vars.selected]
        if (exists("vars.excluded")) 
            vars <- vars[-vars.excluded]
x1 <- x[1,]
        class.a <- rep("", length(vars))
        for (i in 1:length(vars)) {
            class.a[i] <- class(x1[,vars[i]])[1]
        }
        if (is.null(attr(x, "var.labels"))) {
            a <- cbind(colnames(x1)[vars], class.a, rep("", 
                length(vars)))
        }
        else {
            a <- cbind(colnames(x1)[vars], class.a, attr(x, 
                "var.labels")[vars])
        }
        colnames(a) <- c("Variable     ", "Class          ", 
            "Description")
        rownames(a) <- vars
        cat("\n")
        cat(attr(x, "datalabel"), "\n")
        cat(.No.of.observations)
        cat(nrow(x), "\n") 
        print.noquote(a)
        cat("\n")
        options(warn = 0)
    }
    else {
        if (!is.data.frame(x)) {
            if (is.character(x) & (length(grep(pattern = "[*]", 
                x)) == 1) | (length(grep(pattern = "[?]", x) == 
                1))) {
                vars <- grep(pattern = glob2rx(x), names(.data))
                if (length(vars) == 0) {
                  stop(paste(x, "not matchable with any variable name."))
                }
                
x1 <- .data[1,]
                class.a <- rep("", length(vars))
                for (i in 1:length(vars)) {
                class.a[i] <- class(x1[,vars[i]])[1]
                }
                if (is.null(attr(.data, "var.labels"))) {
                  a <- cbind(colnames(x1)[vars], class.a, 
                    rep("", length(vars)))
                }
                else {
                  a <- cbind(colnames(x1)[vars], class.a, 
                    attr(.data, "var.labels")[vars])
                }
                colnames(a) <- c("Variable     ", "Class          ", 
                  "Description")
                rownames(a) <- vars
                cat("\n")
                cat(attr(.data, "datalabel"), "\n")
                cat(.No.of.observations)
                cat(nrow(.data), "\n")
                print.noquote(a)
                cat("\n")
                options(warn = 0)
            }
            else {
                cat("\n")
                candidate.position <- NULL
                for (search.position in 1:length(search())) {
                  if (exists(as.character(substitute(x)), where = search.position)) {
                    if (any(names(get(search()[search.position])) == 
                      as.character(substitute(x))) | any(ls(all = TRUE, 
                      pos = 1) == as.character(substitute(x)))) 
                      candidate.position <- c(candidate.position, 
                        search.position)
                  }
                }
                var.order <- as.character(NULL)
                var.class <- NULL
                var.size <- NULL
                var.lab <- NULL
                for (i in candidate.position) {
                  if (i == 1) {
                    var.order <- c(var.order, "")
                  }
                  else {
                    var.order <- c(var.order, which(as.character(substitute(x)) == 
                      names(get(search()[i]))))
                  }
                  if (i == 1) {
                    var.class <- c(var.class, class(x))
                  }
                  else {
                    var.class <- c(var.class, class(get(search()[i])[, 
                      which(as.character(substitute(x)) == names(get(search()[i])))]))
                  }
                  if (i == 1) {
                    var.size <- c(var.size, length(x))
                  }
                  else {
                    var.size <- c(var.size, nrow(get(search()[i])))
                  }
                  if (i == 1 | is.null(attr(get(search()[i]), 
                    "var.labels")[attr(get(search()[i]), "names") == 
                    substitute(x)])) {
                    var.lab <- c(var.lab, " ")
                  }
                  else {
                    var.lab <- c(var.lab, attr(get(search()[i]), 
                      "var.labels")[attr(get(search()[i]), "names") == 
                      substitute(x)])
                  }
                }
                a <- cbind(search()[candidate.position], var.order, 
                  var.class, var.size, var.lab)
                dim(a)
                colnames(a) <- c("Var. source ", "Var. order", 
                  "Class  ", "# records", "Description")
                rownames(a) <- rep("", length(candidate.position))
                cat(paste("'", as.character(substitute(x)), "'", 
                  " is a variable found in the following source(s):", 
                  "\n", "\n", sep = ""))
                print.noquote(a)
                cat("\n")
            }
        }
        else {
x1 <- x[1,]
            if (is.null(attr(x, "var.labels"))) {
                b <- " "
            }
            else {
                b <- attr(x, "var.labels")
                if (length(b) < length(colnames(x))) {
                  options(warn = -1)
                }
            }
            class.a <- rep("", ncol(x1))
            for (i in 1:ncol(x1)) {
                class.a[i] <- class(x1[, i])[1]
            }
            a <- cbind(colnames(x1), class.a, b)
            colnames(a) <- c("Variable     ", "Class          ", 
                "Description")
            rownames(a) <- 1:nrow(a)
            cat("\n")
            cat(attr(x, "datalabel"), "\n")
            cat(.No.of.observations)
            cat(nrow(x), "\n")
            print.noquote(a)
            cat("\n")
            options(warn = 0)
        }
    }
}

### Detaching all data frame from the search path
detachAllData <- function(){
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
tabpct <- function(row, col, decimal=1, percent=c("both","col","row"), graph=TRUE, las=0, ...) {
tab <- table(row, col, deparse.level=1, dnn=list(substitute(row),substitute(col)), ...)
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
names(attr(tab,"dimnames")) <-c(string2, string4)
cat( "\n")
suppressWarnings(if(percent=="both"){
cat("Original table", "\n")
tabtotal <- addmargins(tab)
colnames(tabtotal)[ncol(tabtotal)] <- "Total"
rownames(tabtotal)[nrow(tabtotal)] <- "Total"
print(tabtotal, print.gap=2)
cat( "\n")})

suppressWarnings(if(percent=="both" | percent=="row"){
cat("Row percent", "\n")
names(attr(rpercent,"dimnames")) <- c(string2, string4)
print.table(rpercent, right=TRUE, print.gap=2)
cat( "\n")})

suppressWarnings(if(percent=="both" | percent=="col"){
cat("Column percent", "\n")
names(attr(cpercent,"dimnames")) <- c(string2, string4)
print.table(cpercent, right=TRUE, print.gap=2)
cat( "\n")})

if(graph==TRUE){
	rownames(tab)[is.na(rownames(tab))] <- "missing"
	colnames(tab)[is.na(colnames(tab))] <- "missing"
	las.value <- las
	if(nchar(paste(titleString()$distribution.of,string4,titleString()$by,string2))>45){
  	plot(as.table(tab),xlab=string2, ylab=string4, main=paste(titleString()$distribution.of,string4,"\n",titleString()$by,string2),
	col=c("white",2:length(col)), las=las.value)
  }else{
	plot(as.table(tab),xlab=string2, ylab=string4, main=paste(titleString()$distribution.of,string4,titleString()$by,string2),
	col=c("white",2:length(col)), las=las.value)}}

cpercent <- tab
for(i in 1:ncol(tab)) {cpercent[,i] <- tab[,i]/colSums(tab)[i]*100}

rpercent <- tab
for(i in 1:nrow(tab)) {rpercent[i,] <- tab[i,]/rowSums(tab)[i]*100}


returns <- list(table.row.percent=rpercent, table.column.percent=cpercent)
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
		cat("Cell counts too small - graph not shown","\n","\n")
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
labelTable(outcome, exposure, cctable=cctable, cctable.dimnames=cctable.dimnames) -> a
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
labelTable <- function(outcome, exposure, cctable=NULL , cctable.dimnames=NULL){
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
names(attr(cctable,"dimnames")) <-c(string2, string4)
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
labelTable(outcome, exposure, cctable=cctable, cctable.dimnames=cctable.dimnames) -> a
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
risk.diff.lower <- round(risk.diff*(1-sign(risk.diff)*(qnorm(1-.05/2)/sqrt(chi2))),decimal)
risk.diff.upper <- round(risk.diff*(1+sign(risk.diff)*(qnorm(1-.05/2)/sqrt(chi2))),decimal)
risk.ratio <- round(risk[2]/risk[1], decimal)
risk.ratio.lower <- round(risk.ratio^(1-sign(risk.diff)*(qnorm(1-.05/2)/sqrt(suppressWarnings(chisq.test(table)$statistic)))),decimal)
risk.ratio.upper <- round(risk.ratio^(1+sign(risk.diff)*(qnorm(1-.05/2)/sqrt(suppressWarnings(chisq.test(table)$statistic)))),decimal)
if(risk.ratio < 1) {
	protective.efficacy <- round(-risk.diff/risk[1]*100, decimal-1)
	protective.efficacy.lower <- round(100*(1-(risk.ratio^(1-(qnorm(1-.05/2)/sqrt(suppressWarnings(chisq.test(table)$statistic)))))),decimal)
	protective.efficacy.upper <- round(100*(1-(risk.ratio^(1+(qnorm(1-.05/2)/sqrt(suppressWarnings(chisq.test(table)$statistic)))))),decimal)
	nnt <- round(-1/risk.diff,decimal)
  nnt.lower <- round(-1/(risk.diff*(1+(qnorm(1-.05/2)/sqrt(chi2)))),decimal)
  nnt.upper <- round(-1/(risk.diff*(1-(qnorm(1-.05/2)/sqrt(chi2)))),decimal) 	
	risk.names <- c("Risk difference (Re - Rne)","Risk ratio","Protective efficacy =(Rne-Re)/Rne*100  ", "  or percent of risk reduced", 
		"Number needed to treat (NNT)"  )
	risk.table <- cbind(risk.names, 
      c(round(risk.diff,decimal), risk.ratio,      protective.efficacy,       " ", nnt),
  		c(risk.diff.lower,          risk.ratio.lower,protective.efficacy.lower, " ", nnt.lower),
      c(risk.diff.upper,          risk.ratio.upper,protective.efficacy.upper, " ", nnt.upper))
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
	row.names(risk.table) <- rep("",nrow(risk.table))
	colnames(risk.table) <- c("","Estimate","Lower95ci","Upper95ci")
	print.noquote(risk.table)
	cat("\n")
suppressWarnings(rm(cctable,caseexp, controlex, casenonex, controlnonex, pos=1))
}

### IDR display for poisson  and negative binomial regression
idr.display <- function (idr.model, alpha = 0.05, crude = TRUE, crude.p.value = FALSE, 
    decimal = 2, simplified = FALSE) 
{
    model <- idr.model
    if(length(grep("[$]", attr(model$term, "term.labels"))) > 0 | length(grep(")", attr(model$term, "term.labels"))) > 0  | length(model$call) < 3){
      simplified <- TRUE; crude <- TRUE
    }else{
    factor.with.colon <- NULL
    for(i in 1:(length(attr(model$term, "term.labels"))-1)){
    factor.with.colon <- c(factor.with.colon, any(grep(pattern=":",model$xlevels[i])))
    }
    factor.with.colon <- any(factor.with.colon)
    if(length(grep(":", attr(model$terms, "term.labels")))> 1 | factor.with.colon){
      simplified <- TRUE; crude <- TRUE
    }}
if(simplified){
coeff <- summary(model)$coefficients[-1,]
table1 <- cbind(exp(coeff[, 1]), 
                exp(coeff[, 1] - qnorm(1 - alpha/2) * coeff[, 2]), 
                exp(coeff[, 1] + qnorm(1 - alpha/2) * coeff[, 2]),
                coeff[,4] )
    colnames(table1) <- c("Adj. IDR", paste("lower", 100 - 100 * alpha, 
        "ci", sep = ""), paste("upper", 100 - 100 * alpha, "ci", 
        sep = ""), "Pr(>|Z|)")
}else{
    if (length(class(model)) == 1) {
        stop("Model not from logistic regression")
    }
    if (!any(class(model) == "glm") | !any(class(model) == "lm") | 
        (model$family$family != "poisson" &  length(grep("Negative Binomial", model$family$family))== 0)) {
        stop("Model not from poisson regression")
    }
    var.names <- attr(model$terms, "term.labels")
    if (length(var.names) == 1) {
        crude <- FALSE
    }
    if (crude) {
        idrci0 <- NULL
        for (i in 1:length(var.names)) {
            formula0 <- as.formula(paste(names(model$model)[1], 
                "~", var.names[i]))
            if(names(model$coefficient)[1]!="(Intercept)"){
            formula0 <- as.formula(paste(names(model$model)[1], 
                "~", var.names[i], "- 1"))
            }
            model0 <- glm(formula0, weights = model$prior.weights,  offset=model$offset,
                family = poisson, data = model$model)
            if(any(class(model)=="negbin")){
            model0 <- glm.nb (as.formula(paste(names(model$model)[1], "~", var.names[i])))
            }
            coeff.matrix <- (summary(model0))$coefficients[-1, 
                , drop = FALSE]
            if(names(model$coefficient)[1]!="(Intercept)"){
            coeff.matrix <- (summary(model0))$coefficients[, 
                , drop = FALSE]
            }
            if (length(grep(":", var.names[i])) > 0) {
                var.name.interact <- unlist(strsplit(var.names[i], 
                  ":"))
                if (any(names(model$xlevels) == var.name.interact[1])) {
                  level1 <- length(unlist(model$xlevels[var.name.interact[1]])) - 
                    1
                }
                else {
                  level1 <- 1
                }
                if (any(names(model$xlevels) == var.name.interact[2])) {
                  level2 <- length(unlist(model$xlevels[var.name.interact[2]])) - 
                    1
                }
                else {
                  level2 <- 1
                }
                dim.coeff <- dim((summary(model0))$coefficients[-1, 
                  , drop = FALSE])
                coeff.matrix <- matrix(rep(NA, dim.coeff[1] * 
                  dim.coeff[2]), dim.coeff[1], dim.coeff[2])
                coeff.matrix <- coeff.matrix[1:(level1 * level2), 
                  , drop = FALSE]
            }
            idrci0 <- rbind(idrci0, coeff.matrix)
        }
        if(names(model$coefficient)[1]=="(Intercept)"){
        idrci0 <- rbind(matrix(rep(0, 4), 1, 4), idrci0)
        }
        colnames(idrci0) <- c("crudeIDR", paste("lower0", 100 - 
            100 * alpha, "ci", sep = ""), paste("upper0", 100 - 
            100 * alpha, "ci", sep = ""), "crude P value")
        idrci0[, 3] <- exp(idrci0[, 1] + qnorm(1 - alpha/2) * idrci0[, 
            2])
        idrci0[, 2] <- exp(idrci0[, 1] - qnorm(1 - alpha/2) * idrci0[, 
            2])
        idrci0[, 1] <- exp(idrci0[, 1])
    }
    s1 <- summary(model)
    idrci <- s1$coefficients
    colnames(idrci) <- c("idr", paste("lower", 100 - 100 * alpha, 
        "ci", sep = ""), paste("upper", 100 - 100 * alpha, "ci", 
        sep = ""), "P(Wald's test)")
    idrci[, 3] <- exp(idrci[, 1] + qnorm(1 - alpha/2) * idrci[, 
        2])
    idrci[, 2] <- exp(idrci[, 1] - qnorm(1 - alpha/2) * idrci[, 
        2])
    idrci[, 1] <- exp(idrci[, 1])
    cat("\n")
    decimal1 <- ifelse(abs(idrci[, 1] - 1) < 0.01, 4, decimal)
    a <- cbind(paste(round(idrci[, 1], decimal1), " (", round(idrci[, 
        2], decimal1), ",", round(idrci[, 3], decimal1), ") ", 
        sep = ""), ifelse(idrci[, 4] < 0.001, "< 0.001", round(idrci[, 
        4], decimal + 1)))
    colnames(a) <- c(paste("adj. IDR(", 100 - 100 * alpha, "%CI)", 
        sep = ""), "P(Wald's test)")
    if (length(var.names) == 1) {
        colnames(a) <- c(paste("IDR(", 100 - 100 * alpha, "%CI)", 
            sep = ""), "P(Wald's test)")
    }
    rownames.a <- rownames(a)
    if (crude) {
        decimal0 <- ifelse(abs(idrci0[, 1] - 1) < 0.01, 4, decimal)
        if (crude.p.value) {
            a0 <- cbind(paste(round(idrci0[, 1], decimal0), " (", 
                round(idrci0[, 2], decimal0), ",", round(idrci0[, 
                  3], decimal0), ") ", sep = ""), ifelse(idrci0[, 
                4, drop = FALSE] < 0.001, "< 0.001", round(idrci0[, 
                4, drop = FALSE], decimal + 1)))
            a <- cbind(a0, a)
            rownames(a) <- rownames.a
            colnames(a) <- c(paste("crude IDR(", 100 - 100 * alpha, 
                "%CI)", sep = ""), "crude P value", paste("adj. IDR(", 
                100 - 100 * alpha, "%CI)", sep = ""), "P(Wald's test)")
            a[grep(":", rownames(a)), 1:2] <- "-"
        }
        else {
            a <- cbind(paste(round(idrci0[, 1], decimal1), " (", 
                round(idrci0[, 2], decimal1), ",", round(idrci0[, 
                  3], decimal1), ") ", sep = ""), a)
            colnames(a) <- c(paste("crude IDR(", 100 - 100 * alpha, 
                "%CI)", sep = ""), paste("adj. IDR(", 100 - 100 * 
                alpha, "%CI)", sep = ""), "P(Wald's test)")
            a[grep(":", rownames(a)), 1] <- "-"
        }
    }
    modified.coeff.array <- a
    table1 <- tableGlm(model, modified.coeff.array, decimal)
}
if(simplified) {
first.line <- NULL
last.lines <- NULL
}else{
    outcome.name <- names(model$model)[1]
    if(any(class(model)=="negbin")){
    modelData <- get(as.character(model$call)[3])
    }else{
    modelData <- model$data
    }
    if (!is.null(attr(modelData, "var.labels"))) {
        outcome.name <- attr(modelData, "var.labels")[attr(modelData, 
            "names") == names(model$model)[1]]
    }
    else {
        outcome.name <- names(model$model)[1]
    }
    outcome.name <- ifelse(outcome.name == "", names(model$model)[1], 
        outcome.name)
    if(!is.null(model$offset)){
    outcome.lab <- paste(outcome.name,  "with offset =", deparse(as.list(model$call)$offset), "\n")
    }else{
        outcome.lab <- paste(outcome.name, "\n")
    }
    }
    first.line <- paste("\n", ifelse(any(class(model)=="negbin"),"Negative binomial", "Poisson"), " regression predicting ", outcome.lab, sep = "")
    last.lines <- paste("Log-likelihood = ", round(logLik(model), decimal + 2), "\n",
                   "No. of observations = ", length(model$y), "\n",
                   "AIC value = ", round(s1$aic, decimal + 2), "\n","\n", sep = "")
    results <- list(first.line=first.line, table=table1, last.lines=last.lines)
    class(results) <- c("display", "list")
    results
}


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

logistic.display <- function (logistic.model, alpha = 0.05, crude = TRUE, crude.p.value = FALSE, 
    decimal = 2, simplified = FALSE) 
{
    model <- logistic.model
    if(length(grep("[$]", attr(model$term, "term.labels"))) > 0 
      | length(grep(")", attr(model$term, "term.labels"))) > 0  
      | length(model$call) < 3){
      simplified <- TRUE; crude <- TRUE
    }else{
    factor.with.colon <- NULL
    for(i in 1:(length(attr(model$term, "term.labels")))){
    factor.with.colon <- c(factor.with.colon, any(grep(":",model$xlevels[i])))
    }
    factor.with.colon <- any(factor.with.colon)
    if((length(grep(":", attr(model$terms, "term.labels"))) > 1) | factor.with.colon){
      simplified <- TRUE; crude <- TRUE
    }}
if(simplified){
coeff <- summary(model)$coefficients[-1,]
table1 <- cbind(exp(coeff[, 1]), 
                exp(coeff[, 1] - qnorm(1 - alpha/2) * coeff[, 2]), 
                exp(coeff[, 1] + qnorm(1 - alpha/2) * coeff[, 2]),
                coeff[,4] )
    colnames(table1) <- c("OR", paste("lower", 100 - 100 * alpha, 
        "ci", sep = ""), paste("upper", 100 - 100 * alpha, "ci", 
        sep = ""), "Pr(>|Z|)")
}else{
    if (length(class(model)) == 1) {
        stop("Model not from logistic regression")
    }
    if (class(model)[1] != "glm" | class(model)[2] != "lm" | 
        model$family$family != "binomial") {
        stop("Model not from logistic regression")
    }

    var.names <- attr(model$terms, "term.labels")
    if (length(var.names) == 1) {
        crude <- FALSE
    }
    if (crude) {
        orci0 <- NULL
        for (i in 1:length(var.names)) {
            formula0 <- as.formula(paste(names(model$model)[1], 
                "~", var.names[i]))
            if(names(model$coefficient)[1]!="(Intercept)"){
            formula0 <- as.formula(paste(names(model$model)[1], 
                "~", var.names[i], "- 1"))
            }
            if(length(grep("cbind", names(model$model)[1])) > 0){
            model0 <- glm(formula0, 
                family = binomial, data = get(as.character(model$call)[4]))
            }else{
            model0 <- glm(formula0, weights = model$prior.weights, 
                family = binomial, data = model$model)
            }
            coeff.matrix <- (summary(model0))$coefficients[-1, 
                , drop = FALSE]
            if(names(model$coefficient)[1]!="(Intercept)"){
            coeff.matrix <- (summary(model0))$coefficients[, 
                , drop = FALSE]
            }
            if (length(grep(":", var.names[i])) > 0) {
                var.name.interact <- unlist(strsplit(var.names[i], 
                  ":"))
                if (any(names(model$xlevels) == var.name.interact[1])) {
                  level1 <- length(unlist(model$xlevels[var.name.interact[1]])) - 
                    1
                }
                else {
                  level1 <- 1
                }
                if (any(names(model$xlevels) == var.name.interact[2])) {
                  level2 <- length(unlist(model$xlevels[var.name.interact[2]])) - 
                    1
                }
                else {
                  level2 <- 1
                }
                dim.coeff <- dim((summary(model0))$coefficients[-1, 
                  , drop = FALSE])
                coeff.matrix <- matrix(rep(NA, dim.coeff[1] * 
                  dim.coeff[2]), dim.coeff[1], dim.coeff[2])
                coeff.matrix <- coeff.matrix[1:(level1 * level2), 
                  , drop = FALSE]
            }
            orci0 <- rbind(orci0, coeff.matrix)
        }
        if(names(model$coefficient)[1]=="(Intercept)"){
        orci0 <- rbind(matrix(rep(0, 4), 1, 4), orci0)
        }
        colnames(orci0) <- c("crudeOR", paste("lower0", 100 - 
            100 * alpha, "ci", sep = ""), paste("upper0", 100 - 
            100 * alpha, "ci", sep = ""), "crude P value")
        orci0[, 3] <- exp(orci0[, 1] + qnorm(1 - alpha/2) * orci0[, 
            2])
        orci0[, 2] <- exp(orci0[, 1] - qnorm(1 - alpha/2) * orci0[, 
            2])
        orci0[, 1] <- exp(orci0[, 1])
    }
    s1 <- summary(model)
    orci <- s1$coefficients
    colnames(orci) <- c("OR", paste("lower", 100 - 100 * alpha, 
        "ci", sep = ""), paste("upper", 100 - 100 * alpha, "ci", 
        sep = ""), "P(Wald's test)")
    orci[, 3] <- exp(orci[, 1] + qnorm(1 - alpha/2) * orci[, 
        2])
    orci[, 2] <- exp(orci[, 1] - qnorm(1 - alpha/2) * orci[, 
        2])
    orci[, 1] <- exp(orci[, 1])

    decimal1 <- ifelse(abs(orci[, 1] - 1) < 0.01, 4, decimal)
    a <- cbind(paste(round(orci[, 1], decimal1), " (", round(orci[, 
        2], decimal1), ",", round(orci[, 3], decimal1), ") ", 
        sep = ""), ifelse(orci[, 4] < 0.001, "< 0.001", round(orci[, 
        4], decimal + 1)))
    colnames(a) <- c(paste("adj. OR(", 100 - 100 * alpha, "%CI)", 
        sep = ""), "P(Wald's test)")
    if (length(var.names) == 1) {
        colnames(a) <- c(paste("OR(", 100 - 100 * alpha, "%CI)", 
            sep = ""), "P(Wald's test)")
    }
    rownames.a <- rownames(a)
    if (crude) {
        decimal0 <- ifelse(abs(orci0[, 1] - 1) < 0.01, 4, decimal)
        if (crude.p.value) {
            a0 <- cbind(paste(round(orci0[, 1], decimal0), " (", 
                round(orci0[, 2], decimal0), ",", round(orci0[, 
                  3], decimal0), ") ", sep = ""), ifelse(orci0[, 
                4, drop = FALSE] < 0.001, "< 0.001", round(orci0[, 
                4, drop = FALSE], decimal + 1)))
            a <- cbind(a0, a)
            rownames(a) <- rownames.a
            colnames(a) <- c(paste("crude OR(", 100 - 100 * alpha, 
                "%CI)", sep = ""), "crude P value", paste("adj. OR(", 
                100 - 100 * alpha, "%CI)", sep = ""), "P(Wald's test)")
            a[grep(":", rownames(a)), 1:2] <- "-"
        }
        else {
            a <- cbind(paste(round(orci0[, 1], decimal1), " (", 
                round(orci0[, 2], decimal1), ",", round(orci0[, 
                  3], decimal1), ") ", sep = ""), a)
            colnames(a) <- c(paste("crude OR(", 100 - 100 * alpha, 
                "%CI)", sep = ""), paste("adj. OR(", 100 - 100 * 
                alpha, "%CI)", sep = ""), "P(Wald's test)")
            a[grep(":", rownames(a)), 1] <- "-"
        }
    }
    modified.coeff.array <- a
    table1 <- tableGlm(model, modified.coeff.array, decimal)
}
if(simplified) {
first.line <- NULL
last.lines <- NULL
}else{
    outcome.name <- names(model$model)[1]
    if (!is.null(attr(model$data, "var.labels"))) {
        outcome.name <- attr(model$data, "var.labels")[attr(model$data, 
            "names") == names(model$model)[1]]
    }
    else {
        outcome.name <- names(model$model)[1]
    }
    outcome.name <- ifelse(outcome.name == "", names(model$model)[1], 
        outcome.name)
    if (crude) {
        if (attr(model0$term, "dataClasses")[1] == "logical") {
            outcome.lab <- paste(outcome.name, "\n")
        }
        else {
            if ((attr(model$term, "dataClasses")[1] == "numeric") | 
                (attr(model$term, "dataClasses")[1] == "integer")) {
                outcome.levels <- levels(factor(model$model[, 
                  1]))
                outcome.lab <- paste(outcome.name, outcome.levels[2], 
                  "vs", outcome.levels[1], "\n")
            }
        }
    }
    if (attr(model$term, "dataClasses")[1] == "factor") {
        outcome.lab <- paste(names(model$model)[1], ":", levels(model$model[, 
            1])[2], "vs", levels(model$model[, 1])[1], "\n")
    }
    else {
        outcome.lab <- paste(outcome.name, "\n")
    }
    first.line <-paste("\n", "Logistic regression predicting ", outcome.lab, 
        sep = "")
    last.lines <- paste("Log-likelihood = ", round(logLik(model), decimal + 2), "\n",
                   "No. of observations = ", length(model$y), "\n",
                   "AIC value = ", round(s1$aic, decimal + 2), "\n","\n", sep = "")
}
    results <- list(first.line=first.line, table=table1, last.lines=last.lines)
    class(results) <- c("display", "list")
    results
}

print.display <- function(x, ...)
{
    cat(x$first.line, "\n")
    print.noquote(x$table)
    cat(x$last.lines)
}



###### Linear regression display

regress.display <- function (regress.model, alpha = 0.05, crude=FALSE, crude.p.value=FALSE, decimal = 2, simplified=FALSE) 
{
    model <- regress.model
    if(length(grep("[$]", attr(model$term, "term.labels"))) > 0 
      | length(grep(")", attr(model$term, "term.labels"))) > 0  
      | length(model$call) < 3){
      simplified <- TRUE; crude <- TRUE
    }else{
    factor.with.colon <- NULL
    for(i in 1:(length(attr(model$term, "term.labels"))-1)){
    factor.with.colon <- c(factor.with.colon, any(grep(":",model$xlevels[i])))
    }
    factor.with.colon <- any(factor.with.colon)
    if((length(grep(":", attr(model$terms, "term.labels"))) > 1) | factor.with.colon){
      simplified <- TRUE; crude <- TRUE
    }}
if(simplified){
coeff <- summary(model)$coefficients
table1 <- cbind(coeff[,1], (coeff[,1] - qt((1 - alpha/2), summary(model)$df[2]) * coeff[,2]),
          (coeff[,1] + qt((1 - alpha/2), summary(model)$df[2]) * coeff[,2]), coeff[,4]) 
colnames(table1) <- c("Coeff", paste("lower0", 100 - 100 * alpha, 
        "ci", sep = ""), paste("upper0", 100 - 100 * alpha, "ci", 
        sep = ""), "Pr>|t|")
}else{
    if(length(class(model))==2){
    if (class(model)[1] != "glm" | class(model)[2] != "lm" | 
        model$family$family != "gaussian") {
        stop("Model not from linear regression")
    }}else{
    if(length(class(model))==1){
    if (class(model) != "lm" ) {
        stop("Model not from linear regression")
    }}}
    var.names <- attr(model$terms, "term.labels") # Independent vars
    if(length(var.names)==1){crude <- FALSE}
    if(crude){
    reg.ci0 <- NULL
    for(i in 1:length(var.names)){
        formula0 <- as.formula(paste(names(model$model)[1], "~", var.names[i]))
    if(any(class(model)=="glm")){
        model0 <- glm(formula0, weights=model$prior.weights,
         family=model$family, data=model$model)
    }else{
        model0 <- lm(formula0, weights=model$prior.weights,
         data=model$model)
    }
    coeff.matrix <- (summary(model0))$coefficients[-1,]     
    if(length(grep(":", var.names[i]))>0){
    var.name.interact <- unlist(strsplit(var.names[i], ":"))
      if(any(names(model$xlevels)==var.name.interact[1])){
      level1 <- length(unlist(model$xlev>els[var.name.interact[1]]))-1
      }else{
      level1 <- 1
      }
      if(any(names(model$xlevels)==var.name.interact[2])){
      level2 <- length(unlist(model$xlevels[var.name.interact[2]]))-1
      }else{
      level2 <- 1
      }
    dim.coeff <- dim((summary(model0))$coefficients[-1,, drop=FALSE])
    coeff.matrix <- matrix(rep(NA, dim.coeff[1]*dim.coeff[2]), dim.coeff[1], dim.coeff[2])
    coeff.matrix <- coeff.matrix[1:(level1*level2), ,drop=FALSE] 
    }
    reg.ci0 <- rbind(reg.ci0, coeff.matrix)   
    }
    reg.ci0 <- rbind(matrix(rep(0,4),1,4), reg.ci0)
    colnames(reg.ci0) <- c("crude.Coeff", paste("lower0", 100 - 100 * alpha, 
        "ci", sep = ""), paste("upper0", 100 - 100 * alpha, "ci", 
        sep = ""), "crude P value")
    reg.ci0[, 3] <- (reg.ci0[, 1] + qt((1 - alpha/2), summary(model0)$df[2]) * reg.ci0[, 
        2])
    reg.ci0[, 2] <- (reg.ci0[, 1] - qt((1 - alpha/2), summary(model0)$df[2]) * reg.ci0[, 
        2])                                               
    }
    s1 <- summary(model)
    reg.ci <- s1$coefficients
    colnames(reg.ci) <- c("Coeff", paste("lower", 100 - 100 * alpha, 
        "ci", sep = ""), paste("upper", 100 - 100 * alpha, "ci", 
        sep = ""), "P(t-test)")
    reg.ci[, 3] <- (reg.ci[, 1] + qt((1 - alpha/2), summary(model)$df[2]) * reg.ci[, 
        2])
    reg.ci[, 2] <- (reg.ci[, 1] - qt((1 - alpha/2), summary(model)$df[2]) * reg.ci[, 
        2])                                        
    decimal1 <- ifelse(abs(reg.ci[,1]-1) < .01,  4, decimal)
    a <- cbind(paste(round(reg.ci[,1], decimal1)," (",round(reg.ci[,2], decimal1),",",
          round(reg.ci[,3], decimal1),") ", sep=""), ifelse(reg.ci[,4] < .001, "< 0.001",round(reg.ci[,4],decimal+1)))
    colnames(a) <- c(paste("adj. coeff.(",100 - 100 * alpha, "%CI)",sep=""),"P(t-test)")
    if(length(var.names)==1){
    colnames(a) <- c(paste("Coeff.(",100 - 100 * alpha, "%CI)",sep=""),"P(t-test)")
    }
    rownames.a <- rownames(a) 
    if(crude){
    decimal0 <- ifelse(abs(reg.ci0[,1]-1) < .01,  4, decimal)
    if(crude.p.value){
    a0 <- cbind(paste(round(reg.ci0[,1], decimal0)," (",round(reg.ci0[,2], decimal0),",",
          round(reg.ci0[,3], decimal0),") ", sep=""), ifelse(reg.ci0[,4] < .001, "< 0.001",round(reg.ci0[,4],decimal+1)))
    a <- cbind(a0,a)
    rownames(a) <- rownames.a
    colnames(a) <- c(paste("crude coeff.(",100 - 100 * alpha, "%CI)",sep="") ,"crude P value", 
        paste("adj. coeff.(",100 - 100 * alpha, "%CI)",sep=""),"P(t-test)")
        a[grep(":",rownames(a)) ,1:2] <- "-"
    }else{
    a <- cbind(paste(round(reg.ci0[,1], decimal1)," (",round(reg.ci0[,2], decimal1),",",
          round(reg.ci0[,3], decimal1),") ", sep=""), a)
    colnames(a) <- c(paste("crude coeff.(",100 - 100 * alpha, "%CI)",sep="") , 
          paste("adj. coeff.(",100 - 100 * alpha, "%CI)",sep=""),"P(t-test)")
        a[grep(":",rownames(a)) ,1] <- "-"
    }
    }
    modified.coeff.array <- a
tableGlm(model, modified.coeff.array, decimal) -> table1    
}
if(simplified) {
first.line <- NULL
last.lines <- NULL
}else{
    outcome.name <- names(model$model)[1]
    if(!is.null(attr(model$data, "var.labels"))){
    outcome.name <- attr(model$data, "var.labels")[attr(model$data, "names")==names(model$model)[1]]
    }else{
    
    if(!is.null(attributes(get(as.character(model$call)[3]))$var.labels)){
    var.labels <- attributes(get(as.character(model$call)[3]))$var.labels
    outcome.name <- var.labels[names(get(as.character(model$call)[3]))==outcome.name]
    }else{
    outcome.name <- names(model$model)[1]
    }}
            outcome.name <- ifelse(outcome.name == 
                "", names(model$model)[1], outcome.name)
    first.line <- paste("Linear regression predicting ",outcome.name, sep="", "\n")
    if(any(class(model)=="glm")){
    last.lines <- paste("Log-likelihood = ", round(logLik(model), decimal + 2), "\n",
                   "No. of observations = ", length(model$y), "\n",
                   "AIC value = ", round(s1$aic, decimal + 2), "\n","\n", sep = "")
    }else{
    last.lines <- paste("No. of observations = ", nrow(model$model), "\n","\n", sep = "")
    }
    }
    results <- list(first.line=first.line, table=table1, last.lines=last.lines)
    class(results) <- c("display", "list")
    results
}


#### Conditional logistic regression display

clogistic.display <- function (clogit.model, alpha = 0.05, crude=TRUE, crude.p.value=FALSE, decimal = 2, simplified = FALSE) 
{
    model <- clogit.model
    if(!any(class(model)=="clogit")){stop("Model not from conditional logisitic regression")}
    if(length(grep("[$]", attr(model$term, "term.labels")[-length(attr(model$term, "term.labels"))])) > 0 
      | length(grep(")", attr(model$term, "term.labels")[-length(attr(model$term, "term.labels"))])) > 0  
      | length(model$userCall) < 3){
      simplified <- TRUE; crude <- TRUE
    }else{
    factor.with.colon <- NULL
    for(i in 1:(length(attr(model$term, "term.labels"))-1)){
    factor.with.colon <- c(factor.with.colon, any(grep(pattern=":",levels(get(as.character(model$call)[3])[,attr(model$term,"term.labels")[i]]))))
    }
    factor.with.colon <- any(factor.with.colon)
    if(length(grep(":", attr(model$terms, "term.labels")))> 1 | factor.with.colon){
      simplified <- TRUE; crude <- TRUE
    }}
if(simplified){
table1 <- summary(model)$conf.int[,-2]
colnames(table1)[1] <- "Adj. OR"
}else{
    var.names0 <- attr(model$terms, "term.labels") # Independent vars
    var.names <- var.names0[-grep(pattern="strata", var.names0)]                                                    
    if(length(var.names)==1){crude <- FALSE}
    if(crude){
    orci0 <- NULL
    for(i in 1:(length(var.names))){                                        
        formula0 <- as.formula(paste( rownames(attr(model$terms,"factor"))[1], "~", 
            paste(c(var.names[i], var.names0[grep(pattern="strata", var.names0)]), collapse="+")))
        model0 <- coxph(formula0, data=get(as.character(model$call)[3]) )
    coeff.matrix <- summary(model0)$coef[,c(1,3:5), drop=FALSE]
    if(length(grep(":", var.names[i]))>0){
    var.name.interact <- unlist(strsplit(var.names[i], ":"))
      if(is.factor(get(as.character(model$call)[3])[,var.name.interact[1]])){
      level1 <- length(levels(get(as.character(model$call)[3])[,var.name.interact[1]]))-1
      }else{
      level1 <- 1
      }
      if(is.factor(get(as.character(model$call)[3])[,var.name.interact[2]])){
      level2 <- length(levels(get(as.character(model$call)[3])[,var.name.interact[2]]))-1
      }else{
      level2 <- 1
      }
    dim.coeff <- dim((summary(model0))$coef[,c(1,3:5), drop=FALSE])
    coeff.matrix <- matrix(rep(NA, dim.coeff[1]*dim.coeff[2]), dim.coeff[1], dim.coeff[2])
    coeff.matrix <- coeff.matrix[1:(level1*level2), ,drop=FALSE] 
    }
    orci0 <- rbind(orci0, coeff.matrix)   
    }
    colnames(orci0) <- c("crudeOR", paste("lower0", 100 - 100 * alpha, 
        "ci", sep = ""), paste("upper0", 100 - 100 * alpha, "ci", 
        sep = ""), "crude P value")
    orci0[, 3] <- exp(orci0[, 1] + qnorm(1 - alpha/2) * orci0[, 
        2])
    orci0[, 2] <- exp(orci0[, 1] - qnorm(1 - alpha/2) * orci0[, 
        2])                                               
    orci0[, 1] <- exp(orci0[, 1])
    }
    s1 <- summary(model)
    orci <- s1$coef[, c(1,3:5), drop=FALSE]
    colnames(orci) <- c("OR", paste("lower", 100 - 100 * alpha, 
        "ci", sep = ""), paste("upper", 100 - 100 * alpha, "ci", 
        sep = ""), "P(Wald's test)")
    orci[, 3] <- exp(orci[, 1] + qnorm(1 - alpha/2) * orci[, 
        2])
    orci[, 2] <- exp(orci[, 1] - qnorm(1 - alpha/2) * orci[, 
        2])                                               
    orci[, 1] <- exp(orci[, 1])
    decimal1 <- ifelse(abs(orci[,1]-1) < .01,  4, decimal)
    a <- cbind(paste(round(orci[,1], decimal1)," (",round(orci[,2], decimal1),",",
          round(orci[,3], decimal1),") ", sep=""), ifelse(orci[,4] < .001, "< 0.001",round(orci[,4],decimal+1)))
    colnames(a) <- c(paste("adj. OR(",100 - 100 * alpha, "%CI)",sep=""),"P(Wald's test)")
    if(length(var.names)==2){
    colnames(a) <- c(paste("OR(",100 - 100 * alpha, "%CI)",sep=""),"P(Wald's test)")
    }
    if(length(var.names)==1){
    colnames(a) <- c(paste("OR(",100 - 100 * alpha, "%CI)",sep=""),"P(Wald's test)")
    }
    rownames.a <- rownames(a) 
    if(crude){
    decimal0 <- ifelse(abs(orci0[,1]-1) < .01,  4, decimal)
    if(crude.p.value){
    a0 <- cbind(paste(round(orci0[,1,drop=FALSE], decimal0)," (",round(orci0[,2,drop=FALSE], decimal0),",",
          round(orci0[,3, drop=FALSE], decimal0),") ", sep=""), 
          ifelse(orci0[,4, drop=FALSE] < .001, "< 0.001",round(orci0[,4, drop=FALSE],decimal+1)))
    a <- cbind(a0,a)
    rownames(a) <- rownames.a
    colnames(a) <- c(paste("crude OR(",100 - 100 * alpha, "%CI)",sep="") ,"crude P value", 
        paste("adj. OR(",100 - 100 * alpha, "%CI)",sep=""),"P(Wald's test)")
    a[grep(":",rownames(a)) , 1:2] <- "-"
    }else{
    a <- cbind(paste(round(orci0[,1, drop=FALSE], decimal1)," (",round(orci0[,2, drop=FALSE], decimal1),",",
          round(orci0[,3, drop=FALSE], decimal1),") ", sep=""), a)
    colnames(a) <- c(paste("crude OR(",100 - 100 * alpha, "%CI)",sep="") , 
          paste("adj. OR(",100 - 100 * alpha, "%CI)",sep=""),"P(Wald's test)")
    a[grep(":",rownames(a)) , 1] <- "-"
    }
    }
    modified.coeff.array <- a

tableGlm(model, modified.coeff.array, decimal) -> table1    
}
if(simplified) {
first.line <- NULL
last.lines <- NULL
}else{
            outcome.name <- substr(as.character(model$userCall)[2], 1,  regexpr(" ", as.character(model$userCall)[2])-1)
            outcome <- get(as.character(model$userCall)[3])[,outcome.name]
            outcome.class <- class(outcome)
            if(outcome.class=="logical"){
            outocme.lab <- paste(outcome.name, "yes vs no","\n")
            }else{
            if(outcome.class=="numeric" | outcome.class=="integer"){
            outcome.levels <- levels(factor(outcome))
            outcome.lab <- paste(outcome.name, ":", outcome.levels[2], "vs", outcome.levels[1],"\n")
            }else{
            if(outcome.class=="factor"){
            outcome.lab <- paste(outcome.name, ":", levels(outcome)[2], "vs", levels(outcome)[1],"\n")
            }else{
            outcome.lab <- outcome.name
            }}}
    first.line <- paste("Conditional logistic regression predicting ",outcome.lab, sep="", "\n")
    last.lines <- paste("No. of observations = ", model$n, "\n")
}
    results <- list(first.line=first.line, table=table1, last.lines=last.lines)
    class(results) <- c("display", "list")
    results
}


####### Cox's regression display

cox.display <- function (cox.model, alpha = 0.05, crude=TRUE, crude.p.value=FALSE, decimal = 2, simplified = FALSE) 
{
    model <- cox.model
    if(!any(class(model)=="coxph")){stop("Model not from conditional logisitic regression")}
    if(length(grep("[$]", attr(model$term, "term.labels"))) > 0 
      | length(grep(")", attr(model$term, "term.labels"))) > 0  
      | length(model$call) < 3){
      simplified <- TRUE; crude <- TRUE
    }else{
    factor.with.colon <- NULL
    for(i in 1:(length(attr(model$term, "term.labels"))-1)){
    factor.with.colon <- c(factor.with.colon, any(grep(pattern=":",levels(get(as.character(model$call)[3])[,attr(model$term,"term.labels")[i]]))))
    }
    factor.with.colon <- any(factor.with.colon)
    if(length(grep(":", attr(model$terms, "term.labels")))> 1 | factor.with.colon){
      simplified <- TRUE; crude <- TRUE
    }}
if(simplified){
table1 <- summary(model)$conf.int[,-2]
colnames(table1)[1] <- "Adj. OR"
}else{
    var.names <- attr(model$terms, "term.labels") # Independent vars
    if(length(grep("strata", var.names)) > 0){
    var.names <- var.names[-grep("strata",var.names)]
    }
    if(length(var.names)==1){crude <- FALSE}
    if(crude){
    orci0 <- NULL
    for(i in 1:(length(var.names))){
        formula0 <- as.formula(paste( rownames(attr(model$terms,"factor"))[1], "~", 
            var.names[i]))
        suppressWarnings(model0 <- coxph(formula0, data=get(as.character(model$call)[3]) ))
    coeff.matrix <- summary(model0)$coef[,c(1,3:5), drop=FALSE]
    if(length(grep(":", var.names[i]))>0){
    var.name.interact <- unlist(strsplit(var.names[i], ":"))
      if(is.factor(get(as.character(model$call)[3])[,var.name.interact[1]])){
      level1 <- length(levels(get(as.character(model$call)[3])[,var.name.interact[1]]))-1
      }else{
      level1 <- 1
      }
      if(is.factor(get(as.character(model$call)[3])[,var.name.interact[2]])){
      level2 <- length(levels(get(as.character(model$call)[3])[,var.name.interact[2]]))-1
      }else{
      level2 <- 1
      }
    dim.coeff <- dim((summary(model0))$coef[,c(1,3:5), drop=FALSE])
    coeff.matrix <- matrix(rep(NA, dim.coeff[1]*dim.coeff[2]), dim.coeff[1], dim.coeff[2])
    coeff.matrix <- coeff.matrix[1:(level1*level2), ,drop=FALSE] 
    }
    orci0 <- rbind(orci0, coeff.matrix)   
    }
    colnames(orci0) <- c("crudeOR", paste("lower0", 100 - 100 * alpha, 
        "ci", sep = ""), paste("upper0", 100 - 100 * alpha, "ci", 
        sep = ""), "crude P value")
    orci0[, 3] <- exp(orci0[, 1] + qnorm(1 - alpha/2) * orci0[, 
        2])
    orci0[, 2] <- exp(orci0[, 1] - qnorm(1 - alpha/2) * orci0[, 
        2])                                               
    orci0[, 1] <- exp(orci0[, 1])
    }
    s1 <- summary(model)
    orci <- s1$coef[, c(1,3:5), drop=FALSE]
    colnames(orci) <- c("OR", paste("lower", 100 - 100 * alpha, 
        "ci", sep = ""), paste("upper", 100 - 100 * alpha, "ci", 
        sep = ""), "P(Wald's test)")
    orci[, 3] <- exp(orci[, 1] + qnorm(1 - alpha/2) * orci[, 
        2])
    orci[, 2] <- exp(orci[, 1] - qnorm(1 - alpha/2) * orci[, 
        2])                                               
    orci[, 1] <- exp(orci[, 1])
    decimal1 <- ifelse(abs(orci[,1]-1) < .01,  4, decimal)
    a <- cbind(paste(round(orci[,1], decimal1)," (",round(orci[,2], decimal1),",",
          round(orci[,3], decimal1),") ", sep=""), ifelse(orci[,4] < .001, "< 0.001",round(orci[,4],decimal+1)))
    colnames(a) <- c(paste("adj. HR(",100 - 100 * alpha, "%CI)",sep=""),"P(Wald's test)")
    if(length(var.names)==2){
    colnames(a) <- c(paste("HR(",100 - 100 * alpha, "%CI)",sep=""),"P(Wald's test)")
    }
    if(length(var.names)==1){
    colnames(a) <- c(paste("HR(",100 - 100 * alpha, "%CI)",sep=""),"P(Wald's test)")
    }
    rownames.a <- rownames(a) 
    if(crude){
    decimal0 <- ifelse(abs(orci0[,1]-1) < .01,  4, decimal)
    if(crude.p.value){
    a0 <- cbind(paste(round(orci0[,1,drop=FALSE], decimal0)," (",round(orci0[,2,drop=FALSE], decimal0),",",
          round(orci0[,3, drop=FALSE], decimal0),") ", sep=""), 
          ifelse(orci0[,4, drop=FALSE] < .001, "< 0.001",round(orci0[,4, drop=FALSE],decimal+1)))
    a <- cbind(a0,a)
    rownames(a) <- rownames.a
    colnames(a) <- c(paste("crude HR(",100 - 100 * alpha, "%CI)",sep="") ,"crude P value", 
        paste("adj. HR(",100 - 100 * alpha, "%CI)",sep=""),"P(Wald's test)")
    a[grep(":",rownames(a)) ,1:2] <- "-"
    }else{
    a <- cbind(paste(round(orci0[,1, drop=FALSE], decimal1)," (",round(orci0[,2, drop=FALSE], decimal1),",",
          round(orci0[,3, drop=FALSE], decimal1),") ", sep=""), a)
    colnames(a) <- c(paste("crude HR(",100 - 100 * alpha, "%CI)",sep="") , 
          paste("adj. HR(",100 - 100 * alpha, "%CI)",sep=""),"P(Wald's test)")
    a[grep(":",rownames(a)) ,1] <- "-"
    }
    }
    modified.coeff.array <- a
tableGlm(model, modified.coeff.array, decimal) -> table1    
}
if(simplified) {
first.line <- NULL
last.lines <- NULL
}else{
    surv.string <- as.character(model$formula)[2]
    if(length(grep(",", surv.string)) > 0){
    time.var.name <- substr(unlist(strsplit(surv.string, ","))[1], 6, nchar(unlist(strsplit(surv.string, ","))[1]))
    status.var.name <-  substr(unlist(strsplit(surv.string, " "))[2], 1, nchar(unlist(strsplit(surv.string, " "))[2])-1)      
    intro <- paste("Cox's proportional hazard model on time ('", time.var.name, "') to event ('", status.var.name, "')",sep="")
    }else{
    intro <- paste("Cox's proportional hazard model on '", surv.string, "'", sep="")
    }
    var.names0 <- attr(model$terms, "term.labels")
    if(length(grep("strata", var.names0))>0) {intro <- paste(intro, " with '", var.names0[grep("strata", var.names0)], "'", sep="" )}
    first.line <- paste(intro, "\n")
    last.lines <- paste("No. of observations = ", model$n, "\n")
}
    results <- list(first.line=first.line, table=table1, last.lines=last.lines)
    class(results) <- c("display", "list")
    results
}




####### Table for GLM and lm

tableGlm <- function (model, modified.coeff.array, decimal)

{
########## Nice row definition starts from here
## What we need here is the glm model object and 'modified.coeff.array'
    var.names <- attr(model$terms, "term.labels") # Independent vars    
    if(any(class(model)=="coxph")){
    var.names0 <- var.names
    if(length(grep("strata", var.names)) > 0){
    var.names <- var.names[-grep(pattern="strata", var.names)]
    }
    if(any(class(model)=="clogit")){
    data <- na.omit((get(as.character(model$call)[3]))[,c(as.character(model$term[[2]][3]),var.names, as.character(model$term[[3]][[length(model$call)-1]][2]))])
    }else{
    data <- na.omit(get(as.character(model$call)[3])[,c(as.character(model$call[[2]][[2]][c(2,3)]) ,as.character(attr(model$terms, "variables")[-c(1:2)]))])
    }}
    table1 <- NULL
    if(any(class(model)=="glm") | any(class(model)=="lm"))
    {
    coeff.names <- names(model$coefficients)
    }else{
    if(any(class(model)=="coxph")){
    coeff.names <- names(model$coef)
    }
    }
            label.row0 <- rep("", ncol(modified.coeff.array))
            label.row0 <- t(label.row0)
            blank.row <- rep("", ncol(modified.coeff.array))
            blank.row <- t(blank.row)
            rownames(blank.row) <- ""
      if(any(class(model)=="glm")){
      if(any(model$family=="gaussian")){
      unlist(summary(aov(model))) -> array.summ.aov
      dim(array.summ.aov) <- c(length(array.summ.aov)/5,5)
      F.p.value <- array.summ.aov[-nrow(array.summ.aov),5]
      F.p.value <- ifelse(F.p.value < .001, "< 0.001",round(F.p.value,decimal+1))
      }
      }else{
      if(any(class(model)=="lm")){
      unlist(summary(aov(model))) -> array.summ.aov
      dim(array.summ.aov) <- c(length(array.summ.aov)/5,5)
      F.p.value <- array.summ.aov[-nrow(array.summ.aov),5]
      F.p.value <- ifelse(F.p.value < .001, "< 0.001",round(F.p.value,decimal+1))
      }}
    for(i in 1:length(var.names)){ # i is the variable order  in model
        # Define variable class and levels
            if(any(class(model)=="lm")){
            if(length(grep(pattern=":", var.names[i])) < 1){
            variable <- model$model[,i+1]
            var.name.class <- attr(model$terms, "dataClasses")[names(attr(model$terms, "dataClasses"))==var.names[i]]
            if(var.name.class=="factor"){
            var.name.levels <- unlist(unlist(model$xlevels))[substr(names(unlist(model$xlevels)),1,nchar(var.names[i]))==var.names[i]]
            }
            }
            }else{
            if(any(class(model)=="coxph")){
            if(length(grep(pattern=":", var.names[i])) < 1){
            variable <- data[,var.names[i]]
            var.name.class <- class(data[,var.names[i]])
            if(var.name.class=="factor"){
            var.name.levels <- levels(data[,var.names[i]]) 
            }}}}

        # Define variable labels
            if(any(class(model)=="glm")){
            var.labels <- attr(model$data, "var.labels")[attr(model$data, "names")==var.names[i]]
              if(any(class(model)=="negbin")){
              var.labels <- attributes(get(as.character(model$call)[3]))$var.labels[names(get(as.character(model$call)[3]))==var.names[i]]
              } 
            }else{
            if(any(class(model)=="coxph")){
            var.labels <- attributes(get(as.character(model$call)[3]))$var.labels[names(get(as.character(model$call)[3]))==var.names[i]]
            }
            if(any(class(model)=="lm")){
            var.labels <- attributes(get(as.character(model$call)[3]))$var.labels[names(get(as.character(model$call)[3]))==var.names[i]]
            }
            }
        # Define model1 for lr test            
    if(any(class(model)=="glm")){
    if(any(model$family=="binomial") |any(model$family=="poisson") | any(class(model)=="negbin")){
        if(length(var.names)==1){
        formula1 <- as.formula(paste(names(model$model)[1], "~", "1"))
        }else{   
        formula1 <- as.formula(paste(names(model$model)[1], "~", paste(var.names[-i], collapse="+")))
        if(names(model$coefficients)[1] != "(Intercept)"){
        formula1 <- as.formula(paste(names(model$model)[1], "~", paste(var.names[-i], "-1", collapse="+")))
        }}
        if(length(grep("cbind", names(model$model)[1])) > 0){
        model1 <- glm(formula1, family=model$family, weights=model$prior.weights, 
             data = get(as.character(model$call)[4]))                                  
        }else{
        model1 <- glm(formula1, family=model$family, weights=model$prior.weights, offset=model$offset, data=model$model)
        }
        if(any(class(model)=="negbin")){
        model1 <- glm.nb (as.formula(paste(names(model$model)[1], "~", paste(var.names[-i], collapse="+"))))
        }
        if((length(var.names)==1 & names(model$coefficients)[1] != "(Intercept)")){
        lr.p.value <- "-"
        }else{
        lr.p.value <- suppressWarnings(lrtest(model1, model, print=FALSE)$p.value)
        lr.p.value <- ifelse(lr.p.value < .001, "< 0.001",round(lr.p.value,decimal+1))
        }
      }
      }else{
      if(any(class(model)=="coxph")){
      if(length(var.names)==1){
      lr.p.value <- summary(model)$logtest[3]
      }else{
      b <- as.character(model$formula)  
      if(any(class(model)=="clogit")){
      formula.full.coxph <- as.formula(paste(as.character(model$term[[2]][3]), "~", paste(var.names0, collapse="+")))
      model.full.coxph <- clogit(formula.full.coxph, data=data)
      }else{
      formula.full.coxph <- as.formula(paste(b[2], "~", paste(var.names, collapse="+")))      
      model.full.coxph <- coxph(formula.full.coxph, data=data)
      }
      if(any(class(model)=="clogit")){
      formula.coxph.i <- as.formula(paste(as.character(model$term[[2]][3]), "~", 
        paste(c(var.names[-i], var.names0[grep("strata", var.names0)]), collapse="+")))
      model.coxph.i <- clogit(formula.coxph.i, data=data)
      }else{
      formula.coxph.i <- as.formula(paste(b[2], "~", paste(var.names[-i], collapse="+")))
      model.coxph.i <- coxph(formula.coxph.i, data=data)
      }
      lr.p.value <- suppressWarnings(lrtest(model.full.coxph, model.coxph.i, print=FALSE)$p.value)
      }
      lr.p.value <- ifelse(lr.p.value < .001, "< 0.001",round(lr.p.value,decimal+1))
      }
      }
      
        # Define table0      
      if(length(var.names)==1){
      if((any(class(model)=="glm") | any(class(model)=="lm")) & names(model$coefficients)[1]=="(Intercept)"){
      table0 <- modified.coeff.array[-1,]
      }else(
      table0 <- modified.coeff.array
      )
      }else{
      if(length(grep(":", var.names[i])) > 0){
      table0 <- modified.coeff.array[grep(":", rownames(modified.coeff.array)),]
      }else{
      if((any(class(model)=="lm") & (var.name.class=="factor"| var.name.class=="logical"))|any(class(model)=="coxph")){
      table0 <- modified.coeff.array[setdiff(which(substr(rownames(modified.coeff.array),1, nchar(var.names[i]))==var.names[i]), grep(":", rownames(modified.coeff.array))) ,]
      }else{
      table0 <- modified.coeff.array[rownames(modified.coeff.array)==var.names[i],]
      }
      }}
      if(is.null(nrow(table0))) table0 <- t(table0) 
      
        # Define column names and row names
      if(nrow(table0)==1){ 
      # table0 with only a single row
        if(any(class(model)=="glm" | any(class(model)=="coxph"))){
        if(any(model$family=="binomial") |any(model$family=="poisson") |any(class(model)=="coxph")){
            table0 <- cbind(table0, lr.p.value)
            colnames(table0) <- c(colnames(modified.coeff.array),"P(LR-test)")
        }
        if(any(model$family=="gaussian")){
            table0 <- cbind(table0, F.p.value[i])
            colnames(table0) <- c(colnames(modified.coeff.array),"P(F-test)")
        }
        }else{
            table0 <- cbind(table0, F.p.value[i])
            colnames(table0) <- c(colnames(modified.coeff.array),"P(F-test)")
        }
            if(!is.null(var.labels)){
            rownames(table0) <- var.labels
            }else{
            rownames(table0) <- var.names[i]
            }
            rownames(table0) <- ifelse(rownames(table0) == 
                "", var.names[i], rownames(table0))
            if(length(grep(":", var.names[i]))==0)
            {
            if(length(table(variable))==2){
            if(var.name.class=="factor"){
            chosen.level <- var.name.levels[2]
            ref.level <- var.name.levels[1]
            rownames(table0) <- paste(rownames(table0),": ",chosen.level," vs ", ref.level, sep="")
            }else{
            if((var.name.class=="numeric")| (var.name.class=="integer")){
            if(names(table(variable))[1]=="0" & names(table(variable))[2]=="1"){
            chosen.level <- "1"
            ref.level <- "0 "
            rownames(table0) <- paste(rownames(table0),": ",chosen.level," vs ", ref.level, sep="")
            }else{
            rownames(table0) <- paste(rownames(table0),"(cont. var.)")            
            }
            }else{
            if(var.name.class=="logical"){rownames(table0) <- rownames(table0)}
            else{
            rownames(table0) <- paste(rownames(table0),"(cont. var.)")            
            }}
            }
            }else{
            if((var.name.class=="numeric")| (var.name.class=="integer")){
            rownames(table0) <- paste(rownames(table0),"(cont. var.)")            
            }
            }
            }else{
            rownames(table0) <- rownames(modified.coeff.array)[grep(":", rownames(modified.coeff.array))]                                                                                                      
            }
        table1 <- rbind(table1, cbind(table0), cbind(blank.row,""))
      }else{ 
      # table0 with multiple rows
            if(any(class(model)=="glm") | any(class(model)=="coxph")){
            if(any(model$family=="binomial") |any(model$family=="poisson") | any(class(model)=="coxph") | any(class(model)=="negbin")){
            label.row <- cbind(label.row0, lr.p.value)
            colnames(label.row) <- c(colnames(modified.coeff.array),"P(LR-test)")
            }
            if(any(model$family=="gaussian")){
            label.row <- cbind(label.row0, F.p.value[i])
            colnames(label.row) <- c(colnames(modified.coeff.array),"P(F-test)")
            }
            }else{
            label.row <- cbind(label.row0, F.p.value[i])
            colnames(label.row) <- c(colnames(modified.coeff.array),"P(F-test)")
            }
            if(!is.null(var.labels)){
            rownames(label.row) <- var.labels
            }else{
            rownames(label.row) <- var.names[i]
            }
            rownames(label.row) <- ifelse(rownames(label.row) == 
                "" | is.null(rownames(label.row)), var.names[i], rownames(label.row))
            if(length(grep(":", var.names[i])) > 0){
            rownames(label.row) <- var.names[i]
            }
            
            if(length(grep(":", var.names[i])) > 0){
            first.var <- unlist(strsplit(var.names[i],":"))[1]
            second.var <- unlist(strsplit(var.names[i],":"))[2]
            splited.old.rownames <- unlist(strsplit(rownames(table0),":"))
            dim(splited.old.rownames) <- c(2, length(splited.old.rownames)/2)
            splited.old.rownames <- t(splited.old.rownames)
            new.rownames1 <- substr(splited.old.rownames[,1], nchar(first.var)+1, nchar(splited.old.rownames[,1]) )
            new.rownames2 <- substr(splited.old.rownames[,2], nchar(second.var)+1, nchar(splited.old.rownames[,2]))
            rownames(table0) <- paste(new.rownames1,":",new.rownames2,sep="") 
            if(any(class(model)=="lm")){
            all.level1 <- unlist(model$xlevels[names(model$xlevels)==first.var])
            all.level2 <- unlist(model$xlevels[names(model$xlevels)==second.var])
            }else{
            all.level1 <- levels(get(as.character(model$call)[3])[,first.var])
            all.level2 <- levels(get(as.character(model$call)[3])[,second.var])
            }
            non.interact <- rownames(modified.coeff.array)[-grep(":", rownames(modified.coeff.array))]
            non.interact1 <- non.interact[substr(non.interact, 1, nchar(first.var))== first.var]
            used.levels1 <- substr(non.interact1, nchar(first.var)+1, nchar(non.interact1))
            ref.level1 <- setdiff(all.level1, used.levels1)
            non.interact2 <- non.interact[substr(non.interact, 1, nchar(second.var))== second.var]
            used.levels2 <- substr(non.interact2, nchar(second.var)+1 , nchar(non.interact2))
            ref.level2 <- setdiff(all.level2, used.levels2)
            ref.level <- paste(ref.level1,":",ref.level2,sep="")
            }else{
            rownames(table0) <- substr(rownames(table0), nchar(var.names[i])+1, nchar(rownames(table0)))
            if(any(class(model)=="lm")){
            all.levels <- unlist(unlist(model$xlevels))[substr(names(unlist(model$xlevels)),1,nchar(var.names[i]))==var.names[i]]
            }else{
            if(any(class(model)=="coxph")){
            all.levels <-levels(get(as.character(model$call)[3])[,var.names[i]])
            }
            }
            ref.level <- setdiff(all.levels, rownames(table0))
            }
            rownames(label.row) <- paste(rownames(label.row), ": ref.=", ref.level, sep="")
            rownames(table0) <- paste("  ", rownames(table0))
        table1 <- rbind(table1, label.row, cbind(table0, ""), cbind(blank.row,""))
      }
    }
}



#### Likelihood ratio test
lrtest <- function (model1, model2, print = TRUE) 
{
    if (any(class(model1) != class(model2))) {
        stop("Two models have different classes")
    }
    if (any(class(model1) == "coxph") & any(class(model2) == 
        "coxph")) {
        if (model1$n != model2$n) {
            stop("Two models has different sample sizes")
        }
        cat("\n")
        df1 <- length(model1$coefficients)
        df2 <- length(model2$coefficients)
        lrt <- 2 * (model2$loglik[2] - model1$loglik[2])
        diff.df <- df2 - df1
        if (lrt < 0) {
            lrt <- -lrt
            diff.df <- -diff.df
        }
        if (lrt * diff.df < 0) {
            stop("Likelihood gets worse with more variables. Test not executed")
        }
        if (print) {
            cat("Likelihood ratio test for Cox regression & conditional logistic regression", 
                "\n")
            cat("Chi-squared", diff.df, "d.f. = ", lrt, ",", 
                "P value = ", round(pchisq(lrt, diff.df, lower.tail = FALSE), 
                  4), "\n")
            cat("\n")
        }
    }
    if (any(class(model1) == "multinom") & any(class(model2) == 
        "multinom")) {
        if (any(dim(model1$residuals) != dim(model2$residuals))) {
            stop("Two models have different outcomes or different sample sizes")
        }
        cat("\n")
        df1 <- model1$edf
        df2 <- model2$edf
        lrt <- model2$deviance - model1$deviance
        diff.df <- df1 - df2
        if (lrt < 0) {
            lrt <- -lrt
            diff.df <- -diff.df
        }
        if (lrt * diff.df < 0) {
            stop("Likelihood gets worse with more variables. Test not executed")
        }
        if (print) {
            cat("Likelihood ratio test for multinomial logistic regression", 
                "\n")
            cat("Chi-squared", diff.df, "d.f. = ", lrt, ",", 
                "P value = ", round(pchisq(lrt, diff.df, lower.tail = FALSE), 
                  4), "\n")
            cat("\n")
        }
    }
    if (any(class(model1) == "polr") & any(class(model2) == "polr")) {
        if (model1$n != model2$n) {
            stop("Two models have different outcomes or different sample sizes")
        }
        cat("\n")
        df1 <- model1$edf
        df2 <- model2$edf
        lrt <- model2$deviance - model1$deviance
        diff.df <- df1 - df2
        if (lrt < 0) {
            lrt <- -lrt
            diff.df <- -diff.df
        }
        if (lrt * diff.df < 0) {
            stop("Likelihood gets worse with more variables. Test not executed")
        }
        if (print) {
            cat("Likelihood ratio test for ordinal regression", 
                "\n")
            cat("Chi-squared", diff.df, "d.f. = ", lrt, ",", 
                "P value = ", round(pchisq(lrt, diff.df, lower.tail = FALSE), 
                  4), "\n")
            cat("\n")
        }
    }
    if (suppressWarnings((all(class(model1) == c("glm", "lm")) & all(class(model2) == 
        c("glm", "lm"))) | (any(class(model1)=="negbin") & any(class(model2)=="negbin")))) {
        if (sum(model1$df.null) != sum(model2$df.null)) 
            stop("Number of observation not equal!!")
        df1 <- attributes(logLik(model1))$df
        df2 <- attributes(logLik(model2))$df
        lrt <- 2 * (as.numeric(logLik(model2) - logLik(model1)))
        diff.df <- df2 - df1
        if (lrt < 0) {
            lrt <- -lrt
            diff.df <- -diff.df
        }
        if (lrt * diff.df < 0) {
            stop("Likelihood gets worse with more variables. Test not executed")
        }
        if (print) {
            cat("Likelihood ratio test for MLE method", "\n")
            cat("Chi-squared", diff.df, "d.f. = ", lrt, ",", 
                "P value = ", round(pchisq(lrt, diff.df, lower.tail = FALSE), 
                  4), "\n")
            cat("\n")
        }
    }
    output <- list(model1 = model1$call, model2 = model2$call, 
        Chisquared = lrt, df = diff.df, p.value = pchisq(lrt, 
            diff.df, lower.tail = FALSE))
}

### List objects excluding function
lsNoFunction <- function() {
 setdiff(ls(envir= .GlobalEnv), as.character(lsf.str()[])
 )
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
  if(all(is.na(x))){ stop("All elements of ", substitute(x), " have a missing value")}
	if(!is.atomic(x)) {graph=FALSE}
	if(graph==TRUE){
#if(length(grep("Thai",Sys.getlocale("LC_ALL")))==1){
#  	Sys.setlocale(category = "LC_ALL", locale = "C")
#    }
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
		string3 <- paste(titleString()$distribution.of,string2)
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
		string5 <- paste(string3,titleString()$by,string4)
		if(nchar(string5)>45){string5 <- paste(string3,"\n", titleString()$by,string4)}
#		if(length(grep("Thai",Sys.getlocale("LC_ALL")))==1){string5<-paste(string3,"¨Óá¹¡µÒÁ",string4)}
#		if(length(grep("People's Republic of China",Sys.getlocale("LC_ALL")))==1){string5<-paste(string3,"Óë",string4)}
#		if(length(grep("Malay",Sys.getlocale("LC_ALL")))==1){string5<-paste(string3,"mengikut",string4)}
#		if(length(grep("Korean",Sys.getlocale("LC_ALL")))==1){string5<-paste(string3,"Ëæ Åà¶£²÷",string4)}

		## Defining pretty x ticking for date and time
		if(any(class(x)=="date")){
      x <- as.Date(paste(date.mdy(x)$year,"-", date.mdy(x)$month,"-", date.mdy(x)$day, sep=""))
    }
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
			by3 <- as.numeric(by2) #by3 <- max(as.numeric(by2))- as.numeric(by2) +1
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
			if(any(class(x)=="Date")){
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
				if(any(class(x)=="difftime")){unit <-attr(x,"unit")} else {unit<-" "}
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
					xlab=" ",ylab=.ylab.for.summ,  xaxt="n")
				axis(1, at=1:length(levels(x1)),labels=levels(x1))
			} else
			if(any(class(x)=="POSIXt")){
				plot(x1,y, pch=18, col="blue", main=string3,
					xlab=" ",ylab=.ylab.for.summ, xaxt="n")
				axis(1, at=time.pretty, labels=as.character(time.pretty,format=format.time))
			}else
			if(any(class(x)=="Date")){
				if(numdate < 700){
					plot(x1,y, pch=18, col="blue", main=string3,
						xlab=" ",ylab=.ylab.for.summ, yaxt="n", xaxt="n")
					axis(1, at=date.pretty, labels=as.character(date.pretty,format=format.time))
				}else{
					plot(x1,y, pch=18, col="blue", main=string3,
						xlab=" ",ylab=.ylab.for.summ, yaxt="n")
				}
			}else{
				plot(x1,y, pch=18, col="blue", main=string3,
					xlab=" ", ylab=.ylab.for.summ, yaxt="n")
				if(any(class(x)=="difftime")){unit <-attr(x,"unit")} else {unit<-" "}
				title(xlab=unit)
			}
			
			if(length(x1)<30){abline(h=y,lty = 3)}
			if(box==TRUE){
				boxplot(unclass(x1), add=TRUE, horizontal=TRUE, 
					axes=FALSE, at=.8*length(sort(x1)),boxwex=.2*length(sort(x1)) )
			}
		}
#if(Sys.getlocale()=="C")	Sys.setlocale(category = "LC_ALL", locale = "")
  }
	if(is.data.frame(x)){
		cat ("\n")
		cat(attr(x, "datalabel"), "\n")
		cat(.No.of.observations); cat(nrow(x), "\n")
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
		if(any(class(x1)=="date")){
      x1 <- as.Date(paste(date.mdy(x1)$year,"-", date.mdy(x1)$month,"-", date.mdy(x1)$day, sep=""))
    }
					if(any(class(x1)=="Date")){
						a[1,] <- c(length(x1),format(c(summary(x1)[4],summary(x1)[3],NA,summary(x1)[1],summary(x1)[6]),"%Y-%m-%d"))
					}else
					if(any(class(x)=="logical")){
					a[1,] <- round(c(length(na.omit(x1)),mean(na.omit(x1)),
						quantile(na.omit(x1), .5), ifelse(is.na(mean(na.omit(x1))), NA,round(sd(na.omit(x1)),2))  , 
					min(na.omit(x1)), max(na.omit(x1)) ),3 )
					}	
					else{
						a[1,] <- round( c(length(na.omit(x1)), summary(x1)[4], summary(x1)[3],
							ifelse(is.na(mean(na.omit(x1))), NA,sd(na.omit(x1)))  , 
						summary(x1)[1],summary(x1)[6]),3 )
					}
					colnames(a) <- c(.obs, .mean, .median, .sd, .min, .max)
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
		if(any(class(x)=="date")){
      x <- as.Date(paste(date.mdy(x)$year,"-", date.mdy(x)$month,"-", date.mdy(x)$day, sep=""))
    }
				if(any(class(x)=="Date")){
					a[1,] <- c(length(na.omit(x)),format(c(summary(x)[4],summary(x)[3],NA,summary(x)[1],summary(x)[6]),"%Y-%m-%d"))
				}else

				if(any(class(x)=="difftime")){
					a[1,] <- c(length(na.omit(x)), summary(x)[4],summary(x)[3],
						ifelse(is.na(mean(na.omit(x1))), NA,round(sd(na.omit(x1)),2)),
						summary(x)[1],summary(x)[6])
				}else{
					a[1,] <- round(c(length(na.omit(x)),mean(na.omit(x)),
						quantile(na.omit(x), .5), ifelse(is.na(mean(na.omit(x))), NA,round(sd(na.omit(x)),2))  , 
					min(na.omit(x)), max(na.omit(x)) ),3 )
				}
				colnames(a) <- c(.obs, .mean, .median, .sd, .min, .max)
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
colnames(a) <- c(.var.name, .obs, .mean, .median, .sd,  .min, .max)
a[,1] <- attr(x, "names")
rownames(a) <- 1:nrow(a)
for(i in 1:(dim(x)[2])) {
	if ((typeof(x[i][1,])=="character")|| is.na(mean(na.omit(as.numeric(x[[i]]))))   )  {
  a[i,3:7] <- ""
  }
	else{
		if(any(class(x[[i]])=="date")){
      x[[i]] <- as.Date(paste(date.mdy(x[[i]])$year,"-", date.mdy(x[[i]])$month,"-", date.mdy(x[[i]])$day, sep=""))
    }                               
	if (any(class(x[[i]])=="Date")){
	a[i,c(3,4,6,7)] <- format(c(summary(x[[i]])[4],summary(x[[i]])[3],
			summary(x[[i]])[1],summary(x[[i]])[6]), "%Y-%m-%d")
	a[i,5] <- NA 
	a[i,2] <- length((x[[i]])[!is.na(x[[i]])])

}
	else
	if (any(class(x[[i]])=="POSIXt")){

	a[i,c(3,4,6,7)] <- format(c(summary(x[[i]])[4],summary(x[[i]])[3],
			summary(x[[i]])[1],summary(x[[i]])[6]), "%Y-%m-%d %H:%M")
	a[i,5] <- NA 
	a[i,2] <- length((x[[i]])[!is.na(x[[i]])])

}
	else

	if(suppressWarnings (is.integer(x[[i]])||is.numeric(x[[i]])|(is.logical(x[[i]]) & !is.na(mean(na.omit(as.numeric(x[[i]]))))))){
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
}}
}
cat("\n")
print.noquote (a[,], digits=3)
cat("\n")
}
}

#### ROC curve from Logistic Regression
lroc <- function (logistic.model, graph=TRUE, add=FALSE, title=FALSE, 
    line.col="red", auc.coords=NULL, ...) {
if(add){
  title <- FALSE
}
table(logistic.model$fitted.values,logistic.model$y) -> firsttable
colnames(firsttable) <- c("Non-diseased","Diseased")
rownames(firsttable) <- substr(rownames(firsttable), 1,6)
firsttable1 <- cbind(as.numeric(rownames(firsttable)),firsttable)
rownames(firsttable1) <- rep("",nrow(firsttable1))
colnames(firsttable1)[1]<- "predicted.prob"
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
## Model description
model.des <- paste("logit (", deparse(logistic.model$formula),")",sep="")
## Area under the curve
auc <- 0
for(i in 1:(nrow(secondtable)-1)) {
	auc <- auc+ (secondtable[i,1]-secondtable[(i+1),1])*
		.5*(secondtable[i,2]+secondtable[(i+1),2])
	}
if(graph){
if(!add){
plot(secondtable[,1],secondtable[,2], xlab="1-Specificity",
		ylab="Sensitivity", xlim=(c(0,1)), 
		ylim=(c(0,1)), asp=1, ...)
if(title){
  title(main =model.des)
}
lines(secondtable[,1],secondtable[,2], col=line.col)
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
  if(!is.null(auc.coords)){text(x=auc.coords[1],y=auc.coords[2], pos=4, labels=auclabel, ...)}
}else{
points(secondtable[,1],secondtable[,2], ...)
lines(secondtable[,1],secondtable[,2], col=line.col, ...)
}}
list(model.description=model.des, auc=auc, predicted.table=firsttable1, diagnostic.table=secondtable)
}

### ROC curve from a table
roc.from.table <- function(table, graph=TRUE, add=FALSE, title=FALSE, line.col="red", auc.coords=NULL, ...) {
if (dim(table)[2] !=2) stop("There must be 2 columns")
if (table[1,1]/table[1,2] < table[nrow(table),1]/table[nrow(table),2]) {
	stop("At higher cut-off point, there should be more non-diseased")
	}
firsttable <- table
colnames(firsttable) <- c("Non-diseased","Diseased")
if(length(rownames(firsttable))==0) {
	rownames(firsttable) <- rep("", times=nrow(firsttable))
}
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
## Area under the curve
auc <- 0
for(i in 1:(nrow(secondtable)-1)) {
	auc <- auc+ (secondtable[i,1]-secondtable[(i+1),1])*
		.5*(secondtable[i,2]+secondtable[(i+1),2])
	}
if(graph){
if(!add){
	plot(secondtable[,1],secondtable[,2], xlab="1-Specificity",
		ylab="Sensitivity", xlim=(c(0,1)), 
		ylim=(c(0,1)), asp=1, ...)
	if(title){title(main = "ROC curve of the diagnostic table")}
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
}else{
	points(secondtable[,1],secondtable[,2], ...)
  lines(secondtable[,1],secondtable[,2], col=line.col, ...)
  }
  if(!is.null(auc.coords)){text(x=auc.coords[1],y=auc.coords[2], pos=4, labels=auclabel, ...)}
}
list(auc=auc, original.table=firsttable, diagnostic.table=secondtable)
}

### Kappa statistics
kap <- function(x, ...){
UseMethod("kap")
}
kap.default <- function(x, ...){
    if (is.table(x)){ 
    kap.table(x, ...)
    }
}
### Kappa statistics from a table cross-tab ratings of 2 raters
kap.table <- function (x, wttable = c(NULL, "w", "w2"), print.wttable = FALSE, ...) 
{
kaptable <- x
    if (ncol(kaptable) != nrow(kaptable)) 
        stop("Column & row not equal length")
    if(is.null(wttable) | (is.character(wttable)& length(wttable)==2)){
        wttable <- kaptable
        wttable[] <- 0
        for (i in 1:nrow(kaptable)) wttable[i, i] <- 1
    }else{
    if(!is.matrix(wttable)){
    if (wttable=="w"| wttable=="w2"){
    wttable1 <- kaptable
    wttable1[] <- 0
        for (i in 1:nrow(kaptable)) {
            for (j in 1:ncol(kaptable)){
            if(wttable=="w") {wttable1[i, j] <- 1 - abs(i-j)/(ncol(kaptable)-1)}
            if(wttable=="w2") {wttable1[i, j] <- 1 - (abs(i-j)/(ncol(kaptable)-1))^2}
            }
        }
        wttable <- wttable1
    }}}
    po <- 0
    pe <- 0
    exptable <- kaptable
    bigbracket <- 0
    wbari <- rep(0, ncol(kaptable))
    wbarj <- rep(0, nrow(kaptable))
    for (i in 1:nrow(kaptable)) {
        for (j in 1:ncol(kaptable)) {
            wbari[i] <- wbari[i] + wttable[i, j] * sum(kaptable[, 
                j])/sum(kaptable)
        }
    }
    for (j in 1:ncol(kaptable)) {
        for (i in 1:nrow(kaptable)) {
            wbarj[j] <- wbarj[j] + wttable[i, j] * sum(kaptable[i, 
                ])/sum(kaptable)
        }
    }
    for (i in 1:nrow(kaptable)) {
        for (j in 1:ncol(kaptable)) {
            po <- po + wttable[i, j] * kaptable[i, j]/sum(kaptable)
            exptable[i, j] <- sum(kaptable[i, ]) * sum(kaptable[, 
                j])/sum(kaptable)/sum(kaptable)
            pe <- pe + wttable[i, j] * exptable[i, j]
            bigbracket <- bigbracket + exptable[i, j] * (wttable[i, 
                j] - (wbari[i] + wbarj[j]))^2
        }
    }
    kap <- (po - pe)/(1 - pe)
    if(print.wttable)  print(wttable)
    if (length(colnames(kaptable)) == 0) {
        rownames(kaptable) <- paste("Group", as.character(1:nrow(kaptable)), 
            sep = "")
        colnames(kaptable) <- rownames(kaptable)
        attr(attr(kaptable, "dimnames"), "names") <- c("Rater A", 
            "Rater B")
        cat("\n")
        print(kaptable)
    }
    else {
        print(kaptable)
    }
    cat("\n")
    cat("Observed agreement =", round(po * 100, 2), "%", "\n")
    cat("Expected agreement =", round(pe * 100, 2), "%", "\n")
    cat("Kappa =", round(kap, 3), "\n")
    sekap <- 1/(1 - pe)/sqrt(sum(kaptable)) * sqrt(bigbracket - 
        pe^2)
    z <- kap/sekap
    p.value <- pnorm(z, lower.tail = FALSE)
    if (p.value < 0.001) {
        P.value <- "< 0.001"
    }
    else {
        P.value <- as.character(round(p.value, 3))
    }
    cat("Standard error =", round(sekap, digits = 3), ", Z =", 
        round(z, digits = 3), ", P value =", P.value, "\n", "\n")
    returns <- list(po = po, pe = pe, kappa = kap, std.error = sekap, 
        z = z, p.value = p.value)
}
## Kappa statistics with two raters
kap.2.raters <- function(x, rater2, ...){
rater1 <- x
kaptable <- table(rater1, rater2)
if(any(rownames(kaptable)!= colnames(kaptable))) {stop("Table to use for kappa calculation must be symmetrical")}
kap.table(kaptable, ...) 
}
## Kappa statistics with more than two raters
kap.m.raters <- function(x, raters, ...){
id <- x
category.levels <- NULL
for(i in 1:ncol(raters)){
  category.levels <- c(category.levels, names(table(raters[,i])))
}
category.levels <- unique(category.levels)
category.counts <- rep(0, times=length(id)*length(category.levels))
dim(category.counts) <- c(length(id), length(category.levels))
for(j in 1:length(category.levels)){
if(is.factor(raters[,1])){
    for(i in 1:length(id)){
    category.counts[i,j] <- sum(raters[i,][!is.na(raters[i,])]==category.levels[j])
    }
  }else{
    for(i in 1:length(id)){
    category.counts[i,j] <- sum(raters[i,][!is.na(raters[i,])]==as.numeric(category.levels[j]))
  }
}
colnames(category.counts) <- category.levels
}
kap.ByCategory(x, as.data.frame(category.counts))
}


## Kappa statistics with id of the ratee and counts of rated categories
kap.ByCategory <- function(x, category.counts, ...){
id <- x
n <- length(id)
mi <- rowSums(category.counts)
mbar <- sum(mi/n)
pbar <- NULL
qbar <- NULL
kapp <- NULL
z <- NULL
sekap <- NULL
p.value <- NULL
for(j in 1:ncol(category.counts)){
  xi <- category.counts[,j]
  last.pbar <- sum(xi/(n*mbar))
  pbar <- c(pbar, last.pbar)
  last.qbar <- 1-last.pbar
  qbar <- c(qbar, last.qbar)
  B <- 1/n*sum((xi-mi*last.pbar)^2/mi) # Between-subject mean square
  W <- 1/(n*(mbar-1))*sum(xi*(mi-xi)/mi) # Within-subject mean square
  mbarH <- 1/(mean(1/mi)) # harmonic mean of mi
  kapp <- c(kapp, (B-W)/(B+(mbar-1)*W))
  if(ncol(category.counts)==2| var(mi)==0){
    last.sekap <- 1/((mbar-1)*sqrt(n*mbarH))*
        sqrt(2*(mbarH-1)+(mbar-mbarH)*(1-4*last.pbar*last.qbar)/(mbar*last.pbar*last.qbar))
    sekap <- c(sekap, last.sekap)
    last.z <- (B-W)/(B+(mbar-1)*W)/last.sekap
    z <- c(z, last.z)
    last.p.value <- pnorm(last.z, lower.tail = FALSE)
    p.value <- c(p.value, last.p.value)
  }
}
if(ncol(category.counts)==2){
data.frame(kappa=kapp[1], std.error=last.sekap, z=last.z, p.value=last.p.value, row.names="")
}else{
  if( var(mi)==0){
each.category <- data.frame(kappa = kapp, std.error = sekap, 
        z = z, p.value = p.value, row.names=colnames(category.counts))
}else{
each.category <- data.frame(kappa = kapp, std.error = ".", z=".", p.value = ".", row.names=colnames(category.counts)) 
}
kapp.bar <- sum(pbar*qbar*kapp)/sum(pbar*qbar)
  if(ncol(category.counts)==2| var(mi)==0){
m <- mi[1]
sekap.bar <- sqrt(2)/(sum(pbar*qbar)*sqrt(n*m*(m-1)))*sqrt((sum(pbar*qbar))^2-sum(pbar*qbar*(qbar-pbar)))
z.bar <- kapp.bar/sekap.bar
p.value.bar <- pnorm(z.bar, lower.tail = FALSE)
row.names.overall <- ""
for(i in 1:max(nchar(colnames(category.counts)))){row.names.overall <- paste(row.names.overall, " ", sep="")}
Overall <-data.frame(kappa = kapp.bar, std.error = sekap.bar, z=z.bar, p.value = p.value.bar, row.names=row.names.overall) 
list(Each.category=each.category, Overall=Overall)
}else{
row.names.overall <- ""
for(i in 1:max(nchar(colnames(category.counts)))){row.names.overall <- paste(row.names.overall, " ", sep="")}
Overall <-data.frame(kappa = kapp.bar, std.error = ".", z=".", p.value = ".", row.names=row.names.overall) 
list(Each.category=each.category, Overall=Overall)
}
}
}
 
### Make 2 x 2 table
make2x2 <- function(caseexp, controlex, casenonex, controlnonex) {
table <- c(controlnonex, casenonex, controlex, caseexp)
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
pack <- function (dataFrame = .data) 
{
    data1 <- dataFrame
    j <- NULL
    k <- attr(data1, "var.labels")
    candidate.objects <- setdiff(lsNoFunction(), as.character(ls.str(mode = "list")[]))
    if (length(candidate.objects) == 0) 
        stop("No related vector outside the default data frame")
    for (i in 1:length(candidate.objects)) {
        if (length(get(candidate.objects[i])) == nrow(data1)) {
            if (any(names(data1) == candidate.objects[i])) {
                data1[, names(data1) == candidate.objects[i]] <- get(candidate.objects[i])
                j <- c(j, i)
            }
            else {
                data1 <- data.frame(data1, get(candidate.objects[i]))
                names(data1)[ncol(data1)] <- candidate.objects[i]
                j <- c(j, i)
                if (!is.null(k)) {
                  k <- c(k, "")
                }
            }
        }
    }
    attr(data1, "var.labels") <- k
    rm(list = candidate.objects[j], pos = 1)
    assign(as.character(substitute(dataFrame)), data1, pos=1)
    if(is.element(as.character(substitute(dataFrame)), search())){
      detach(pos=which(search() %in% as.character(substitute(dataFrame))))
      attach(data1, name=as.character(substitute(dataFrame)), warn.conflicts = FALSE)
    }
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
qqnorm(x, plot.it = FALSE) -> q
qqnorm(x, main= paste("Normal Q-Q plot of ",deparse(substitute(x)), sep=""), ...)
text(min(q$x, na.rm=TRUE), max(q$y, na.rm=TRUE), pos=4, shapvalue, col="brown", font=3)
qqline(x, col="blue", lty=2)
}

### Match tabulation
matchTab <- function(case, exposed, strata) {
cat("\n")
if((length(table(case))!=2)){
stop("Case variable not binary")
}
if(any(is.na(case))){
stop("There should not be any missing outcome")}
if(length(table(exposed))!=2){
stop("Exposure variable not binary")
}
exposed1 <- exposed
if(is.factor(exposed)){
	cat(paste("Exposure status:", as.character(substitute(exposed)), "=", levels(exposed)[2],"\n"))
}else{
	cat(paste("Exposure status:", as.character(substitute(exposed)), "=", max(exposed, na.rm=TRUE),"\n"))
}
cat("\n")
if(is.factor(exposed1)){
	exposed1 <- exposed1==levels(exposed1)[2]
}
control <- 1-case
aggregate.data.frame(control, list(strata=strata), sum) -> a
colnames(a)[2] <- "ncontrols"
case.exposed <- case*exposed1
aggregate.data.frame(case.exposed, list(strata=strata), sum) -> b
colnames(b)[2] <- "ncase.exposed"
control.exposed <- control*exposed1
aggregate.data.frame(control.exposed, list(strata=strata), sum) -> c
colnames(c)[2] <- "ncontrol.exposed"
aggregate.data.frame(case, list(strata=strata), length) -> d
colnames(d)[2] <- "all.subjects"
aggregate.data.frame(exposed1, list(strata=strata), sum) -> e
colnames(e)[2] <- "all.exposed"
merge(a,b,by.x="strata", by.y="strata") -> f
merge(f,c,by.x="strata", by.y="strata") -> g
merge(g,d,by.x="strata", by.y="strata") -> h
merge(h,e,by.x="strata", by.y="strata") -> ii
sum.ii <- rowSums(ii[,2:6])
rowi0 <- nrow(ii)
ii <- subset(ii, !is.na(sum.ii))
rowi1 <- nrow(ii)
if(rowi1 < rowi0){
cat (rowi0-rowi1,"match sets with incomplete information omitted from tabulation.","\n")
}
cat ("Total number of match sets in the tabulation =", rowi1,"\n")
all.unexposed <- ii$all.subjects-ii$all.exposed
ii$ncontrol.exposed1 <- factor(ii$ncontrol.exposed, levels=as.character(0:max(ii$ncontrols)))
ii$ncase.exposed1 <- factor(ii$ncase.exposed, levels=as.character(0:max(ii$ncase.exposed)))
table(ii$ncase.exposed1, ii$ncontrol.exposed1, ii$ncontrols, dnn=c("No. of cases exposed","No. of controls exposed","No. of controls per case"))->matchTable
cat("\n")
for(i in 1:max(ii$ncontrols)){
	cat(paste("Number of controls =",i,"\n"))
	print(matchTable[1:max(c(2,max(which(rowSums(matchTable[,,i])>0)))),1:(i+1),i])
	cat("\n")
}
numerator <- (ii$ncontrols-ii$ncontrol.exposed)*ii$ncase.exposed/(ii$ncontrols+1)
denominator <- ii$ncontrol.exposed*(1-ii$ncase.exposed)/(ii$ncontrols+1)
if(sum(denominator) <1){
cat("Inadequate discordant pairs. Odds ratio not computed"); cat("\n")
}else{
if(any(ii$ncase.exposed>1)){
cat(paste(c("More than one cases exposed in strata # ", as.character(ii$strata[ii$ncase.exposed > 1]), ". M-H odds ratio not computed."), sep=""))
cat("\n", "\n")
}else{
mhor <- sum(numerator)/sum(denominator)
cat(paste("Odds ratio by Mantel-Haenszel method =", round(mhor,3), "\n", "\n"))
### computing MLE-OR using clogit
}
library(survival)
model <- clogit(case ~ exposed + strata(strata))
clogitor <- exp(model$coefficients)
lnci95 <- c(model$coefficients-qnorm(0.975)*sqrt(model$var),model$coefficients+qnorm(0.975)*sqrt(model$var))
ci95.mleor <- exp(lnci95)
cat(paste("Odds ratio by maximum likelihood estimate (MLE) method =", round(clogitor,3),"\n","95%CI=",round(ci95.mleor[1],3),",",round(ci95.mleor[2],3), "\n"))
cat("\n")
}}
 
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
sortBy <- function(..., dataFrame = .data) {
data1 <- dataFrame
data1 <- data1[order(...),]
y <- setdiff(lsNoFunction(), as.character(ls.str(mode="list")[]))
if (length(y)>0){
for(i in 1:length(y)){
	if(length(get(y[i]))==nrow(data1)){
	nam <- y[i]
	assign (nam, (get(y[i]))[order(...)], env = .GlobalEnv)
	}
}
}
    detachAllData()
    assign(as.character(substitute(dataFrame)), data1, pos=1)
    attach(data1, name=as.character(substitute(dataFrame)), warn.conflicts = FALSE)
}

### One-way tabulation
tab1 <- function (x0, decimal=1, sort.group=c(FALSE,"decreasing","increasing"), cum.percent=!any(is.na(x0)), graph=TRUE, missing=TRUE, bar.values=c("frequency","percent", "none"), horiz=FALSE) {
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
	string3 <- paste(titleString()$distribution.of,string2)
	
	table.to.plot <- table(x0)
	if(missing==TRUE){table.to.plot <- table(x0,exclude=NULL)
	if(is.factor(x0)) {table.to.plot <- as.table(summary(x0))}
	if(is.na(names(table.to.plot)[length(names(table.to.plot))]) |  
		names(table.to.plot)[length(names(table.to.plot))]=="NA's") 
	names(table.to.plot)[length(names(table.to.plot))] <-"Missing"}
	scale.label <- as.character(titleString()$frequency)
	suppressWarnings(if(bar.values=="percent"){
      table.to.plot <- round(table.to.plot/sum(table.to.plot)*100,decimal)
      scale.label <- "%"
      })
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
	if((max(nchar(names(table.to.plot)))>8 & length(table.to.plot)>6)|horiz==TRUE){
		par(mai=c(0.95625, 0.1, 0.76875, 0.39375)+.1+c(0,par()$cin[1]*max(nchar(names(table.to.plot))*.75),0,0))
		barplot(table.to.plot,main=string3, horiz=TRUE, las=1, xlim=c(0, max(table.to.plot)*1.2), xlab = scale.label) -> y.coordinates
		suppressWarnings(if(bar.values=="frequency" | bar.values=="percent" | length(bar.values)==3){
    text(table.to.plot, y.coordinates, as.character(table.to.plot), pos=4, offset=0.3)
    })
    par(mai=c(0.95625, 0.76875, 0.76875, 0.39375))
		}else{
			barplot(table.to.plot, main=string3, ylab=scale.label, 
				ylim=c(0, max(table.to.plot)*1.1)) -> x.coordinates
		suppressWarnings(if(bar.values=="frequency" | bar.values=="percent" | length(bar.values)==3){
      text(x.coordinates, table.to.plot, as.character(table.to.plot), pos=3)
		})
}}
if(any(is.na(x0))){
	if(is.factor(x0)){
		output0 <- t(t(as.table(summary(x0))))
		output1 <- (t(t(table(x0))))
	}
	else{
		output0 <- t(t(table(x0, exclude=NULL)))
		output1 <- (t(t(table(x0))))
	}	
		percent0 <- output0[,1]/sum(output0)*100
		percent1 <- output1[,1]/sum(output1[,1],na.rm=TRUE)*100
if(cum.percent){
		output <- cbind(output0, round(percent0,decimal), round(cumsum(percent0),decimal), c(round(percent1,decimal),as.integer(0)), round(cumsum(c(percent1, as.integer(0))),decimal))
		}else{
    output <- cbind(output0, round(percent0,decimal), c(round(percent1,decimal),as.integer(0)))    
    }
suppressWarnings(if(sort.group=="decreasing"){
	output <- output[order(output[,1],decreasing=TRUE),]
})
suppressWarnings(if(sort.group=="increasing"){
	output <- output[order(output[,1],decreasing=FALSE),]
})
if(cum.percent){
		output <- rbind(output,c(sum(as.integer(output[,1])),100,100,100,100))
		colnames(output) <- c(.frequency,"  %(NA+)", "cum.%(NA+)","  %(NA-)","cum.%(NA-)")
  }else{
		output <- rbind(output,c(sum(as.integer(output[,1])),100,100))
		colnames(output) <- c(.frequency,"  %(NA+)", "  %(NA-)")  
  }
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
	percent <- output/sum(output)*100
if(cum.percent){
	output <- cbind(output,round(percent,decimal),round(cumsum(percent),decimal))
	output <- rbind(output,c(sum(output[,1]),100,100))
	colnames(output) <- c(.frequency1,.percent,.cum.percent)
  }else{
	output <- cbind(output,round(percent,decimal))
	output <- rbind(output,c(sum(output[,1]),100))
	colnames(output) <- c(.frequency1,.percent)
  }
	rownames(output)[length(rownames(output))] <- "  Total"
}
if(substring(search()[2], first=1, last=8)!="package:"){
options(warn=-1)
	first.line <- paste(as.character(substitute(x0)),":", attr(get(search()[2]), "var.labels")[attr(get(search()[2]), "names")==substitute(x0)], "\n","\n")
options(warn=TRUE)
}else{
  first.line <- paste(as.character(substitute(x0)),":", "\n") 
}
returns <- list(first.line=first.line, output.table=output)
class(returns) <- c("tab1", "list")
returns
}

### Print tab1 results
print.tab1 <- function(x, ...)
{
cat(x$first.line, "\n")
print(x$output.table, justify="right")
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
use <- function(filename, dataFrame = .data, clear=TRUE, tolower=TRUE) {
if(clear){
	detachAllData()
}
library(foreign)
if(is.character(filename)){
  ext <- tolower(substring(filename,first=nchar(filename)-3, last=nchar(filename)) )
  if( ext == ".dta") {
    data1 <- read.dta(filename)
    }else{
  if( ext == ".dbf") {
    data1 <- read.dbf(filename)
    if(tolower)	  names(data1) <- tolower(names(data1))    
    }else{
  if( ext == ".rec") {
	  data1 <- read.epiinfo(filename)
    if(tolower)	  names(data1) <- tolower(names(data1))
    }else{
    if( ext == ".sav") {
	 data1 <- read.spss(filename)
	 var.labels <- attr(data1, "variable.labels")
    if(tolower)	 names(data1) <- tolower(names(data1))
   data1 <- as.data.frame(data1)
	 attr(data1, "var.labels") <- var.labels
   }else{
  if( substring(filename,first=nchar(filename)-3, last=nchar(filename))==".csv") {
	 data1 <- read.csv(filename, header=TRUE, sep=",")
   }else{
   stop("This type of file cannot be 'use'd.")
   }
}}}}
}else{
if(is.data.frame(filename)){
  data1 <- filename
  }else{stop("The argument is not a data frame or no such file")}
}
assign(as.character(substitute(dataFrame)), data1, pos=1)
attach(data1, name=as.character(substitute(dataFrame)), warn.conflicts = FALSE)
}

### Dot plot
dotplot <- function(x, bin="auto", by=NULL, xmin=NULL, xmax=NULL, time.format=NULL, time.step=NULL, pch=18, dot.col="auto", ...){
if(!is.null(by)){
if(length(dot.col)>1 & length(table(by))!=length(dot.col)){
stop(paste("The argument 'dot.col' must either be \"auto\"","\n"," or number of colours equals to number of categories of 'by'."))
}}
if (bin=="auto"){
if(!is.null(attr(max(x, na.rm=TRUE)-min(x, na.rm=TRUE), "units")) & !any(class(x)=="difftime")){
  unit1 <- "weeks"
  bin <- as.numeric(difftime(max(x, na.rm=TRUE), min(x,na.rm=TRUE), unit=unit1))+1
while(bin!=trunc(bin)){
  if(unit1=="weeks"){ unit1 <- "days"
}else
  if(unit1=="days"){ unit1 <- "hours"
}else
  if(unit1=="hours"){ unit1 <- "mins"
}else
  if(unit1=="mins") unit1 <- "secs"
  bin <- as.numeric(difftime(max(x, na.rm=TRUE), min(x,na.rm=TRUE), unit=unit1))+1
  }
  }else{
if(is.integer(x)){
  bin <- as.integer(max(x, na.rm=TRUE)- min(x, na.rm=TRUE) +1)
}else{
if(any(class(x)=="Date")){
  bin <- as.numeric(difftime(max(x, na.rm=TRUE), min(x,na.rm=TRUE), unit=unit1))+1
 }else{
 bin <- 40
}}}}
character.x <- as.character(substitute(x))
		if(any(class(x)=="date")){
      x <- as.Date(paste(date.mdy(x)$year,"-", date.mdy(x)$month,"-", date.mdy(x)$day, sep=""))
    }
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
if(any(class(x)=="Date")){
	value <- as.numeric(value) 
}

xgr <- cut(value, breaks=bin, labels=FALSE, include.lowest=TRUE)
if(!is.null(xmax) & !is.null(xmin)){
original.lim <- c(min(value), max(value))
xgr.lim <- c(min(xgr), max(xgr))
lm00 <- lm(xgr.lim ~ original.lim)
newdata <- data.frame(original.lim=as.numeric(c(xmin, xmax)))
xgr1 <- predict.lm(lm00, newdata )
}
xgr <- as.numeric(xgr)
     string2 <- ifelse ((character.x[1]=="$" | character.x[1]==":"),paste(character.x[2],character.x[1],character.x[3],sep=""), character.x)
	byname <- as.character(substitute(by))

if(substring(search()[2],first=1,last=8)!="package:"){
	string2 <-  attr(.data, "var.labels")[attr(.data,"names")==string2]
	byname <-  attr(.data, "var.labels")[attr(.data, "names")==substitute(by)]
	
	if(length(string2)==0){
		string2 <- ifelse ((character.x[1]=="$" | character.x[1]==":"),paste(character.x[2],character.x[1],character.x[3],sep=""), character.x)
	}
	if(length(byname)==0){
		byname <- as.character(substitute(by))
	}else{
	if(byname==""){byname <- as.character(substitute(by))}
	}
	if(string2==""){
		string2 <- ifelse ((character.x[1]=="$" | character.x[1]==":"),paste(character.x[2],character.x[1],character.x[3],sep=""), character.x)
	}
}
string3 <- paste(titleString()$distribution.of,string2)
value.pretty <- pretty(value)
if(exists("xgr1")){ 
value.pretty <- pretty(c(xmin,xmax))
}
if(any(class(x)=="Date")) {
  range.date <- difftime(summary(x)[6], summary(x)[1])
	if(exists("xgr1")) {range.date <- difftime(xmax, xmin)}
  min.date <- summary(x)[1]
	if(exists("xgr1")) {min.date <- xmin}
	max.date <- summary(x)[6]
	if(exists("xgr1")) {max.date <- xmax}
	numdate <- (range.date)
    if(numdate <1){stop(paste("Only one day ie.",format(x,"%Y-%m-%d"),"not suitable for plotting"))}
	if(numdate <10){date.pretty <- seq(from=min.date,to=max.date,by="day"); format.time <- "%a%d%b"}
	if(numdate >=10 & numdate <30){date.pretty <- seq(from=min.date,to=max.date,by="2 day"); format.time <- "%d%b"}
	if(numdate >=30 & numdate <60){date.pretty <- seq(from=min.date,to=max.date,by="week"); format.time <- "%a %d"}
	if(numdate >=60 & numdate <700){date.pretty <- seq(from=min.date,to=max.date,by="month"); format.time <- "%d%b'%y"}
	if(numdate >=700){date.pretty <- seq(from=min.date,to=max.date,by="year")
  format.time <- "%d%b'%y"}
  if(!is.null(time.format)){format.time <- time.format}
  if(!is.null(time.step)){date.pretty <- seq(from=min.date,to=max.date,by=time.step)}
  value.pretty <- as.numeric(date.pretty)
}
if(any(class(x)=="POSIXt")){
	range.time <- difftime(summary(x)[6],summary(x)[1])
	if(exists("xgr1")) {range.time <- difftime(xmax, xmin)}
  min.time <- summary(x)[1]
	if(exists("xgr1")) {min.time <- xmin}
	max.time <- summary(x)[6]
	if(exists("xgr1")) {max.time <- xmax}
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
  if(!is.null(time.format)){format.time <- time.format}
  if(!is.null(time.step)){step <- time.step}
	time.pretty <- seq(from=min.time,to=max.time,by=step)
	value.pretty <- as.numeric(time.pretty)
}
	xlim <- c(min(xgr),max(xgr))
	value.lim <- c(min(value), max(value))
	if(exists("xgr1")) {
  xlim <- c(min(xgr1),max(xgr1))
  value.lim <- as.numeric(c(xmin, xmax))
}
	glm(xlim~value.lim)->model1
	xgr.pretty <- model1$coefficient[1] + model1$coefficient[2]*value.pretty
if(is.null(by)){
if(dot.col=="auto") dot.col <- "black"	
	xgr <- sort(xgr)
	freq <- rep(1, length(value))
	for(i in 1:max(xgr)){
		freq[xgr==i] <- 1:sum(xgr==i) 
	}
	if(max(freq)<20){
		plot(xgr,freq, xaxt="n", xlab=" ",main=string3,	ylab=titleString()$frequency,
      ylim=c(0,20), xlim = xlim, pch=pch, col=dot.col[1], ...)
	}else{
  plot(xgr,freq, xaxt="n", xlab=" ",main=string3,	ylab=titleString()$frequency, 
       xlim = xlim, pch=pch, col=dot.col[1], ...)
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
	main.lab <- paste(string3,titleString()$by,byname)
	if(nchar(main.lab)>45){main.lab <- paste(string3,"\n",titleString()$by,byname)}
  if(any(dot.col=="auto")){
  dot.col1 <- as.numeric(by1)
  }else{
  dot.col1 <- rep(dot.col[1], length(by1))
  if(length(dot.col) > 1){
  for(i in length(table(by1))){
  dot.col1[by1==as.numeric(names(table(by1)))[i]] <- dot.col[i] 
  }
  }}
  if(max(y)<20){
	plot(xgr,y, xaxt="n", yaxt="n",
		xlab=" ",main=main.lab, ylim=c(-1,20),
		ylab=" ", col=dot.col1, pch=pch, xlim=xlim, ...)
	}else{
	plot(xgr,y, xaxt="n", yaxt="n",
		xlab=" ",main=main.lab, ylim=c(-1,max(y)),
		ylab=" ", col=dot.col1, pch=pch, xlim=xlim, ...)
	}
	abline(h=yline, col="blue")
	axis(2,at=yline, labels=levels(by1), padj=0, las=1, ...)
	par(mai=c(0.95625, 0.76875, 0.76875, 0.39375))
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
	if(any(class(x)=="numeric") || any(class(x)=="integer")){
		axis(side=1,at=xgr.pretty, labels=value.pretty)
	}
}

### Labeling variables
label.var <-function(var, label, pack=TRUE, dataFrame = .data){
# Store list of variable labels, 
	#if exist, in a temporary vector
data1 <- dataFrame
if(any(names(data1)==as.character(substitute(var)))){
	if(is.null(attributes(data1)$var.labels)){
		attributes(data1)$var.labels <- rep("", length(names(data1)))
	}
	attributes(data1)$var.labels[names(data1)==as.character(substitute(var))] <- label
}else{
  if(length(var) != nrow(dataFrame)){
  stop(paste("The length of", as.character(substitute(var)), "is not equal to number of rows of", as.character(substitute(dataFrame))))
  }
	old.labels <-attributes(data1)$var.labels
	data1[,ncol(data1)+1]<- var
	names(data1)[length(names(data1))] <- as.character(substitute(var))
	if(is.null(old.labels)){
		attributes(data1)$var.labels <- c(rep("", length(names(data1))-1),label)
	}else{
	attributes(data1)$var.labels <- c(old.labels,label)
	}
}
if(exists(as.character(substitute(var)))){
  if(!is.atomic(var)){
  stop(paste("A non-variable object", as.character(substitute( var)),"exists in the environment and cannot be labelled.","\n", 
  " If this variable in the data frame is to be labelled,","\n",
  " either the non-variable object of this name must be removed before labelling","\n", "\n",
  paste("   rm(",as.character(substitute( var)),")",";             ",
  " label.var(", as.character(substitute(var)),", \"", as.character(substitute(label)),"\")",sep=""),"\n", "\n",
  " or the variable in the data frame must be prior renamed","\n",  "\n",
  paste("   ren(", as.character(substitute( var)),", newname)", "; ",
  " label.var(newname,\"", as.character(substitute(label)),"\")", sep=""), "\n"))
  }
  if(length(var)==nrow(dataFrame)){
	data1[,names(data1)==as.character(substitute(var))] <- var
  }else{
  stop(paste("The length of", as.character(substitute(var)), "is not equal to number of rows of", as.character(substitute(dataFrame))))
  }
}
if(pack){
	suppressWarnings(rm(list=as.character(substitute(var)), pos=1))
}
    assign(as.character(substitute(dataFrame)), data1, pos=1)
    if(is.element(as.character(substitute(dataFrame)), search())){
      if(length(which(search() %in% as.character(substitute(dataFrame))))>1){
       warning(paste("\n","There are more than one '", as.character(substitute(dataFrame)),"' attached!","\n", sep=""))
      }
      detach(pos=which(search() %in% as.character(substitute(dataFrame)))[1])
      attach(data1, name=as.character(substitute(dataFrame)), warn.conflicts = FALSE)
    }
}

### Recoding a variable or set of variables for the same final value
recode <- function (vars, old.value, new.value, dataFrame = .data) 
{
        data1 <- dataFrame
        nl <- as.list(1:ncol(data1))
        names(nl) <- names(data1)
            var.order <- eval(substitute(vars), nl, parent.frame())
    if (is.numeric(old.value) | is.integer(old.value) | any(class(data1[,var.order]) == 
        "POSIXt")) {
        data1[, var.order][data1[, var.order] == old.value] <- new.value
    }
    else for (i in var.order) {
        if (is.factor(data1[, i])) {
            if (is.character(old.value)) {
                levels(data1[, i])[levels(data1[, i]) == old.value] <- new.value
            }
        }
    }
    if (length(old.value) == nrow(data1)) {
        if (length(var.order) == 1) {
            data1[, var.order] <- replace(data1[, var.order], 
                old.value, new.value)
        }
        else {
            for (i in 1:length(var.order)) {
                data1[, var.order[i]] <- replace(data1[, var.order[i]], 
                  old.value, new.value)
            }
        }
    }
    assign(as.character(substitute(dataFrame)), data1, pos=1)
    if(is.element(as.character(substitute(dataFrame)), search())){
      detach(pos=which(search() %in% as.character(substitute(dataFrame))))
      attach(data1, name=as.character(substitute(dataFrame)), warn.conflicts = FALSE)
    }
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
vector1 <-  setdiff(ls(envir= .GlobalEnv), lsf.str()[])
rm(list=vector1, pos=1)
} 

### Pyramid of age by sex
pyramid <- function(age, sex, binwidth=5, inputTable=NULL, printTable=FALSE, 
  percent=c("none","each","total"), decimal=3, ...){
if(is.null(inputTable)){
agegr <- cut(age, br = ( (min(age, na.rm=TRUE)%/%binwidth)
:(max(age,na.rm=TRUE)%/%binwidth+ (max(age,na.rm=TRUE)%%binwidth>0))*binwidth), include.lowest=TRUE)
	age.sex.table <- table(agegr, sex, deparse.level=1, dnn=list(substitute(age),substitute(sex)))
if(ncol(table(agegr,sex))!=2)stop("There must be two genders")
	age.sex.table.dimnames <- names(attr(age.sex.table,"dimnames"))
}else{
if(is.matrix(inputTable) | is.table(inputTable)){
  age.sex.table <- inputTable
	age.sex.table.dimnames <- names(attr(inputTable,"dimnames"))
  }
}
par(mfrow=c(1,2))
old.par.mai <- c(0.95625, 0.76875, 0.76875, 0.39375)
left.par.mai <- old.par.mai
right.par.mai <- c(old.par.mai[1],old.par.mai[4],old.par.mai[3],old.par.mai[2])
column.names <-colnames(age.sex.table)
suppressWarnings(if(percent=="each"){
  age.sex.table<-cbind(age.sex.table[,1]/colSums(age.sex.table)[1]*100,age.sex.table[,2]/colSums(age.sex.table)[2]*100)
  column.names -> colnames(age.sex.table)
    age.sex.table1 <- round(age.sex.table, digits=decimal)
    table.header <- "(percentage of each sex)."
})
suppressWarnings(if(percent=="total"){
  age.sex.table<-cbind(age.sex.table[,1]/sum(age.sex.table),age.sex.table[,2]/sum(age.sex.table))*100
  column.names -> colnames(age.sex.table)
    age.sex.table1 <- round(age.sex.table, digits=decimal)
    table.header <- "(percentage of each sex)."
})
par(mai=left.par.mai)
barplot(-age.sex.table[,1], horiz=TRUE,yaxt="n",xlab=colnames(age.sex.table)[1], xlim=c(-max(age.sex.table),0),xaxt="n",...)-> label.points
axis(side=1, at=pretty(c(-max(age.sex.table),0)),labels=-pretty(c(-max(age.sex.table),0)))
axis(side=4,at=label.points, labels=rownames(age.sex.table), tick=FALSE, col="blue", las=2)
par(mai=right.par.mai)
barplot(age.sex.table[,2], horiz=TRUE, yaxt="n",xlab=colnames(age.sex.table)[2], xlim=c(0,max(age.sex.table)), ...) 
par(mfrow=c(1,1))
par(mai=old.par.mai)

if(printTable & is.null(inputTable)){
  cat("\n","Tabulation of age by sex ")
  if (!exists("age.sex.table1")) {
    table.header <- "(frequency)."  
    age.sex.table1 <- age.sex.table
  }
  cat(table.header, "\n")
  print.noquote(age.sex.table1)
  cat("\n")
} 
if(is.null(inputTable)){
  returns <- list(output.table=age.sex.table, ageGroup=agegr) 
  }
}

## Followup plot
followup.plot <- function (id, time, outcome, by = NULL, n.of.lines = NULL, legend = TRUE, 
    line.col = "blue", stress = NULL, stress.labels = FALSE, label.col = 1, stress.col = NULL, 
    stress.width = NULL, stress.type = NULL, ...) 
{

    plot(time, outcome, xlab = " ", ylab = " ", type = "n", ...)
id1 <- id; time1 <- time; by1 <- by; outcome1 <- outcome
    if (is.null(n.of.lines)) {
        if (!is.null(by)) {
            id <- id[order(id1, time1, by1)]
            id.factor <- factor(id)
            time <- time[order(id1, time1, by1)]
            outcome <- outcome[order(id1, time1, by1)]
            by <- by[order(id1, time1, by1)]
            by.factor <- factor(by)
            for (i in 1:length(levels(by.factor))) {
                for (j in 1:length(levels(id.factor))) {
                  lines(time[id.factor == levels(id.factor)[j] & by.factor == levels(by.factor)[i]], 
                  outcome[id.factor == levels(id.factor)[j] & by.factor == levels(by.factor)[i]], 
                    col = i, lty = i)
                }
            }
            if (legend) {
                legend("topright", legend = levels(factor(by)), 
                  col = 1:length(levels(factor(by))), bg = "white", 
                  lty = 1:length(levels(factor(by))))
            }
        }
        else {
            id <- id[order(id1, time1)]
            id.factor <- factor(id)
            time <- time[order(id1, time1)]
            outcome <- outcome[order(id1, time1)]
            if (length(levels(factor(id))) < 8) {
                for (j in 1:length(levels(id.factor))) {
                  lines(time[id.factor == levels(id.factor)[j]], 
                    outcome[id.factor == levels(id.factor)[j]], 
                    col = j)
                  }
                if (legend) {
                  legend("topright", legend = levels(factor(id[order(id1)])), 
                    col = 1:length(levels(factor(id))), bg = "white", 
                    lty = 1)
                }
            }
            else {
                for (j in 1:length(levels(id.factor))) {
                  if (line.col == "multicolor") {
                    lines(time[id.factor == levels(id.factor)[j]], 
                      outcome[id.factor == levels(id.factor)[j]], 
                      col = j)
                  }
                  else {
                    lines(time[id.factor == levels(id.factor)[j]], 
                      outcome[id.factor == levels(id.factor)[j]], 
                      col = line.col)
                  }
                }
            }
        }
    }
    else {
        order.id.selected <- sample(c(rep(TRUE, n.of.lines), 
            rep(FALSE, length(levels(factor(id))) - n.of.lines)))
        if (!is.null(by)) {
            id <- id[order(id1, time1, by1)]
            time <- time[order(id1, time1, by1)]
            outcome <- outcome [order(id1, time1, by1)]
            by <- by[order(id1, time1, by1)]
            id.factor <- factor(id)
            by.factor <- factor(by)
            for (i in 1:length(levels(by.factor))) {
                for (j in 1:length(levels(id.factor))) {
                  lines(time[id.factor == levels(id.factor)[j] & 
                    by.factor == levels(by.factor)[i]] * order.id.selected[j], 
                    outcome[id.factor == levels(id.factor)[j] & 
                      by.factor == levels(by.factor)[i]] * order.id.selected[j], 
                    col = i, lty = i)
                }
            }
            if (legend) {
                legend("topright", legend = levels(factor(by)), 
                  col = 1:length(levels(factor(by))), lty = 1:length(levels(factor(by))), 
                  bg = "white")
            }
        }
        else {
            id <- id[order(id1, time1)]
            id.factor <- factor(id)
            time <- time[order(id1, time1)]
            outcome <- outcome[order(id1, time1)]
            for (j in 1:length(levels(id.factor))) {
                if (line.col == "multicolor") {
                  lines(time[id.factor == levels(id.factor)[j]] * 
                    order.id.selected[j], outcome[id.factor == 
                    levels(id.factor)[j]] * order.id.selected[j], 
                    col = j)
                }
                else {
                  lines(time[id.factor == levels(id.factor)[j]] * 
                    order.id.selected[j], outcome[id.factor == 
                    levels(id.factor)[j]] * order.id.selected[j], 
                    col = line.col)
                }
            }
        }
    }
    for (j in 1:length(levels(id.factor))) {
                  text(time[id.factor == levels(id.factor)[j]],
                  outcome[id.factor ==  levels(id.factor)[j]], 
                    labels = j, col = any(stress.labels*stress %in% j)*label.col)
    }
    for (j in 1:length(levels(id.factor))) {
                  lines(time[id.factor == levels(id.factor)[j]],
                  outcome[id.factor ==  levels(id.factor)[j]], 
                    col = any(stress %in% j)*stress.col,
                    lwd = stress.width, lty = stress.type)
    }
}

## Subsetting .data
keepData <- function (dataFrame = .data, sample = NULL, exclude = NULL, subset, select, 
    drop = FALSE, ...) 
{

dataName <- as.character(substitute(dataFrame))
    data1 <- dataFrame
    datalabel <- attr(data1, "datalabel")
    val.labels <- attr(data1, "val.labels")
    var.labels <- attr(data1, "var.labels")
    label.table <- attr(data1, "label.table")
    if (!is.null(sample)) {
        if (!is.numeric(sample) | sample <= 0 | length(sample) > 
            1 | (trunc(sample) != sample) & sample > 1) {
            stop("Size of sample must be a positive integer")
        }
        if(sample < 1) {
            sample0 <- sample; 
            sample <- trunc(sample*nrow(data1))
            cat("Keep only ", round(sample0*100,2),"% or ", sample, " of the total ", nrow(data1), " records","\n", sep="")
            }
        dataFrame <- dataFrame[sample(nrow(dataFrame), sample), ]
        data1 <- dataFrame
        attr(data1, "datalabel") <- paste(datalabel, "(subset)")
        attr(data1, "val.labels") <- val.labels
        attr(data1, "var.labels") <- var.labels
        attr(data1, "label.table") <- label.table
    }
        if (missing(subset)) 
            r <- TRUE
        else {
            e <- substitute(subset)
            r <- eval(e, dataFrame, parent.frame())
            if (!is.logical(r)) 
                stop("'subset' must evaluate to logical")
            r <- r & !is.na(r)
        }
        if (missing(select)) {
            vars <- TRUE
            if (suppressWarnings(!is.null(exclude))) {
                nl <- as.list(1:ncol(dataFrame))
                names(nl) <- names(dataFrame)
                if ((length(grep(pattern = "[*]", as.character(substitute(exclude)))) == 
                  1) | (length(grep(pattern = "[?]", as.character(substitute(exclude)))) == 
                  1)) {
                  vars <- -grep(pattern = glob2rx(as.character(substitute(exclude))), 
                    names(dataFrame))
                  if (length(vars) == 0) {
                    stop(paste(as.character(substitute(exclude)), 
                      "not matchable with any variable name."))
                  }
                }
                else {
                  vars <- -eval(substitute(exclude), nl, parent.frame())
                }
            }
        }
        else {
            nl <- as.list(1:ncol(dataFrame))
            names(nl) <- names(dataFrame)
            if ((length(grep(pattern = "[*]", as.character(substitute(select)))) == 
                1) | (length(grep(pattern = "[?]", as.character(substitute(select)))) == 
                1)) {
                vars <- grep(pattern = glob2rx(as.character(substitute(select))), 
                  names(dataFrame))
                if (length(vars) == 0) {
                  stop(paste(select, "not matchable with any variable name."))
                }
            }
            else {
                vars <- eval(substitute(select), nl, parent.frame())
            }
        }
        data1 <- dataFrame[r, vars, drop = drop]
        attr(data1, "datalabel") <- paste(datalabel, "(subset)")
        attr(data1, "val.labels") <- val.labels[vars]
        attr(data1, "var.labels") <- var.labels[vars]
        attr(data1, "label.table") <- label.table[is.element(names(label.table), 
            val.labels[vars])][1:length(vars)]

    assign(dataName, data1, pos=1)
    if(is.element(dataName, search())){
      detach(pos=which(search() %in% dataName))
      attach(data1, name=dataName, warn.conflicts = FALSE)
    }
}



## Adjusted mean, proportion and rate
adjust <- function(adjust = NULL, by, model, standard=NULL, offset=FALSE, 
   type = c("response", "link"), se.fit=TRUE, alpha=.05, ci=FALSE, 
   ...){
if(length(type)==2) type <- "response"
if(missing(by)) stop ("'by' variable(s) missing!")
if(!any(class(model)=="glm") & !any(class(model)=="lm")) stop("Model must be in class 'lm' or 'glm'")
nl <- as.list(1:ncol(model$model))
names(nl) <- names(model$model)
by.vars <- as.numeric(eval(substitute(by), envir=nl, enclos=parent.frame()))
adjust.vars <- eval(substitute(adjust), nl, parent.frame())
mod <- model.matrix(model)
if(length(grep("offset", names(model$model)))> 0 ){
mod <- data.frame(model.matrix(model),model$model[,ncol(model$model)])
names(mod) <- c(colnames(model.matrix(model)),"offset1")
}
newdata0 <- aggregate.data.frame(mod, by=as.list(model$model)[by.vars], FUN="mean")
if(length(grep("offset", names(model$model)))> 0 ){
if(!offset) {
newdata0$offset1 <- 0
}else{
persontime <- exp(model$model[,ncol(model$model)])
newdata0$offset1 <- log(aggregate.numeric(persontime,by=as.list(model$model)[by.vars],FUN="mean")$mean.persontime)
}
}
means <- apply(mod,2,mean)
if(!is.null(adjust)){
selected.names <- names(model$model)[adjust.vars]
for(i in 1:length(selected.names)){
j0 <- grep(selected.names[i], names(newdata0))
j1 <- grep(selected.names[i], names(means))
for(k in 1:length(j0)){
newdata0[,j0[k]] <- means[j1[k]]
}
}
}
data.for.new.glm <- data.frame(model$model[,1], model.matrix(model)[,-1])
names(data.for.new.glm)[-1] <- colnames(model.matrix(model))[-1]
names(data.for.new.glm)[1] <- "y"
offset1 <- model$offset
newglm <- glm(y ~.,  data=data.for.new.glm, family=as.character(model$family)[1])
if(length(grep("offset", names(model$model)))> 0 ){
newglm <- glm(y ~., offset= offset1,  family=as.character(model$family)[1],data=data.for.new.glm )
}
newdata1 <- newdata0[,(ncol(newdata0)-(ncol(data.for.new.glm)-2)):ncol(newdata0)]
if(length(grep("offset", names(model$model)))> 0 ){
newdata1 <- newdata0[,(ncol(newdata0)-(ncol(data.for.new.glm)-1)):ncol(newdata0)  ]
}
if(!is.null(standard)){
if(length(standard)!=length(newglm$coefficient)-1){stop("In appropriate length of standard value(s) for predictor(s)")}
for(i in 1:length(standard)){
if(!is.na(standard[i])){newdata1[,i] <- standard[i]}
}
}
result.gold <- as.data.frame(predict.glm(newglm,newdata=newdata1, type="link", 
    se.fit)[c(1,2)])
result0 <- predict.glm(newglm,newdata=newdata1, type, se.fit)[c(1,2)]
result <- data.frame(newdata0[,1:length(by.vars)], as.data.frame(result0))
names(result)[1:length(by.vars)] <- names(model$model)[by.vars]
if(!se.fit) names(result)[length(names(result))] <- "fit"
if(as.character(model$family[1])=="binomial" & type=="response") {
result$se.fit <- NULL
names(result)[ncol(result)] <- "probability"
if(ci){
result$ul <- result.gold[,1] - qnorm(1-alpha/2)* result.gold[,2]
result$ul <- exp(result$ul)/(1+exp(result$ul))
result$ll <- result.gold[,1] + qnorm(1-alpha/2)* result.gold[,2]
result$ll <- exp(result$ll)/(1+exp(result$ll))
names(result)[c(ncol(result)-1, ncol(result))] <- c(paste("lower",100-100*alpha,"ci",sep=""),paste("upper",100-100*alpha,"ci",sep=""))
}
}
if(as.character(model$family[1])=="poisson" & type=="response") {
result$se.fit <- NULL
if(!offset){
names(result)[ncol(result)] <- "rate"
}else{
names(result)[ncol(result)] <- "count"
}
if(ci){
result$ul <- result.gold[,1] - qnorm(1-alpha/2)* result.gold[,2]
result$ul <- exp(result$ul)
result$ll <- result.gold[,1] + qnorm(1-alpha/2)* result.gold[,2]
result$ll <- exp(result$ll)
names(result)[c(ncol(result)-1, ncol(result))] <- c(paste("lower",100-100*alpha,"ci",sep=""),paste("upper",100-100*alpha,"ci",sep=""))
}
}
if(as.character(model$family[1])=="gaussian" & type=="response") {
names(result)[names(result)=="fit"] <- "mean"
names(result)[names(result)=="se.fit"] <- "se.mean"
}
if(ci & (type=="link"| as.character(model$family[1])=="gaussian") & se.fit){
result$ul <- result[,ncol(result)-1] - qnorm(1-alpha/2)* result[,ncol(result)]
result$ll <- result[,ncol(result)-2] + qnorm(1-alpha/2)* result[,ncol(result)-1]
names(result)[c(ncol(result)-1, ncol(result))] <- c(paste("lower",100-100*alpha,"ci",sep=""),paste("upper",100-100*alpha,"ci",sep=""))
}
result
}
## Aggregate a numeric variable
aggregate.numeric <- function (x, by, FUN = c("count", "sum", "mean", "median", "sd", "min", 
    "max"), na.rm = TRUE, length.warning = TRUE, ...) 
{

count <- function(x1) {length(na.omit(x1))}
    if (length(FUN) == 1 & class(FUN) == "function") {
        FUN <- as.character(substitute(FUN))
    }
    else {

if(any(is.na(x)) & na.rm==FALSE & (is.element("var", FUN) | is.element("sd", FUN))){
cat(paste("\n","   'FUN = \"var\"' and 'FUN = \"sd\" not computable when 'na.rm=FALSE'","\n","   and therefore omitted"), "\n", "\n")
FUN <- setdiff(FUN, c("sd", "var"))
}
if(length(FUN)==0)
{stop("Too few FUN's")}

        if (any(is.na(x)) & length.warning & na.rm) {
            if (any(FUN == "var") | any(FUN == "sd") | any(FUN=="mean") |any(FUN=="sum")) {
                #na.rm <- TRUE
                cat("\n", "Note:","\n","     Missing values removed.","\n")
            }
            if (any(FUN == "length")) {
                cat("     'length' computed with missing records included.", 
                  "\n")
            }
            cat("\n")
        }
    }
    if (FUN[1] != "length") {
        if(FUN[1]=="count"){
        y <- aggregate.data.frame(x, by, FUN = count)
            names(y)[length(names(y))] <- paste("count", as.character(substitute(x)), 
               sep = ".")
        }else{
            if(FUN[1]=="sum"|FUN[1]=="mean"|FUN[1]=="median"|FUN[1]=="var"|
                  FUN[1]=="sd"|FUN[1]=="min"|FUN[1]=="max"){
              y <- aggregate.data.frame(x, by, FUN = FUN[1], na.rm = na.rm)
            }else{
            y <- aggregate.data.frame(x, by, FUN = FUN[1])
            }
            names(y)[length(names(y))] <- paste(FUN[1], as.character(substitute(x)), 
               sep = ".")
            }
    }
    else {
        y <- aggregate.data.frame(x, by, FUN = length)
        names(y)[length(names(y))] <- FUN[1]
    }
    if (length(FUN) > 1) {
        for (i in 2:length(FUN)) {
            if (FUN[i] != "length") {
                if(FUN[i]=="count"){
                y1 <- aggregate.data.frame(x, by, FUN = count)
                y <- data.frame(y, y1[, length(names(y1))])
                }else{
                if(FUN[i]=="sum"|FUN[i]=="mean"|FUN[i]=="median"|FUN[i]=="var"|
                  FUN[i]=="sd"|FUN[i]=="min"|FUN[i]=="max"){
                  y1 <- aggregate.data.frame(x, by, FUN = FUN[i], 
                  na.rm = na.rm)
                  }else{
                y1 <- aggregate.data.frame(x, by, FUN = FUN[i])
                }
                y <- data.frame(y, y1[, length(names(y1))])
                }
                names(y)[length(names(y))] <- paste(FUN[i], as.character(substitute(x)), 
                  sep = ".")
            }
            else {
                y1 <- aggregate.data.frame(x, by, FUN = length)
                y <- data.frame(y, y1[, length(names(y1))])
                names(y)[length(names(y))] <- FUN[i]
            }
        }
    }
    y
}

## Confidence interval
ci <- function(x, ...){
UseMethod("ci")
}
ci.default <- function(x, ...){
    if (is.logical(x)){ 
    ci.binomial(x, ...)
    }else{
        if(is.numeric(x)){
         if(min(x, na.rm=TRUE)==0 & max(x, na.rm=TRUE)==1){
          ci.binomial(x,  ...)
          }
          }else{
              if(is.factor(x) & length(levels(x))==2){
              x <- as.numeric(unclass(x))-1
              ci.binomial(x, ...)
              }else{
               ci.numeric(x, ...)
              }
          }
        }
}


ci.binomial <- function(x, size, precision, alpha=.05, ...){
success <- x
if(missing(size)){
success1 <- success
if(min(success, na.rm=TRUE)!=0 | max(success, na.rm=TRUE)!=1){stop("This is not a binary vector.")}
success <- length(na.omit(success1)[na.omit(success1) >0 ])
size <- length(na.omit(success1))
}
reverse <- rep(FALSE, length(success))
reverse[success/size > .5] <- TRUE

success[reverse] <- size[reverse]-success[reverse]
if(missing(precision)){
precision <- success/size/10000}
precision[success==0 |success==size] <-.01/size[success==0 |success==size]
probab <- success/size
success1 <- success
success1[success > 0] <-  success[success > 0]-1
for(i in 1:length(success)){while(pbinom(success1[i], size[i], probab[i], lower.tail=FALSE) > alpha/2){
probab[i] <- probab[i] - precision[i]
}}
estimate <- success/size
se <- sqrt(estimate*(1-estimate)/size)
ll <- probab

probab <- success/size
for(i in 1:length(success)){while(pbinom(success[i], size[i], probab[i], lower.tail=TRUE) > alpha/2){
probab[i] <- probab[i]+ precision[i]
}}
ul <- probab
data.frame.a <- data.frame(events=success,total=size,probability = estimate, se=se,
  ll=ll, ul=ul)
data.frame.a[reverse,] <- data.frame(events=size[reverse]-success[reverse],total=size[reverse],probability = 1-estimate[reverse], se=se[reverse], ll=1-ul[reverse], ul=1-ll[reverse])

names(data.frame.a)[5] <- paste("exact.lower",100*(1-alpha),"ci",sep="")
names(data.frame.a)[6] <- paste("exact.upper",100*(1-alpha),"ci",sep="")
if(nrow(data.frame.a)==1){rownames(data.frame.a) <- ""}
data.frame.a
}

## Confidence interval of continuous variable(s)
ci.numeric <- function(x, n, sds, alpha=.05, ...){
means <- x
mean1 <- means
if(missing(n) & missing(sds)){
means <- mean(mean1, na.rm=TRUE)
n <- length(na.omit(mean1))
sds <- sd(mean1, na.rm=TRUE)
}
se <- sds/sqrt(n)
ll <- means - qt(p=(1-alpha/2), df = n-1)*se
ul <- means + qt(p=(1-alpha/2), df = n-1)*se
data.frame.a <- data.frame(n=n,mean=means, sd=sds, se=se, 
  ll=ll, ul=ul)
names(data.frame.a)[5] <- paste("lower",100*(1-alpha),"ci",sep="")
names(data.frame.a)[6] <- paste("upper",100*(1-alpha),"ci",sep="")
if(nrow(data.frame.a)==1){rownames(data.frame.a) <- ""}
data.frame.a
}

# Confidence interval for Poisson variables
ci.poisson <- function(x, person.time, precision,  alpha=.05, ...){
count <- x
incidence <- count/person.time
if(missing(precision)){
precision <- incidence/1000
precision[incidence==0] <- 0.001/person.time[incidence==0] 
}
lamda <- incidence * person.time
for(i in 1:length(count)){
while(ppois(count[i], lamda[i], lower.tail=TRUE) > alpha/2){
incidence[i] <- incidence[i] + precision[i]
lamda[i] <- incidence[i] * person.time[i]
}}
ul <- incidence

incidence <- count/person.time
lamda <- incidence * person.time
count1 <- count-1
count1[count==0] <- count[count==0]
for(i in 1:length(count)){
while(ppois(count1[i], lamda[i], lower.tail=FALSE) > alpha/2){
incidence[i] <- incidence[i] - precision[i]
lamda[i] <- incidence[i] * person.time[i]
}}
ll <- incidence
data.frame.a <- data.frame(events=count,person.time=person.time,incidence=count/person.time, se=sqrt(count)/person.time, ll=ll, ul=ul)
names(data.frame.a)[5] <- paste("exact.lower",100*(1-alpha),"ci",sep="")
names(data.frame.a)[6] <- paste("exact.upper",100*(1-alpha),"ci",sep="")
if(nrow(data.frame.a)==1){rownames(data.frame.a) <- ""}
data.frame.a
}

# Rename 
rename <- function(x1, x2, dataFrame = .data, ...){
UseMethod("rename")
}
ren <- rename
rename.default <- function (x1, x2, dataFrame = .data, ...) 
{
data1 <- dataFrame
    if (any(names(data1) == as.character(substitute(x1)))) {
        names(data1)[names(data1) == as.character(substitute(x1))] <- as.character(substitute(x2))
    assign(as.character(substitute(dataFrame)), data1, pos=1)
    if(is.element(as.character(substitute(dataFrame)), search())){
      detach(pos=which(search() %in% as.character(substitute(dataFrame))))
      attach(data1, name=as.character(substitute(dataFrame)), warn.conflicts = FALSE)
    }
    }
    else {
        if (length(grep(pattern = x1, x = names(data1))) > 0) {
            rename.pattern(x1, x2, printNote = TRUE, dataFrame = .data)
        }
        else {
            stop(paste("\n", "\"", as.character(substitute(x1)), 
                "\"", " is neither a var name nor an available pattern"))
        }
    }
}

## Rename a variable
rename.var <- function (x1, x2, dataFrame = .data, ...) 
{
data1 <- dataFrame
    if (any(names(data1) == as.character(substitute(x1)))) {
        names(data1)[names(data1) == as.character(substitute(x1))] <- as.character(substitute(x2))
    assign(as.character(substitute(dataFrame)), data1, pos=1)
    if(is.element(as.character(substitute(dataFrame)), search())){
      detach(pos=which(search() %in% as.character(substitute(dataFrame))))
      attach(data1, name=as.character(substitute(dataFrame)), warn.conflicts = FALSE)
    }
    }
    else {
        if (any(names(data1) == x1)) {
            names(data1)[names(data1) == x1] <- as.character(substitute(x2))
    assign(as.character(substitute(dataFrame)), data1, pos=1)
    if(is.element(as.character(substitute(dataFrame)), search())){
      detach(pos=which(search() %in% as.character(substitute(dataFrame))))
      attach(data1, name=as.character(substitute(dataFrame)), warn.conflicts = FALSE)
    }
        }
        else {
            stop(paste("\n", "\"", as.character(substitute(x1)), 
                "\"", " does not exist in the data frame", 
                sep = ""))
        }
    }
}

## Rename pattern of variables
rename.pattern <- function (x1, x2, dataFrame = .data, printNote = TRUE, ...) 
{
data1 <- dataFrame
    if (length(grep(pattern = x1, x = names(data1))) == 0) 
        stop(paste("Pattern ", "\"", as.character(substitute(x1)), 
            "\"", " does not exist", sep = ""))
    table1 <- cbind(names(data1)[grep(pattern = x1, x = names(data1))], 
        sub(pattern = x1, replacement = x2, x = names(data1))[grep(pattern = x1, 
            x = names(data1))])
    rownames(table1) <- rep("    ", length(names(data1)[grep(pattern = x1, 
        x = names(data1))]))
    colnames(table1) <- c("Old var names  ", "New var names")
    if (printNote) {
        cat("Note the following change(s) in variable name(s):", 
            "\n")
        print(table1)
    }
    names(data1) <- sub(pattern = x1, replacement = x2, x = names(data1))
    assign(as.character(substitute(dataFrame)), data1, pos=1)
    if(is.element(as.character(substitute(dataFrame)), search())){
      detach(pos=which(search() %in% as.character(substitute(dataFrame))))
      attach(data1, name=as.character(substitute(dataFrame)), warn.conflicts = FALSE)
    }
}


## Expand
expand <- function(aggregate.data, index.var="Freq", retain.freq=FALSE){
output <- NULL
for(i in 1:nrow(aggregate.data)){
if(retain.freq){
output <- rbind(output, aggregate.data[rep(i, aggregate.data[,which(names(aggregate.data)==index.var)][i]),])
}else{
output <- rbind(output, aggregate.data[rep(i, aggregate.data[,which(names(aggregate.data)==index.var)][i]),][,-which(names(aggregate.data)==index.var)])
}}
data.frame(output,row.names=1:nrow(output))}
## BE to AD
be2ad <- function(Date.in.BE){
if(class(Date.in.BE)!="Date") {stop("The class of the variable must be Date")}
year <- format(Date.in.BE, "%Y")
year <- as.integer(year)
AD <- year-543
month <- format(Date.in.BE, "%m")
day <- format(Date.in.BE, "%d")
as.Date(paste(AD,"-",month,"-",day, sep=""))
}

## Cronbach's alpha
alpha <- function (vars, dataFrame = .data, casewise = FALSE, reverse = TRUE, 
    decimal = 4, vars.to.reverse = NULL, var.labels = TRUE, var.labels.trunc=150) 
{
    if (casewise) {
        usage <- "complete.obs"
    }
    else {
        usage <- "pairwise.complete.obs"
    }
    nl <- as.list(1:ncol(dataFrame))
    names(nl) <- names(dataFrame)
    selected <- eval(substitute(vars), nl, parent.frame())
    selected.dataFrame <- dataFrame[, selected]
    selected.matrix <- NULL
    for (i in selected) {
        selected.matrix <- cbind(selected.matrix, unclass(dataFrame[, 
            i]))
    }
    colnames(selected.matrix) <- names(selected.dataFrame)

        nl1 <- as.list(1:ncol(dataFrame[, selected]))
        names(nl1) <- names(dataFrame[, selected])
        which.neg <- eval(substitute(vars.to.reverse), nl1, parent.frame())
    if (suppressWarnings(!is.null(which.neg))) {
        selected.matrix[, which.neg] <- -1 * selected.matrix[, 
            which.neg]
        reverse <- FALSE
        sign1 <- rep(1, ncol(selected.matrix))
        sign1[which.neg] <- -1
    }
    matR1 <- cor(selected.matrix, use = usage)
    diag(matR1) <- 0
    if(any(matR1 > .999)){
    reverse <- FALSE
    which(matR1 > .999, arr.ind =TRUE) -> temp.mat
    warning(paste(paste(rownames(temp.mat), collapse= " and "))," are extremely correlated.","\n", "  Remove one of them from 'vars' if 'reverse' is required.")
    }
    
    if (reverse) {
        score <- factanal(na.omit(selected.matrix), factor = 1, 
            score = "regression")$score
        sign1 <- NULL
        for (i in 1:length(selected)) {
            sign1 <- c(sign1, sign(cor(score, na.omit(selected.matrix)[, 
                i], use = usage)))             
        }
        which.neg <- which(sign1 < 0)
        selected.matrix[, which.neg] <- -1 * selected.matrix[, 
            which.neg]
    }
    reliability <- function(matrixC, matrixR, matrixN) {
        k1 <- ncol(matrixC)
        if (casewise) {
            cbar <- mean(matrixC[lower.tri(matrixC)])
            rbar <- mean(matrixR[lower.tri(matrixR)])
        }
        else {
            cbar.numerator <- sum(matrixC[lower.tri(matrixC)] * 
                matrixN[lower.tri(matrixN)])
            rbar.numerator <- sum(matrixR[lower.tri(matrixR)] * 
                matrixN[lower.tri(matrixN)])
            denominator <- sum(matrixN[lower.tri(matrixN)])
            cbar <- cbar.numerator/denominator
            rbar <- rbar.numerator/denominator
        }
        vbar <- sum(diag(matrixC) * diag(matrixN))/sum(diag(matrixN))
        alpha <- k1 * cbar/(vbar + (k1 - 1) * cbar)
        std.alpha <- k1 * rbar/(1 + (k1 - 1) * rbar)
        list(alpha = alpha, std.alpha = std.alpha, rbar = rbar)
    }
    k <- ncol(selected.matrix)
    matC <- cov(selected.matrix, use = usage)
    matR <- cor(selected.matrix, use = usage)
    if (casewise) {
        samp.size <- nrow(na.omit(selected.matrix))
        matN <- matrix(nrow(na.omit(selected.matrix)), k, k)
    }
    else {
        samp.size <- length(na.omit(rowSums((!is.na(selected.matrix)) * 
            1) > 1))
        matN <- matrix(0, k, k)
        for (i in 1:k) {
            for (j in 1:k) {
                matN[i, j] <- length(na.omit(selected.matrix[, 
                  i] + selected.matrix[, j]))
            }
        }
    }
    rel <- matrix(0, k, 3)
    colnames(rel) <- c("Alpha", "Std.Alpha", "r(item, rest)")
    rownames(rel) <- names(dataFrame)[selected]
    for (i in 1:k) {
        rel[i, 1] <- reliability(matrixC = matC[-i, -i], matrixR = matR[-i, 
            -i], matrixN = matN[-i, -i])$alpha
        rel[i, 2] <- reliability(matC[-i, -i], matR[-i, -i], 
            matN[-i, -i])$std.alpha
        if (usage == "pairwise.complete.obs") {
            meanrest <- rowMeans(selected.matrix[, -i], na.rm = TRUE)
        }
        if (usage == "complete.obs") {
            meanrest <- rowMeans(na.omit(selected.matrix)[, -i])
        }
        if (usage == "pairwise.complete.obs") {
            rel[i, 3] <- cor(selected.matrix[, i], meanrest, 
                use = "pairwise")
        }
        if (usage == "complete.obs") {
            rel[i, 3] <- cor(na.omit(selected.matrix)[, i], meanrest, 
                use = "complete.obs")
        }
    }
    if (!is.null(which.neg)) {
        Reversed <- ifelse(sign1 < 0, "    x   ", "    .   ")
        result <- cbind(Reversed, round(rel, digits = decimal))
    }else{
    result <- round(rel, digits=decimal)
    }
    rownames(result) <- names(dataFrame)[selected]
    if (var.labels) {
        if (!is.null(attributes(dataFrame)$var.labels)) {
            result <- cbind(result, substr(attributes(dataFrame)$var.labels[selected],1,var.labels.trunc))
                        colnames(result)[ncol(result)] <- "description"
        }
    }
results <- list(alpha = reliability(matC, matR, matN)$alpha, 
    std.alpha = reliability(matC, matR, matN)$std.alpha, 
    sample.size=samp.size,
    use.method = usage,
    rbar=reliability(matC, matR, matN)$rbar,
    items.selected = names(dataFrame)[selected], alpha.if.removed = rel,
    result=result, decimal=decimal)
if(!is.null(which.neg)) results <- c(results, list(items.reversed = names(selected.dataFrame)[sign1 < 0]))
if(var.labels && !is.null(attributes(dataFrame)$var.labels)){
    results <- c(results, list(item.labels=attributes(dataFrame)$var.labels[selected]))
}
class(results) <- "alpha"
results
}

print.alpha <- function(x, ...)
{
cat("Number of items in the scale =", length(x$items.selected), "\n")
cat("Sample size =", x$sample.size, "\n")
cat(paste("Average inter-item correlation =", round(x$rbar,
    digits = x$decimal), "\n", "\n"))
cat(paste("Cronbach's alpha: ", "cov/cor computed with ", 
     "'", x$use.method, "'", "\n", sep = ""))
cat(paste("      unstandardized value =", round(x$alpha, digits = x$decimal), "\n"))
cat(paste("        standardized value =", round(x$std.alpha, digits = x$decimal), "\n", "\n"))
if(!is.null(x$items.reversed)){
  cat(paste("Item(s) reversed:", paste(x$items.reversed, collapse= ", "), "\n", "\n"))
}else{
  cat(paste("Note: no attempt to reverse any item.", "\n", "\n"))
}
  cat(paste("New alpha if item omitted:", "\n"))
print.noquote(x$result)
}


# The best Cronbach alpha
alphaBest <- function (vars, standardized = FALSE, dataFrame = .data) 
{
    nl <- as.list(1:ncol(dataFrame))
    names(nl) <- names(dataFrame)
    selected <- eval(substitute(vars), nl, parent.frame())
    a <- alpha(vars = selected, dataFrame = dataFrame)
    sorted.alpha.if.removed <- a$alpha.if.removed[order(a$alpha.if.removed[, 
        1 + standardized], decreasing = TRUE), 1 + standardized]
    removed.names <- NULL
    removed.orders <- NULL
    while (a[1 + standardized] < sorted.alpha.if.removed[1]) 
    {
        removed.name0 <- names(sorted.alpha.if.removed)[1]
        removed.names <- c(removed.names, removed.name0)
        removed.orders <- c(removed.orders, which(names(dataFrame) %in% 
            removed.name0))
        a <- alpha(vars = setdiff(selected, removed.orders), 
            dataFrame = dataFrame)
        sorted.alpha.if.removed <- a$alpha.if.removed[order(a$alpha.if.removed[, 
            1 + standardized], decreasing = TRUE), 1 + standardized]
    }
    names(removed.orders) <- removed.names
    remaining.names <- a$items.selected
    remaining.orders <- which(names(dataFrame) %in% remaining.names)
    names(remaining.orders) <- remaining.names
    if (standardized) {
        list(best.std.alpha = a$alpha, removed.items = removed.orders, 
            remaining.items = remaining.orders)
    }
    else {
        list(best.alpha = a$alpha, removed = removed.orders, 
            remaining = remaining.orders, items.reversed = a$items.reversed)
    }
}


## Table stack
tableStack <-
function (vars, minlevel = "auto", maxlevel = "auto", count = TRUE, 
    means = TRUE, medians = FALSE, sds = TRUE, decimal = 1, dataFrame = .data, 
    total = TRUE, var.labels = TRUE, var.labels.trunc = 150, reverse = FALSE, 
    vars.to.reverse = NULL, by = NULL, vars.to.factor = NULL, 
    iqr = "auto", prevalence = FALSE, percent = c("column", "row", 
        "none"), frequency=TRUE, test = TRUE, name.test = TRUE, total.column = FALSE) 
{
    nl <- as.list(1:ncol(dataFrame))
    names(nl) <- names(dataFrame)
    selected <- eval(substitute(vars), nl, parent.frame())
    by.var <- eval(substitute(by), nl, parent.frame())
    if (is.numeric(by.var)) {
        by <- dataFrame[, by.var]
    }
    if (is.character(by.var)) {
        by1 <- as.factor(rep("Total", nrow(dataFrame)))
    }
    if (is.null(by)) {
        selected.class <- NULL
        for (i in selected) {
            selected.class <- c(selected.class, class(dataFrame[, 
                i]))
        }
        if (length(table(table(selected.class))) > 1) 
            warning("Without 'by', classes of all selected variables should be the same.")
    }
    selected.to.factor <- eval(substitute(vars.to.factor), nl, 
        parent.frame())
    if (!is.character(iqr)) {
        selected.iqr <- eval(substitute(iqr), nl, parent.frame())
        intersect.selected <- intersect(selected.iqr, selected.to.factor)
        if (length(intersect.selected) != 0) {
            stop(paste(names(dataFrame)[intersect.selected], 
                "cannot simultaneously describe IQR and be coerced factor"))
        }
        for (i in selected.iqr) {
            if (!is.integer(dataFrame[, i]) & !is.numeric(dataFrame[, 
                i])) {
                stop(paste(names(dataFrame)[i], "is neither integer nor numeric, not possible to compute IQR"))
            }
        }
    }
    for (i in selected) {
        if (class(dataFrame[, i]) == "integer" & !is.null(by)) {
            if (any(selected.to.factor == i)) {
                dataFrame[, i] <- factor(dataFrame[, i])
            }
            else {
                dataFrame[, i] <- as.numeric(dataFrame[, i])
            }
        }
    }
    if ((reverse || suppressWarnings(!is.null(vars.to.reverse))) && 
        is.factor(dataFrame[, selected][, 1])) {
        stop("Variables must be in 'integer' class before reversing. \n        Try 'unclassDataframe' first'")
    }
    selected.dataFrame <- dataFrame[, selected, drop = FALSE]
    if (is.null(by)) {
        selected.matrix <- NULL
        for (i in selected) {
            selected.matrix <- cbind(selected.matrix, unclass(dataFrame[, 
                i]))
        }
        colnames(selected.matrix) <- names(selected.dataFrame)
        if (minlevel == "auto") {
            minlevel <- min(selected.matrix, na.rm = TRUE)
        }
        if (maxlevel == "auto") {
            maxlevel <- max(selected.matrix, na.rm = TRUE)
        }
        nlevel <- as.list(minlevel:maxlevel)
        names(nlevel) <- eval(substitute(minlevel:maxlevel), 
            nlevel, parent.frame())
        if (suppressWarnings(!is.null(vars.to.reverse))) {
            nl1 <- as.list(1:ncol(dataFrame))
            names(nl1) <- names(dataFrame[, selected])
            which.neg <- eval(substitute(vars.to.reverse), nl1, 
                parent.frame())
            for (i in which.neg) {
                dataFrame[, selected][, i] <- maxlevel + 1 - 
                  dataFrame[, selected][, i]
                selected.matrix[, i] <- maxlevel + 1 - selected.matrix[, 
                  i]
            }
            reverse <- FALSE
            sign1 <- rep(1, ncol(selected.matrix))
            sign1[which.neg] <- -1
        }
        if (reverse) {
            matR1 <- cor(selected.matrix, use = "pairwise.complete.obs")
            diag(matR1) <- 0
            if (any(matR1 > 0.98)) {
                reverse <- FALSE
                temp.mat <- which(matR1 > 0.98, arr.ind = TRUE)
                warning(paste(paste(rownames(temp.mat), collapse = " and ")), 
                  " are extremely correlated.", "\n", "  The command has been excuted without 'reverse'.", 
                  "\n", "  Remove one of them from 'vars' if 'reverse' is required.")
            }
            else {
                score <- factanal(na.omit(selected.matrix), factor = 1, 
                  score = "regression")$score
                sign1 <- NULL
                for (i in 1:length(selected)) {
                  sign1 <- c(sign1, sign(cor(score, na.omit(selected.matrix)[, 
                    i], use = "pairwise")))
                }
                which.neg <- which(sign1 < 0)
                for (i in which.neg) {
                  dataFrame[, selected][, i] <- maxlevel + minlevel - 
                    dataFrame[, selected][, i]
                  selected.matrix[, i] <- maxlevel + minlevel - selected.matrix[, 
                    i]
                }
            }
        }
        table1 <- NULL
        for (i in as.integer(selected)) {
            if (!is.factor(dataFrame[, i])) {
                            x <- factor(dataFrame[, i])
                if(!is.logical(dataFrame[, i, drop = TRUE]))
                {
                  levels(x) <- nlevel
                }
                tablei <- table(x)
            }
            else {
                tablei <- table(dataFrame[, i])
            }
            if (count) {
                tablei <- c(tablei, length(na.omit(dataFrame[, 
                  i])))
                names(tablei)[length(tablei)] <- "count"
            }
            if (is.numeric(selected.dataFrame[, 1, drop = TRUE]) | is.logical(selected.dataFrame[, 1, drop = TRUE])) {
#                if (total) {
#                  means <- TRUE
#                }
                if (means) {
                  tablei <- c(tablei, round(mean(as.numeric(dataFrame[, 
                    i]), na.rm = TRUE), digits = decimal))
                  names(tablei)[length(tablei)] <- "mean"
                }
                if (medians) {
                  tablei <- c(tablei, round(median(as.numeric(dataFrame[, 
                    i]), na.rm = TRUE), digits = decimal))
                  names(tablei)[length(tablei)] <- "median"
                }
                if (sds) {
                  tablei <- c(tablei, round(sd(as.numeric(dataFrame[, 
                    i]), na.rm = TRUE), digits = decimal))
                  names(tablei)[length(tablei)] <- "sd"
                }
            }
            table1 <- rbind(table1, tablei)
        }
        results <- as.table(table1)
        if (var.labels) {
            rownames(results) <- names(selected.dataFrame)
        }
        else {
            rownames(results) <- paste(selected, ":", names(selected.dataFrame))
        }
        if (is.integer(selected.dataFrame[, 1])) {
            rownames(results) <- names(nl)[selected]
            if (is.factor(dataFrame[, selected][, 1])) {
                colnames(results)[1:(ncol(results) - (count + 
                  means + medians + sds))] <- levels(dataFrame[, 
                  selected][, 1])
            }
            else {
                colnames(results)[1:(ncol(results) - (count + 
                  means + medians + sds))] <- names(nlevel)
            }
        }
        result0 <- results
        if (var.labels) {
            if (!is.null(attributes(dataFrame)$var.labels)) {
                results <- as.table(cbind(results, substr(attributes(dataFrame)$var.labels[selected], 
                  1, var.labels.trunc)))
            }
            if (!is.null(attributes(dataFrame)$var.labels)) 
                colnames(results)[ncol(results)] <- "description"
        }
        if (is.integer(selected.dataFrame[, 1]) | is.numeric(selected.dataFrame[, 1]) | is.logical(selected.dataFrame[, 1])) {
            if (reverse || (!is.null(vars.to.reverse))) {
                Reversed <- ifelse(sign1 < 0, "    x   ", "    .   ")
                results <- cbind(Reversed, results)
            }
            sumMeans <- 0
            sumN <- 0
            for (i in selected) {
                sumMeans <- sumMeans + mean(as.numeric(dataFrame[, 
                  i]), na.rm = TRUE) * length(na.omit(dataFrame[, 
                  i]))
                sumN <- sumN + length(na.omit(dataFrame[, i]))
            }
            mean.of.total.scores <- weighted.mean(rowSums(selected.matrix), 
                w = rowSums(!is.na(selected.matrix)), na.rm = TRUE)
            sd.of.total.scores <- sd(rowSums(selected.matrix), 
                na.rm = TRUE)
            mean.of.average.scores <- weighted.mean(rowMeans(selected.matrix), 
                w = rowSums(!is.na(selected.matrix)), na.rm = TRUE)
            sd.of.average.scores <- sd(rowMeans(selected.matrix), 
                na.rm = TRUE)
            countCol <- which(colnames(results) == "count")
            meanCol <- which(colnames(results) == "mean")
            sdCol <- which(colnames(results) == "sd")
            if (total) {
                results <- rbind(results, rep("", reverse || 
                  suppressWarnings(!is.null(vars.to.reverse)) + 
                    (maxlevel + 1 - minlevel) + (count + means + 
                    medians + sds + var.labels)))
                results[nrow(results), countCol] <- length((rowSums(selected.dataFrame))[!is.na(rowSums(selected.dataFrame))])
                results[nrow(results), meanCol] <- round(mean.of.total.scores, 
                  digits = decimal)
                results[nrow(results), sdCol] <- round(sd.of.total.scores, 
                  digits = decimal)
                rownames(results)[nrow(results)] <- " Total score"
                results <- rbind(results, rep("", reverse || 
                  suppressWarnings(!is.null(vars.to.reverse)) + 
                    (maxlevel + 1 - minlevel) + (count + means + 
                    medians + sds + var.labels)))
                results[nrow(results), countCol] <- length(rowSums(selected.dataFrame)[!is.na(rowSums(selected.dataFrame))])
                results[nrow(results), meanCol] <- round(mean.of.average.scores, 
                  digits = decimal)
                results[nrow(results), sdCol] <- round(sd.of.average.scores, 
                  digits = decimal)
                rownames(results)[nrow(results)] <- " Average score"
            }
        }
        results <- list(results = noquote(results))
        if (reverse || suppressWarnings(!is.null(vars.to.reverse))) 
            results <- c(results, list(items.reversed = names(selected.dataFrame)[sign1 < 
                0]))
        if (var.labels && !is.null(attributes(dataFrame)$var.labels)) {
            results <- c(results, list(item.labels = attributes(dataFrame)$var.labels[selected]))
        }
        if (total) {
            if (is.integer(selected.dataFrame[, 1]) | is.numeric(selected.dataFrame[, 1])) {
                results <- c(results, list(total.score = rowSums(selected.matrix)), 
                  list(mean.score = rowMeans(selected.matrix)), 
                  list(mean.of.total.scores = mean.of.total.scores, 
                    sd.of.total.scores = sd.of.total.scores, 
                    mean.of.average.scores = mean.of.average.scores, 
                    sd.of.average.scores = sd.of.average.scores))
            }
        }
        class(results) <- c("tableStack", "list")
        results
    }
    else {
        if (is.character(by.var)) {
            by1 <- as.factor(rep("Total", nrow(dataFrame)))
        }
        else {
            by1 <- factor(dataFrame[, by.var])
        }
        if (is.logical(dataFrame[, i])) {
            dataFrame[, i] <- as.factor(dataFrame[, i])
            levels(dataFrame[, i]) <- c("No", "Yes")
        }
        if (length(table(by1)) == 1 & !is.logical(dataFrame[, by.var, drop = TRUE])) 
            test <- FALSE
        name.test <- ifelse(test, name.test, FALSE)
        if (is.character(iqr)) {
            if (iqr == "auto") {
                selected.iqr <- NULL
                for (i in 1:length(selected)) {
                  if (is.integer(dataFrame[, selected[i]]) | 
                    is.numeric(dataFrame[, selected[i]])) {
                    if (length(table(by1)) > 1) {
                      if (nrow(dataFrame) < 5000) {
                        if (shapiro.test(lm(dataFrame[, selected[i]] ~ 
                          by1)$residuals)$p.value < 0.01 | bartlett.test(dataFrame[, 
                          selected[i]] ~ by1)$p.value < 0.01) {
                          selected.iqr <- c(selected.iqr, selected[i])
                        }
                      }
                      else {
                        sampled.shapiro <- sample(lm(dataFrame[, 
                          selected[i]] ~ by1)$residuals, 250)
                        if (shapiro.test(sampled.shapiro)$p.value < 
                          0.01 | bartlett.test(dataFrame[, selected[i]] ~ 
                          by1)$p.value < 0.01) {
                          selected.iqr <- c(selected.iqr, selected[i])
                        }
                      }
                    }
                  }
                }
            }
            else {
                selected.iqr <- NULL
            }
        }
        table2 <- NULL
        for (i in 1:length(selected)) {
            if (is.factor(dataFrame[, selected[i]]) | is.logical(dataFrame[, selected[i]])) {
                x0 <- table(dataFrame[, selected[i]], by1)
                if (total.column) {
                  x <- addmargins(x0, margin = 2)
                }
                else {
                  x <- x0
                }
                nr <- nrow(x)
                nc <- ncol(x0)
                sr <- rowSums(x0)
                if (any(sr) == 0) {
                  stop(paste(names(dataFrame)[selected[i]], " has zero count in at least one row"))
                }
                sc <- colSums(x0)
                if (any(sc) == 0) {
                  stop(paste(names(dataFrame)[selected[i]], " has zero count in at least one column"))
                }
                x.row.percent <- round(x/rowSums(x0) * 100, decimal)
                table0 <- x
                if (nrow(x) == 2 & prevalence) {
                  table00 <- addmargins(x, margin = 1)
                  table0 <- paste(table00[2, ], "/", table00[3, 
                    ], " (", round(table00[2, ]/table00[3, ] * 
                    100, decimal), "%)", sep = "")
                  table0 <- t(table0)
                  rownames(table0) <- "  prevalence"
                }
                else {
                  if (any(percent == "column")) {
                    x.col.percent <- round(t(t(x)/colSums(x)) * 
                      100, decimal)
                    x.col.percent1 <- matrix(paste(x, " (", x.col.percent, 
                      ")", sep = ""), nrow(x), ncol(x))
                    if(!frequency){
                        x.col.percent1 <- x.col.percent 
                    }
                    table0 <- x.col.percent1
                  }
                  else {
                    if (any(percent == "row")) {
                      x.row.percent <- round(x/rowSums(x0) * 
                        100, decimal)
                      x.row.percent1 <- matrix(paste(x, " (", 
                        x.row.percent, ")", sep = ""), nrow(x), 
                        ncol(x))
                    if(!frequency){
                        x.row.percent1 <- x.row.percent 
                    }
                      table0 <- x.row.percent1
                    }
                  }
                  rownames(table0) <- paste("  ", rownames(x))
                  colnames(table0) <- colnames(x)
                }
                if (test) {
                  E <- outer(sr, sc, "*")/sum(x0)
                  dim(E) <- NULL
                  if ((sum(E < 5))/length(E) > 0.2) {
                    test.method <- "Fisher's exact test"
                    p.value <- fisher.test(x0, simulate.p.value = TRUE)$p.value
                  }
                  else {
                    test.method <- paste("Chisq. (", suppressWarnings(chisq.test(x0)$parameter), 
                      " df) = ", suppressWarnings(round(chisq.test(x0)$statistic, 
                        decimal + 1)), sep = "")
                    p.value <- suppressWarnings(chisq.test(x0)$p.value)
                  }
                }
            }
            if (is.numeric(dataFrame[, selected[i]])) {
                if (any(selected.iqr == selected[i])) {
                  term1 <- NULL
                  term2 <- NULL
                  term3 <- NULL
                  for (j in 1:(length(levels(by1)))) {
                    term1 <- c(term1, quantile(dataFrame[by1 == 
                      levels(by1)[j], selected[i]], na.rm = TRUE)[3])
                    term2 <- c(term2, quantile(dataFrame[by1 == 
                      levels(by1)[j], selected[i]], na.rm = TRUE)[2])
                    term3 <- c(term3, quantile(dataFrame[by1 == 
                      levels(by1)[j], selected[i]], na.rm = TRUE)[4])
                  }
                  if (total.column) {
                    term1 <- c(term1, quantile(dataFrame[, selected[i]], 
                      na.rm = TRUE)[3])
                    term2 <- c(term2, quantile(dataFrame[, selected[i]], 
                      na.rm = TRUE)[2])
                    term3 <- c(term3, quantile(dataFrame[, selected[i]], 
                      na.rm = TRUE)[4])
                  }
                  term.numeric <- paste(round(term1, decimal), 
                    " (", round(term2, decimal), ",", round(term3, 
                      decimal), ")", sep = "")
                  term.numeric <- t(term.numeric)
                  rownames(term.numeric) <- "  median(IQR)"
                }
                else {
                  term1 <- as.vector(tapply(X = dataFrame[, selected[i]], 
                    INDEX = list(by1), FUN = "mean", na.rm = TRUE))
                  if (total.column) {
                    term1 <- c(term1, mean(dataFrame[, selected[i]], 
                      na.rm = TRUE))
                  }
                  term2 <- as.vector(tapply(X = dataFrame[, selected[i]], 
                    INDEX = list(by1), FUN = "sd", na.rm = TRUE))
                  if (total.column) {
                    term2 <- c(term2, sd(dataFrame[, selected[i]], 
                      na.rm = TRUE))
                  }
                  term.numeric <- paste(round(term1, decimal), 
                    " (", round(term2, decimal), ")", sep = "")
                  term.numeric <- t(term.numeric)
                  rownames(term.numeric) <- "  mean(SD)"
                }
                table0 <- term.numeric
                if (test) {
                  if (any(selected.iqr == selected[i])) {
                    if (length(levels(by1)) > 2) {
                      test.method <- "Kruskal-Wallis test"
                      p.value <- kruskal.test(dataFrame[, selected[i]] ~ 
                        by1)$p.value
                    }
                    else {
                      test.method <- "Ranksum test"
                      p.value <- wilcox.test(dataFrame[, selected[i]] ~ 
                        by1, exact = FALSE)$p.value
                    }
                  }
                  else {
                    if (length(levels(by1)) > 2) {
                      test.method <- paste("ANOVA F-test (", 
                        anova(lm(dataFrame[, selected[i]] ~ by1))[1, 
                          1], ", ", anova(lm(dataFrame[, selected[i]] ~ 
                          by1))[2, 1], " df) = ", round(anova(lm(dataFrame[, 
                          selected[i]] ~ by1))[1, 4], decimal + 
                          1), sep = "")
                      p.value <- anova(lm(dataFrame[, selected[i]] ~ 
                        by1))[1, 5]
                    }
                    else {
                      test.method <- paste("t-test", paste(" (", 
                        t.test(dataFrame[, selected[i]] ~ by1, 
                          var.equal = TRUE)$parameter, " df)", 
                        sep = ""), "=", round(abs(t.test(dataFrame[, 
                        selected[i]] ~ by1, var.equal = TRUE)$statistic), 
                        decimal + 1))
                      p.value <- t.test(dataFrame[, selected[i]] ~ 
                        by1, var.equal = TRUE)$p.value
                    }
                  }
                }
            }
            if (test) {
                if (name.test) {
                  label.row <- c(rep("", length(levels(by1)) + 
                    total.column), test.method, ifelse(p.value < 
                    0.001, "< 0.001", round(p.value, decimal + 
                    2)))
                  label.row <- t(label.row)
                  if (total.column) {
                    colnames(label.row) <- c(levels(by1), "Total", 
                      "Test stat.", "  P value")
                  }
                  else {
                    colnames(label.row) <- c(levels(by1), "Test stat.", 
                      "  P value")
                  }
                  table0 <- cbind(table0, "", "")
                  blank.row <- rep("", length(levels(by1)) + 
                    total.column + 2)
                }
                else {
                  label.row <- c(rep("", length(levels(by1)) + 
                    total.column), ifelse(p.value < 0.001, "< 0.001", 
                    round(p.value, decimal + 2)))
                  label.row <- t(label.row)
                  if (total.column) {
                    colnames(label.row) <- c(levels(by1), "Total", 
                      "  P value")
                  }
                  else {
                    colnames(label.row) <- c(levels(by1), "  P value")
                  }
                  table0 <- cbind(table0, "")
                  blank.row <- rep("", length(levels(by1)) + 
                    total.column + 1)
                }
            }
            else {
                label.row <- c(rep("", length(levels(by1)) + 
                  total.column))
                label.row <- t(label.row)
                if (total.column) {
                  colnames(label.row) <- c(levels(by1), "Total")
                }
                else {
                  colnames(label.row) <- c(levels(by1))
                }
                blank.row <- rep("", length(levels(by1)) + total.column)
            }
            if (var.labels) {
                rownames(label.row) <- ifelse(!is.null(attributes(dataFrame)$var.labels[selected][i]), 
                  attributes(dataFrame)$var.labels[selected[i]], 
                  names(dataFrame)[selected][i])
                rownames(label.row) <- ifelse(rownames(label.row) == 
                  "", names(dataFrame[selected[i]]), rownames(label.row))
            }
            else {
                rownames(label.row) <- paste(selected[i], ":", 
                  names(dataFrame[selected[i]]))
            }
            if (!is.logical(dataFrame[, selected[i]])) {
                if (prevalence & length(levels(dataFrame[, selected[i]])) == 
                  2) {
                  rownames(label.row) <- paste(rownames(label.row), 
                    "=", levels(dataFrame[, selected[i]])[2])
                }
            }
            blank.row <- t(blank.row)
            rownames(blank.row) <- ""
            table2 <- rbind(table2, label.row, table0, blank.row)
        }
        class(table2) <- c("tableStack", "table")
        table2
    }
}



# Print tableStack 

print.tableStack <- 
function (x, ...)
{
if(any(class(x)=="list")){
print(x$results)
}else{
print.table(noquote((x)))
}
}

# Unclass data frame
unclassDataframe <- function(vars, dataFrame = .data){
data1 <- dataFrame
nl <- as.list(1:ncol(data1))
names(nl) <- names(data1)
selected <- eval(substitute(vars), nl, parent.frame())
for(i in selected){
data1[,i] <- unclass(data1[,i])
}
    assign(as.character(substitute(dataFrame)), data1, pos=1)
    if(is.element(as.character(substitute(dataFrame)), search())){
      detach(pos=which(search() %in% as.character(substitute(dataFrame))))
      attach(data1, name=as.character(substitute(dataFrame)), warn.conflicts = FALSE)
    }
}
