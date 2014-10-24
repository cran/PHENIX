int.boot <-
function (traits,replicates=1000){

  X<-traits
  nas<-length(which(is.na(traits)))
  if(nas>0)
	{
  warning(paste("Rows containing missing data (",nas, if(nas==1) " row", if(nas>1) " rows",") has been removed to perform the analysis",sep=""))
  X<-na.exclude(traits)
	}

  Y<-replicates

  INT = list()
  length (INT) = Y
  for (i in 1:Y){
    cor_X<-cor(X[sample(nrow(X), replace=TRUE),])
    d<-eigen(cor_X, only.values=TRUE)$values
    p <- length (d)
    n <- nrow(X)
    Int<-sum((d-1)^2)/(p-1)
    Int.c<-(Int-((p-1)/n))
    INT[i]=Int.c

	if(i==1) cat("\nStarting bootstrap...........\n")
	if(i==round(Y/4)) cat("\nPerforming bootstrap......25%\n")
	if(i==round(Y/2)) cat("\nPerforming bootstrap......50%\n")
	if(i==round(3*Y/4)) cat("\nPerforming bootstrap......75%\n")
	if(i==Y) cat("\nBootstrap finished.......100%\n")

  }
  Intphen <-as.numeric(INT)
  pref0="Mean = "
#  print(paste(pref, round(mean(Intphen), 3)))
  pref1="Median ="
#  print(paste(pref, round(median(Intphen), 3)))
  pref2="SD = "
#  print(paste(pref2, round(sd(Intphen), 3)))
  pref3="SE = "
  se<-(sd(Intphen)/sqrt(nrow(X)))
#  print (paste(pref3, round(se, 3)))
  pref4="Lower IC 99% = "
#  print(paste(pref4,round(quantile(Intphen, probs=0.5/100), 3)))
  pref5="Higher IC 99% = "
#  print(paste(pref5,round(quantile(Intphen, probs=99.5/100), 3)))
  pref6="Lower IC 95% = "
#  print(paste(pref6,round(quantile(Intphen, probs=2.5/100), 3)))
  pref7="Higher IC 95% = "
#  print(paste(pref7,round(quantile(Intphen, probs=97.5/100), 3)))
  pref8="Number of replicates = "
#  print (paste(pref8, length(INT)))

#Igual que antes:
names<-matrix(c(pref0,pref1,pref2,pref3,pref4,pref5,pref6,pref7,pref8))
outs<-matrix(c(
round(mean(Intphen), 3),
round(median(Intphen), 3),
round(sd(Intphen), 3),
round(se, 3),
round(quantile(Intphen, probs=0.5/100), 3),
round(quantile(Intphen, probs=99.5/100), 3),
round(quantile(Intphen, probs=2.5/100), 3),
round(quantile(Intphen, probs=97.5/100), 3),
length(INT)))
row.names(outs)<-names
colnames(outs)<-""
outs
}
