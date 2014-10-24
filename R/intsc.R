intsc <-
function(traits,control=NA){

  if(is.na(control[1]))  stop("Undefined control trait!")
  X<-cbind(traits,control)
  nas<-length(which(is.na(X)))
  if(nas>0)
	{
  warning(paste("Rows containing missing data (",nas, if(nas==1) " row", if(nas>1) " rows",") has been removed to perform the analysis",sep=""))
  X<-na.exclude(X)
	}

  N<-ncol(X)

  traits<-X[,(1:ncol(X))[-N]] # redefino los traits
  c.trait<-X[,N]
  cor_X<-cor.par(traits, c.trait)
  eig_X<-eigen(cor_X, only.values=TRUE)$values
  d <- eig_X
  p <- length (d)
  n <- nrow(X)
  INT<-sum((d-1)^2)/(p-1)
  INT.c<-(INT-((p-1)/n))
  pref="INTsc = "
  pref2="RelINTsc = "
  perc<-(INT/(p-1))*100
  pref3="INTsc.c = "
  pref4="RelINTsc.c = "
  perc.c<-(INT.c/(p-1))*100
  pref5="N = "

names<-matrix(c(pref,pref2,pref3,pref4,pref5))
outs<-matrix(c(
round(INT, 3),
round(perc, 3),
round(INT.c, 3),
round(perc.c, 3),
n
))
row.names(outs)<-names 
colnames(outs)<-""
print(outs)

OUT<-as.list(outs)
namesOUT<-c()
for(i in 1:nrow(outs))
namesOUT<-c(namesOUT,strsplit(row.names(outs),split=" =")[[i]][1])
names(OUT)<-namesOUT
OUT<-OUT
}
