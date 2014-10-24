int <-
function(traits){
 
  X<-traits
  nas<-length(which(is.na(traits)))
  if(nas>0)
	{
  warning(paste("Rows containing missing data (",nas, if(nas==1) " row", if(nas>1) " rows",") has been removed to perform the analysis",sep=""))
  X<-na.exclude(traits)
	}

  cor_X <-cor(X)
  eig_X<-eigen(cor_X, only.values=TRUE)$values
  d <- eig_X
  p <- length (d)
  n <- nrow(X)
  INT<-sum((d-1)^2)/(p-1)
  INT.c<-(INT-((p-1)/n))
  pref="INT = "
  pref2="RelINT = "
  pref3="INT.c = "
  pref4="RelINT.c = "
  perc.c<-(INT.c/(p-1))*100
  pref5="N = "

names<-matrix(c(pref,pref2,pref3,pref4,pref5))
outs<-matrix(c(round(INT, 3),round((INT/(p-1))*100, 3),round(INT.c, 3),round(perc.c, 3),n))
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

