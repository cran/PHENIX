cor.par <-
function(traits, c.trait, trait.names=FALSE) {

  X<-traits
  nas<-length(which(is.na(traits)))
  if(nas>0)
	{
  warning(paste("Rows containing missing data (",nas, if(nas==1) " row", if(nas>1) " rows",") has been removed to perform the analysis",sep=""))
  X<-na.exclude(traits)
	}

  ntraits<-ncol(traits)
  z<-c.trait
  r <- matrix(0, nrow = ntraits, ncol = ntraits)
  for (i in seq_len(ntraits)) {
    for (j in seq_len(i)) {
      if (i==j) r[i, j] <- 1
      else {
        x2 <- traits[, i]
        y2 <- traits[, j]
        z2 <- z
        r[i, j] <-pcor.test(x2, y2, z2, method="pearson")$estimate
      }}}
  r <- r + t(r) - diag(diag(r))
  if (trait.names==FALSE) #return(r) # ¿Qué te parece si al ser FALSE pone "trait1, trait2...traitN"?
#para eso:
	{
	rownames(r) <- paste("Trait",c(1:ncol(traits)))
	colnames(r) <- paste("Trait",c(1:ncol(traits)))
	r
	}
  else{
    rownames(r) <- colnames(traits)
    colnames(r) <- colnames(traits)
    r
  }}
