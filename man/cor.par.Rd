\name{cor.par}
\alias{cor.par}
\title{Partial correlation}
\description{This function calculates the partial correlation matrix between a set of traits and a third control variable.}
\usage{cor.par(traits, c.trait, trait.names = FALSE)}
\arguments{
  \item{traits}{a dataframe or a matrix object with all your traits in which columns are the traits and rows are the individuals}
  \item{c.trait}{a vector containing the third control variable to estimate partial correlations, for instance: the size of the organ or the individual}
  \item{trait.names}{a logical (default=FALSE): If 'FALSE' (default), trait names in the output are defined as "Trait 1", "Trait 2", ..., "Trait n". If 'TRUE', output trait names are the same than those in the input dataframe}
}
\details{This function is utilised internally by \code{\link{intsc}} and \code{\link{intsc.boot}} to estimate the phenotypic integration index from partial correlations. Rows containing missing values are removed.}
\value{A matrix with the pairwise partial correlation between traits.}
\author{R. Torices, A. J. Muñoz-Pajares}
\seealso{\code{\link{pcor}}, \code{\link{pcor.test}}}
\examples{
# Dataset from Torices & Méndez (2014)
# This data set represents the dry mass (in grames) of inflorescence components of the sunflower
# species Tussilago farfara. The inflorescences were dissected in 'SCAPE', 'RECEPTACLE', 'MALEFL'
# (male flowers), 'OVAR' (reproductive part of female flowers), and 'RAYS' (the petaloid ray of
# female flower). Furthermore in the last column the total weight of the inflorescence is added
# 'TOTALSIZE'

data(tussilago)
print(tussilago)
cor.par(tussilago[,1:5], tussilago[,6], trait.names=TRUE) # NA automatically removed

traits<-na.exclude(tussilago) # NA manually removed
cor.par(traits[,1:5], traits[,6], trait.names=TRUE)
}