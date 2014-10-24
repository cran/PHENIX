\name{int}
\alias{int}
\title{Phenotypic integration index by Wagner}

\description{Estimates phenotypic integration indices based on the variance of the eigenvalues of the correlatoin matrix between phenotypic traits following Wagner (1984)}
\usage{int(traits)}
\arguments{
  \item{traits}{a dataframe or a matrix object with all your traits in which columns are the traits and rows are the individuals}
	}

\details{This function allows to estimate the phenotypic integration index (INT) using the correlation
matrix following Wagner (1984), as well as a phenotypic integration index (INT.c) corrected by the number
of traits and individuals of each population. Both estimates are also expressed as percentage depending on
the maximum possible integration levels.}

\value{ A list with five elements containing:
\item{INT:}{The phenotypic integration index.}
\item{RelINT:}{Percentage of maximum possible integration.}
\item{INT.c:}{Corrected phenotypic integration index.}
\item{RelINT.c:}{Percentage of maximum possible integration.}
\item{N:}{Number of observations used.}
}
\references{
Cheverud JM, GP Wagner, MM Dow 1989 Methods for the comparative analysis of variation patterns. Syst Zool 38:201–213

Pavlicev M, JM Cheverud, GPWagner 2009 Measuring morphological integration using eigenvalue variance. Evolutionary Biology 36: 157–170

Wagner GP 1984 On the eigenvalue distribution of genetic and phenotypic dispersion matrices: evidence for a nonrandom organization of quantitative character variation. J Math Biol 21:77–95.
}
\author{R. Torices, A. J. Muñoz-Pajares}

\seealso{\code{\link{intsc}}, \code{\link{int.boot}}}

\examples{
# Dataset from Torices & Méndez (2014)
# This data set represents the dry mass (in grames) of inflorescence components of the sunflower
# species Tussilago farfara. The inflorescences were dissected in 'SCAPE', 'RECEPTACLE', 'MALEFL'
# (male flowers), 'OVAR' (reproductive part of female flowers), and 'RAYS' (the petaloid ray of
# female flower). Furthermore in the last column the total weight of the inflorescence is added
# 'TOTALSIZE'

data(tussilago)
int (tussilago[,1:5]) # the last column is not included since represents the total size
# NOTE that the number of observations used by the function was 29 instead 40 that were
# included in the 'tussilago' data set. Missing values were removed. 
}
