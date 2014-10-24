\name{intsc}
\alias{intsc}
\title{size-controlled phenotypic integration}
\description{This function estimates the phenotypic integration indices controlling by a third variable}
\usage{intsc(traits,control=NA)}
\arguments{
  \item{traits}{a dataframe or a matrix object with all your traits in which columns are the traits and rows are the individuals}
  \item{control}{a vector containing the variable to control by size.}
}
\details{This function allows to estimate the same set of indices than int but from partial correlation
matrix instead of the correlation matrix: INTsc: The variance of the eigenvalues using the partial correlation
matrix instead of a correlation matrix. Torices & Méndez (2014) proposed this index for analysing phenotypic
integration of resource allocation to different components in an individual or a given organ when resource
allocation data come from observational studies in which resource availability is not controlled and therefore
could lead to components correlations only by the fact of different resource availability. Thus, INT can be
estimated using the matrix of partial correlations in which size of the organ or individual is used as the third
control variable. INTsc.c: The INTpc corrected by the number of traits and individuals of each population
RelINTsc: The same as RelINT but estimated with the partial correlation matrix.

Rows containing missing values are removed.}
\value{ A list with five elements containing:
\item{INTsc:}{The phenotypic integration index.}
\item{RelINTsc:}{Percentage of maximum possible integration.}
\item{INTsc.c:}{Corrected phenotypic integration index.}
\item{RelINTsc.c:}{Percentage of maximum possible integration.}
\item{N:}{Number of observations used.}
}
\references{Torices, R. and Méndez, M. (2014) Resource allocation to inflorescence components is highly integrated despite differences between allocation currencies and aites. Int. J. Plant Sci. 175(6):713–723.}
\author{R. Torices, A. J. Muñoz-Pajares}

\seealso{\code{\link{int}}, \code{\link{intsc.boot}}}

\examples{
# Dataset from Torices & Méndez (2014)
# This data set represents the dry mass (in grames) of inflorescence components of the sunflower
# species Tussilago farfara. The inflorescences were dissected in 'SCAPE', 'RECEPTACLE', 'MALEFL'
# (male flowers), 'OVAR' (reproductive part of female flowers), and 'RAYS' (the petaloid ray of
# female flower). Furthermore in the last column the total weight of the inflorescence is added
#'TOTALSIZE'

data(tussilago)

# To estimate the INT indices controlling by a third variable
# it is mandatory to define the "control" variable. Otherwise, it gives an error:
#
# intsc(traits=tussilago)
intsc(traits=tussilago[,1:5],control=tussilago[,6])
}
