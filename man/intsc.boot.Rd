\name{intsc.boot}
\alias{intsc.boot}
\title{size-controlled phenotypic integration significance test}
\description{This function estimates the confidence intervals of the phenotypic integration indices controlling by a third variable using bootstrap. After a number of replicates defined by the user, this function evaluates if the obtained indices are significantly different from 0.}
\usage{
intsc.boot(traits, replicates = 1000, control=NA)
}

\arguments{
  \item{traits}{a dataframe or a matrix object with all your traits in which columns are the traits and rows are the individuals}
  \item{replicates}{a numeric, the number of randomizations performed to evaluate significance (1000 by default)}
  \item{control}{a vector containing the variable to control by size.}
}
\details{Rows containing missing values are removed.}

\value{A nine rows matrix containing Mean, Median, standard deviation (SD), standard error (SE), lower and higer confidence interval at 99\%, lower and higer confidence interval at 95\%, and the number of randomizations.}

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

# To estimate the significance of INT indices controlling by a third variable
# it is mandatory to define the "control" variable. Otherwise, it gives an error:
# int.boot(tussilago, 5000)
intsc.boot(traits=tussilago[,1:5], replicates=100, control=tussilago[,6])
# The phenotypic integration index is significant because
# intervals do not include 0.
# For faster execution, we used 100 replicates in this example.
# For real data set increase this number:
# intsc.boot(traits=tussilago[,1:5], replicates=5000, control=tussilago[,6])
}
