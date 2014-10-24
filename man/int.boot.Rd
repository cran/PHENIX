\name{int.boot}
\alias{int.boot}
\title{Phenotypic integration (by Wagner) significance test}
\description{This function estimates the confidence intervals of the phenotypic integration indices (Wagner, 1984) using bootstrap. After a number of replicates defined by the user, this function evaluates if the obtained indices are significantly different from 0.}
\usage{int.boot(traits, replicates = 1000)}
\arguments{
  \item{traits}{a dataframe or a matrix object with all your traits in which columns are the traits and rows are the individuals}
  \item{replicates}{a numeric, the number of randomizations performed to evaluate significance (1000 by default)}
}
\details{Rows containing missing values are removed.}
\value{A nine rows matrix containing Mean, Median, standard deviation (SD), standard error (SE), lower and higer confidence interval at 99\%, lower and higer confidence interval at 95\%, and the number of randomizations.}

\references{
Cheverud JM, GP Wagner, MM Dow 1989 Methods for the comparative analysis of variation patterns. Syst Zool 38:201–213

Pavlicev M, JM Cheverud, GPWagner 2009 Measuring morphological integration using eigenvalue variance. Evolutionary Biology 36: 157–170
}

\author{R. Torices, A. J. Muñoz-Pajares}

\seealso{\code{\link{int}}, \code{\link{intsc.boot}}}

\examples{
# Dataset from Torices & Méndez (2014)
# This data set represents the dry mass (in grames) of inflorescence components of the sunflower
# species Tussilago farfara. The inflorescences were dissected in 'SCAPE', 'RECEPTACLE', 'MALEFL'
# (male flowers), 'OVAR' (reproductive part of female flowers), and 'RAYS' (the petaloid ray of
# female flower). Furthermore in the last column the total weight of the inflorescence is added
# 'TOTALSIZE'

data(tussilago)
int.boot(tussilago[,1:5], 100) # The phenotypic integration index is significant because
# intervals do not include 0.
# For faster execution, we used 100 replicates in this example.
# For real data set increase this number:
# int.boot(tussilago[,1:5], 5000)
}
