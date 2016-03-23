\name{cophenetic.index.context}
\alias{cophenetic.index.context}

\title{Cophenetic Index Range}
\description{Calculate the range of values that the Total Cophenetic Index can take, and expected values under the Yule and Uniform models of evolution.}
\usage{
cophenetic.index.context(tree)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{a tree of class \code{\link{phylo}}}
}
\details{
  For a tree with n tips, the Total Cophenetic Index can take values of 0 to \code{\link{choose}(nTips, 3)}
  The minimum value is higher in a perfectly resolved (i.e. dichotomous) tree (see Lemma 14)
  Formulæ to calculate the expected values under the Yule and Uniform models of evolution are given in Theorems 17 and 23.
}
\value{
  A data frame detailing the maximum and minimum value obtainable for the Total Cophenetic Index for dichotomous 
  trees with the number of tips of the given tree, and the expected value under the Yule and Unifrom models.
  The variance of the expected value is given under the Yule model, but cannot be obtained explicitly for the Uniform model.
}
\references{
A. Mir, F. Rosselló, L. A. Rotger, \cite{A new balance index for phylogenetic trees.} Math. Biosci. 241, 125-136 (2013).
}
\author{
Martin R. Smith
}

\seealso{
  \code{\link{max.cophenetic.index}}, the maximum value for a given tree
}
\examples{
  tree12 <- read.tree(text='(1, (2, (3, (4, 5))));')  #Fig. 4, tree 12
  cophenetic.index.context(tree12)
}