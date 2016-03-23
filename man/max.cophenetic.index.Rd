\name{total.cophenetic.index}
\alias{total.cophenetic.index}

\title{Total Cophenetic Index}
\description{Calculate the Total Cophenetic Index for any tree.}
\usage{
total.cophenetic.index(tree)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{a tree of class \code{\link{phylo}}}
}
\details{
  For a tree with n tips, the Total Cophenetic Index can take values of 0 to \code{\link{choose}(nTips, 3)}
}
\value{Returns the maximum value obtainable for the Total Cophenetic Index for trees with the number of tips of the given tree.}
\references{
A. Mir, F. Rosselló, L. A. Rotger, \cite{A new balance index for phylogenetic trees.} Math. Biosci. 241, 125–136 (2013).
}
\author{
Martin R. Smith
}

\seealso{
  \code{\link{max.cophenetic.index}}, the maximum value for a given tree
}
\examples{
  tree12 <- read.tree(text='(1, (2, (3, (4, 5))));')  #Fig. 4, tree 12
  total.cophenetic.index(tree12) # 10
  tree8  <- read.tree(text='((1, 2, 3, 4), 5);')      #Fig. 4, tree 8
  total.cophenetic.index(tree8)  # 6
}