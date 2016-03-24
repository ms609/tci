\name{tci}
\alias{tci}

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
  The Total Cophenetic Index is a measure of tree balance -
  i.e. whether a (phylogenetic) tree comprises symmetric pairs of nodes, or has a pectinate 
  'caterpillar' shape.   The index has a greater resolution power than Sackin's and Colless' indices,
  and can be applied to trees that are not perfectly resolved.
  Full details are provided by Mir et al (2013).
}
\value{
  Returns the Total Cophenetic Index, a measure of the balance of the tree.
  This ranges from 0 to \code{\link{max.cophenetic.index}(tree)}
}
\references{
A. Mir, F. Rossello, L. A. Rotger, \cite{A new balance index for phylogenetic trees.} Math. Biosci. 241, 125-136 (2013).
}
\author{
Martin R. Smith
}

\seealso{
  \code{\link{max.cophenetic.index}}, the maximum value that the Total Cophenetic Index can obtain for a tree with the given number of tips.
}

\examples{
  tree12 <- read.tree(text='(1, (2, (3, (4, 5))));')  #Fig. 4, tree 12
  total.cophenetic.index(tree12) # 10
  tree8  <- read.tree(text='((1, 2, 3, 4), 5);')      #Fig. 4, tree 8
  total.cophenetic.index(tree8)  # 6
}
