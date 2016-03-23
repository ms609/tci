\name{list.ancestors}
\alias{list.ancestors}

\title{List ancestors}
\description{Reports the ancestors of a given node}
\usage{
list.ancestors(parent, child, node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parent}{the 'parent' column of the edges property of a tree of class \code{phylo};}
  \item{child}{the 'child' column of the edges property of a tree of class \code{phylo};}
  \item{node}{the number of the node or tip whose ancestors are required.}
}
\details{
To observe the number of a node or tip, use 
\code {plot(tree); \link{nodelabels}(); \link{tiplabels}();}
}
\value{Returns a vector of the numbers of the nodes ancestral to the given \code{node}, including the root node.}
\references{
}
\author{
Martin R. Smith
}

\seealso{
\code{\link{phangorn:::Ancestors}}, a less efficient implementation on which this code is based.
}
\examples{
  tree <- read.tree(text='(1, (2, (3, (4, 5))));')  
  edge <- tree$edge
  parent <- tree$edge[, 1]
  child  <- tree$edge[, 2]
  list.ancestors(parent, child, 4)
}