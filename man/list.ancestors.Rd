\name{list.ancestors}
\alias{list.ancestors}

\title{List ancestors}
\description{Reports the ancestors of a given node}
\usage{
ancestors(tree, node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{a tree of class \code{phylo};}
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
tree <- rtree(20, br=NULL)
tbr(tree)
}