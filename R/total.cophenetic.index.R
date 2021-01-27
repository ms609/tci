list.ancestors <- function (parent, child, node)
  ListAncestors(parent, child, node)

tci <- function (tree) TotalCopheneticIndex(tree)

tci.context.n <- function(n) TCIContext(n)

tci.context <- function (tree) TCIContext(tree)
