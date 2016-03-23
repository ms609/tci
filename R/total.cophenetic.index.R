list.ancestors <- function (parent, child, node) {
    if (length(node) == 1) {
        pvector <- numeric(max(parent))
        pvector[child] <- parent
        anc <- function(pvector, node) {
            res <- numeric(0)
            repeat {
                anc <- pvector[node]
                if (anc == 0) 
                  break
                res <- c(res, anc)
                node <- anc
            }
            res
        }
        return(anc(pvector, node))
    }
    else all.ancestors(parent, child)[node]
}

depths <- function (parent, child) {
  root   <- min(parent)
  depth  <- integer(max(parent))
  for (i in 1:length(parent)) {
    depth[child[i]] <- depth[parent[i]] + 1L
  }
  as.integer(depth)
}

total.cophenetic.index <- function (tree) {
  edge <- tree$edge
  parent <- edge[, 1]
  child  <- edge[, 2]
  nTip   <- length(tree$tip.label)
  depth  <- depths(parent, child)
  ancestors <- lapply(1:nTip, function(node) list.ancestors (parent, child, node))
  lca.depth <- vapply(1:nTip, function(i) {
    vapply(1:nTip, function (j) {
      anc.i <- ancestors[[i]]
      anc.j <- ancestors[[j]]
      lca <- max(anc.i[anc.i %in% anc.j])
      depth[lca]
    }, integer(1))
  }, integer(nTip))
  return (sum(lca.depth[upper.tri(lca.depth)]))
}

max.cophenetic.index <- function (tree) {
  nTip <- length(tree$tip.label)
  choose(nTip, 3)
}