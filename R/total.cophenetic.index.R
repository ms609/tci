list.ancestors <- function (parent, child, node) {
  pvector <- integer(max(parent))
  pvector[child] <- parent
  anc <- function(pvector, node) {
    res <- integer(0)
    repeat {
      anc <- pvector[node]
      if (anc == 0)
        break
      res <- c(res, anc)
      node <- anc
    }
    res
  }

  # Return:
  anc(pvector, node)
}

depths <- function (parent, child) {
  root   <- min(parent)
  depth  <- integer(max(parent))
  for (i in seq_along(parent)) {
    depth[child[i]] <- depth[parent[i]] + 1L
  }

  # Return:
  as.integer(depth)
}

tci <- function (tree) {
  edge <- tree$edge
  parent <- edge[, 1]
  child  <- edge[, 2]
  nTip   <- length(tree$tip.label)
  depth  <- depths(parent, child)

  ancestors <- lapply(seq_len(nTip),
                      function(node) list.ancestors (parent, child, node))

  lca.depth <- vapply(seq_len(nTip), function(i) {
    vapply(seq_len(nTip), function (j) {
      anc.i <- ancestors[[i]]
      anc.j <- ancestors[[j]]
      lca <- max(anc.i[anc.i %in% anc.j])

      # Return:
      depth[lca]
    }, integer(1L))
  }, integer(nTip))

  # Return:
  sum(lca.depth[upper.tri(lca.depth)])
}

tci.context <- function (tree) {
  n <- length(tree$tip.label)
  maximum <- choose(n, 3L)
  minimum <- mci(n)

  # Theorem 17
  uniform.expected <- (1 / 2) * choose(n, 2) *
    ((dfact((2 * n) - 2) / dfact((2 * n) - 3)) - 2)
  yule.expected    <- (n * (n + 1)) - (2 * n * H(n))
  yule.variance    <- ((1 / 12) * (n^4 - (10 * n^3) + (131 * n^2) - (2 * n))) -
    (4 * n^2 * H2(n)) - (6 * n * H(n))

  # Return:
  data.frame(maximum,   minimum,   uniform.expected,  yule.expected,  yule.variance)
}

H <- function (n) sum(1 / (seq_len(n)))
H2 <- function (n) sum(1 / (seq_len(n)^2))

dfact <- function (n) {
  if (n < 2) {
    1L
  } else {
    n * dfact(n - 2L)
  }
}

mci <- function (n) { # Lemma 14 in Mir er al 2013
  if (n < 3) return (0)
  halfN <- n / 2
  ceiling(mci(halfN)) + floor (mci(halfN)) +
    choose(ceiling(halfN), 2L) + choose(floor(halfN), 2L)
}

