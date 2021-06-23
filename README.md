[![CRAN Status Badge](https://www.r-pkg.org/badges/version/TotalCopheneticIndex)](https://cran.r-project.org/package=TotalCopheneticIndex)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/TotalCopheneticIndex)](https://cran.r-project.org/package=TotalCopheneticIndex)
[![DOI](https://zenodo.org/badge/54549731.svg)](https://zenodo.org/badge/latestdoi/54549731)
[![Project Status: Inactive - The project has reached a stable, usable state but is no longer being actively developed; support/maintenance will be provided as time allows.](https://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive)

# TotalCopheneticIndex
R package implementing the Total Cophenetic Index measure of Mir _et al._ (2013).
This package is now simply a wrapper for the function `TotalCopheneticIndex()`
in the package '[TreeTools](https://ms609.github.io/TreeTools/)'.

The total cophenetic index describes how balanced a phylogenetic tree is,
by measuring the how deep the most recent common ancestor of each pair of leaves
lies within the tree.

# See also

This functionality is now available via the function [`TotalCopheneticIndex()`](https://ms609.github.io/TreeTools/reference/TotalCopheneticIndex.html) in the
'[TreeTools](https://ms609.github.io/TreeTools/)' package, and via Rotger's
(unmaintained?) package 
'[CollessLike](https://github.com/LuciaRotger/CollessLike)'.

# References

A. Mir, F. Rossello, L. A. Rotger, A new balance index for phylogenetic trees.
  _Math. Biosci._ 241, 125-136 (2013).
