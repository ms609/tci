context("Total cophenetic index tests")
library("ape")

test_that("Mir et al. (2013) trees scored correctly", {
  expect_equal(0L, tci(read.tree(text = '(1,2,3,4,5);')))
  expect_equal(1L, tci(read.tree(text = '((1,2),3,4,5);')))
  expect_equal(2L, tci(read.tree(text = '((1,2),(3,4),5);')))
  expect_equal(3L, tci(read.tree(text = '((1,2,3),4,5);')))
  expect_equal(4L, tci(read.tree(text = '(((1,2),3),4,5);')))
  expect_equal(4L, tci(read.tree(text = '((1,2,3),(4,5));')))
  expect_equal(5L, tci(read.tree(text = '(((1,2),3),(4,5));')))
  expect_equal(6L, tci(read.tree(text = '((1,2,3,4),5);')))
  expect_equal(7L, tci(read.tree(text = '(((1,2),3,4),5);')))
  expect_equal(8L, tci(read.tree(text = '(((1,2),(3,4)),5);')))
  expect_equal(9L, tci(read.tree(text = '(((1,2,3),4),5);')))
  expect_equal(10L, tci(read.tree(text = '((((1,2),3),4),5);')))
})

test_that("TCI context correct", {
  tree7 <- read.tree(text = "(1, (2, 3, ((4, 5), (6, 7))));")
  context7 <- structure(list(maximum = 35, minimum = 11,
                             uniform.expected = 25.54,
                             yule.expected = 19.7, yule.variance = 42.78),
                        class = "data.frame", row.names = c(NA, -1L))

  expect_equal(context7, tci.context(tree7), tolerance = 1e-2)
  expect_equal(context7, tci.context.n(7), tolerance = 1e-2)

})

test_that("TCI context correct", {
  tree7 <- read.tree(text = "(1, (2, 3, ((4, 5), (6, 7))));")
  expect_equal(11:8, list.ancestors(tree7$edge[, 1], tree7$edge[, 2], 5))
})
