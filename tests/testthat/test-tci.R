context("Total cophenetic index tests")
library("ape")

test_that("Trees from Mir et al. 2013 are scored correctly", {
  expect_equal(0L, tci(read.tree(text='(1,2,3,4,5);')))
  expect_equal(1L, tci(read.tree(text='((1,2),3,4,5);')))
  expect_equal(2L, tci(read.tree(text='((1,2),(3,4),5);')))
  expect_equal(3L, tci(read.tree(text='((1,2,3),4,5);')))
  expect_equal(4L, tci(read.tree(text='(((1,2),3),4,5);')))
  expect_equal(4L, tci(read.tree(text='((1,2,3),(4,5));')))
  expect_equal(5L, tci(read.tree(text='(((1,2),3),(4,5));')))
  expect_equal(6L, tci(read.tree(text='((1,2,3,4),5);')))
  expect_equal(7L, tci(read.tree(text='(((1,2),3,4),5);')))
  expect_equal(8L, tci(read.tree(text='(((1,2),(3,4)),5);')))
  expect_equal(9L, tci(read.tree(text='(((1,2,3),4),5);')))
  expect_equal(10L, tci(read.tree(text='((((1,2),3),4),5);')))
})