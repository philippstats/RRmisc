# test
# getStratifiedBinaryData
library(testthat)
library(mlbench)
library(mlr)


###############################################################################

test_that("check basic properties", {
  data(BreastCancer, package = "mlbench")
  df = BreastCancer
  df$Id = NULL
  tsk = makeClassifTask(id = "BreastCancer", data = df, target = "Class")
  t = getStratifiedBinaryData(task = tsk, size = 100L)
  r = as.vector(table(getTaskTargets(t))) 

  expect_equal(getTaskSize(t), 100L)
  expect_equal(r, c(50L, 50L))
})
