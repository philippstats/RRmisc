# test
# getStratifiedBinaryData

###############################################################################

data(BreastCancer, package = "mlbench")
df = BreastCancer
df$Id = NULL
classif.task = makeClassifTask(id = "BreastCancer", data = df, target = "Class")


###############################################################################

t = getStratifiedBinaryData(task = classif.task, size = 100L)
r = as.vector(table(getTaskTargets(t))) 

###############################################################################

expect_equal(getTaskSize(t), 100L) 
expect_equal(r, c(50L, 50L))
