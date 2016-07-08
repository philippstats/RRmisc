###############################################################################
# STACKING
###############################################################################

#' Create Base Learners 
#' 
#' @param task [\code{Task}]\cr
#'   Used for mtry in randomForest
#' @import mlr
#' @export

MTcreateBaselearners = function(task) {
  
  ###############################################################################
  # KNN

  
  k.max = 100
  k = as.integer(seq(1, k.max, length.out = 5))
  
  lrns.knn.rec = vector("list", length = length(k))
  for (i in k) {
    lrns.knn.rec[[which(k == i)]] = makeLearner("classif.kknn", 
      id = paste0("kknn.rec.", i), 
      fix.factors.prediction = TRUE,
      predict.type = "prob",
      kernel = "rectangular", 
      k = i
    )}
  
  lrns.knn.inv = vector("list", length = length(k))
  for (i in k) {
    lrns.knn.inv[[which(k == i)]] = makeLearner("classif.kknn", 
      id = paste0("kknn.inv.", i), 
      fix.factors.prediction = TRUE,
      predict.type = "prob",
      kernel = "inv", 
      k = i
    )}
  
  ###############################################################################
  # Neuronal Net
  
  hidden.units = c(1, 2, 4, 8, 32, 128) #momentum = c(0, 0.2, 0.5, 0.9)
  maxit = as.integer(seq(50, 200, length.out = 8))
  
  grid = expand.grid(hidden.units, maxit)
  grid.length = dim(grid)[1]
  
  lrns.nnet = vector("list", length = grid.length)
  for (i in 1:grid.length) {
    lrns.nnet[[i]] = makeLearner("classif.nnet", 
      id = paste0("nnet.s", grid[i, 1], ".m", grid[i, 2]),
      predict.type = "prob",
      fix.factors.prediction = TRUE,
      trace = FALSE,
      # parameters
      size = grid[i, 1],
      maxit = grid[i, 2], 
      MaxNWts = 35000
    )}
  
  
  ###############################################################################
  # randomForest
  
  ntree = seq(100, 1000, by = 100) 
  mtry = unique(as.integer(seq(1, getTaskNFeats(task), length.out = 8)))
  
  grid = expand.grid(ntree, mtry)
  grid.length = nrow(grid)
  
  lrns.rf = vector("list", length = grid.length)
  for (i in 1:grid.length) {
    lrns.rf[[i]] = makeLearner("classif.randomForest", 
      id = paste0("rF.t", grid[i, 1], ".m", grid[i, 2]),
      predict.type = "prob",
      fix.factors.prediction = TRUE,
      # parameters
      ntree = grid[i, 1],
      mtry = grid[i, 2]
    )}
  
  ###############################################################################
  # gbm
  
  n.trees = seq(100, 1000, length.out = 10)
  interaction.depth = c(1, 2, 5, 10)
  shrinkage = c(0.001, 0.01, 0.1)
  
  grid = expand.grid(n.trees, interaction.depth, shrinkage)
  grid.length = nrow(grid)
  
  lrns.gbm = vector("list", length = grid.length)
  for (i in 1:grid.length) {
    lrns.gbm[[i]] = makeLearner("classif.gbm",
      id = paste0("gbm.t", grid[i, 1], ".i", grid[i, 2], ".s", grid[i, 3]),
      predict.type = "prob",
      fix.factors.prediction = TRUE,
      # parameters
      distribution = "bernoulli",
      n.trees = grid[i, 1],
      interaction.depth = grid[i, 2],
      shrinkage = grid[i, 3]
    )}
  
  
  
  ###############################################################################
  # SVM

  C = 10^(-7:3)
  C.length = length(C)
  
  # linear 
  lrns.svm.lin = vector("list", length = C.length)
  for (i in seq_along(C)) {
    lrns.svm.lin[[i]] = makeLearner("classif.svm", 
      id = paste0("svm.lin.C", C[i]), kernel = "linear", 
      predict.type = "prob",
      fix.factors.prediction = TRUE,
      cost = C[i]
    )}
  
  # polyn
  
  degree = 2:3
  
  grid = expand.grid(C, degree)
  grid.length = nrow(grid)
  
  lrns.svm.poly = vector("list", length = grid.length)
  for (i in 1:grid.length) {
    lrns.svm.poly[[i]] = makeLearner("classif.svm", 
      id = paste0("svm.poly.C", grid[i, 1], ".d", grid[i, 2]), 
      predict.type = "prob",
      fix.factors.prediction = TRUE,
      kernel = "polynomial", 
      cost = grid[i, 1], 
      degree = grid[i, 2] 
    )}
  
  # radial
  
  sigma = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2)
  
  grid = expand.grid(C, sigma)
  grid.length = nrow(grid)
  
  lrns.svm.rbf = vector("list", length = grid.length)
  for (i in 1:grid.length) {
    lrns.svm.rbf[[i]] = makeLearner("classif.svm", 
      id = paste0("svm.rad.C", grid[i, 1], ".s", grid[i, 2]), 
      predict.type = "prob",
      fix.factors.prediction = TRUE,
      cost = grid[i, 1], 
      kernel = "radial", 
      gamma = grid[i, 2]
    )}
  
  
  ###############################################################################
  #

  BASELEARNERS = c(lrns.knn.rec, lrns.knn.inv, #5+5=10
                   lrns.nnet, #=48
                   lrns.rf, #=100
                   lrns.gbm, #=120
                   lrns.svm.poly, lrns.svm.rbf #22+88=110
  )
  
  return(BASELEARNERS)
}






###############################################################################
# BoostedStacking
###############################################################################

#' MT: Make Model ultiplexer Setting
#' @import mlr
#' @export

MTcreateMM = function() {
  mm.lrns = list(
    makeLearner("classif.kknn", predict.type = "prob", kernel = "inv", fix.factors.prediction = TRUE),
    makeLearner("classif.nnet", predict.type = "prob", MaxNWts = 35000, fix.factors.prediction = TRUE, trace = FALSE),
    makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE),
    makeLearner("classif.gbm", predict.type = "prob", fix.factors.prediction = TRUE),
    #makeLearner("classif.xgboost", predict.type = "prob", fix.factors.prediction = TRUE),
    makeLearner("classif.svm", predict.type = "prob", fix.factors.prediction = TRUE))
  
  #mm.lrns = lapply(mm.lrns, setPredictType, bpt)
  MM = makeModelMultiplexer(mm.lrns)  
}


#' MT: Make Model Multiplexer Param Set Setting
#' @param mm Model Multiplexer (from MTmakeMM)
#' @param task Task 
#' @import mlr
#' @export

MTcreateMMPS = function(mm, task) {
  makeModelMultiplexerParamSet(mm,
   # knn
   classif.kknn = makeParamSet(
     makeIntegerParam("k", lower = 1, upper = 50)
   ),
   # nnet
   classif.nnet = makeParamSet(
     makeIntegerParam("size", lower = 1, upper = 200), 
     makeIntegerParam("maxit", lower = 50, upper = 500)
   ),
   # rF
   classif.randomForest = makeParamSet(
     makeIntegerParam("ntree", lower = 1L, upper = 500L),
     makeIntegerParam("mtry", lower = 1L, upper = getTaskNFeats(task))
   ),
   # gbm
   classif.gbm = makeParamSet(
     makeIntegerParam("n.trees", lower = 1L, upper = 500L),
     makeIntegerParam("interaction.depth", lower = 1L, upper = 10L),
     makeNumericParam("shrinkage", lower = 10^-5, upper = 0.5)
   ),
   # xgb
   #classif.xgboost = makeParamSet(
   #  makeNumericParam("eta", lower = 2^(-7), upper = 2^(-5)),
   #  makeIntegerParam("max_depth", lower = 1L, upper = 5),
   #  makeIntegerParam("nrounds", lower = 300L, upper = 1200)
   #),
   # svm
   classif.svm = makeParamSet(
     makeDiscreteParam("kernel", values = c("linear", "polynomial", "radial")),
     makeNumericParam("cost", lower = 2^-12, upper = 2^12),
     makeIntegerParam("degree", lower = 2L, upper = 5L, requires = quote(kernel == "polynomial")),
     makeNumericParam("gamma", lower = 2^(-12), upper = 2^12, requires = quote(kernel == "radial"))
   )
  )
}


