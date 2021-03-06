###############################################################################
# STACKING
###############################################################################

#' Create Base Learners 2
#' 
#' @param task [\code{Task}]\cr
#'   Used for mtry in randomForest
#' @import mlr
#' @export

createBaselearners4b = function(task) {
  
  ###############################################################################
  # KNN

  
  k.max = 100
  k = as.integer(seq(3, k.max, length.out = 25))
  
  lrns.knn.rec = vector("list", length = length(k))
  for (i in k) {
    lrns.knn.rec[[which(k == i)]] = makeLearner("classif.kknn", 
      id = paste0("kknn_rec_", i), 
      fix.factors.prediction = TRUE,
      predict.type = "prob",
      kernel = "rectangular", 
      k = i
    )}
  
  lrns.knn.inv = vector("list", length = length(k))
  for (i in k) {
    lrns.knn.inv[[which(k == i)]] = makeLearner("classif.kknn", 
      id = paste0("kknn_inv_", i), 
      fix.factors.prediction = TRUE,
      predict.type = "prob",
      kernel = "inv", 
      k = i
    )}
  
  ###############################################################################
  # Neuronal Net
  
  hidden.units = 2^(seq(1, 7, 2)) #momentum = c(0, 0.2, 0.5, 0.9)
  maxit = as.integer(seq(50, 200, length.out = 5))
  skip = c(TRUE, FALSE)
  decay = c(0, 1e-4, 1e-3)
  
  grid = expand.grid(hidden.units, maxit, skip, decay)
  grid.length = dim(grid)[1]
  
  lrns.nnet = vector("list", length = grid.length)
  for (i in 1:grid.length) {
    lrns.nnet[[i]] = makeLearner("classif.nnet", 
      id = paste0("nnet_", grid[i, 1], "_", grid[i, 2], "_", grid[i, 3], "_", grid[i, 4]),
      predict.type = "prob",
      fix.factors.prediction = TRUE,
      trace = FALSE,
      # parameters
      size = grid[i, 1],
      maxit = grid[i, 2], 
      skip = grid[i, 3],
      decay = grid[i, 4],
      MaxNWts = 35000
    )}
  
  
  ###############################################################################
  # randomForest
  
  ntree = seq(500, 2500, by = 200) 
  mtry = unique(as.integer(seq(3, getTaskNFeats(task)-1, length.out = 2)))
  nodesize = 2^(c(1, 3, 7))
  maxnodes = c(10, 100)
  
  grid = expand.grid(ntree, mtry, nodesize, maxnodes)
  grid.length = nrow(grid)
  
  lrns.rf = vector("list", length = grid.length)
  for (i in 1:grid.length) {
    lrns.rf[[i]] = makeLearner("classif.randomForest", 
      id = paste0("rF_", grid[i, 1], "_", grid[i, 2], "_",  grid[i, 3], "_",  grid[i, 4]),
      predict.type = "prob",
      fix.factors.prediction = TRUE,
      # parameters
      ntree = grid[i, 1],
      mtry = grid[i, 2],
      nodesize = grid[i, 3],
      maxnodes = grid[i, 4]
    )}
  
  ###############################################################################
  # 
  #
  #n.trees = as.integer(seq(1000, 2500, length.out = 10))
  #interaction.depth = c(1, 2, 5, 10)
  #shrinkage = c(0.001, 0.01, 0.1)
  #
  #grid = expand.grid(n.trees, interaction.depth, shrinkage)
  #grid.length = nrow(grid)
  #
  #lrns.gbm = vector("list", length = grid.length)
  #for (i in 1:grid.length) {
  #  lrns.gbm[[i]] = makeLearner("classif.gbm",
  #    id = paste0("gbm_t", grid[i, 1], "_i", grid[i, 2], "_s", grid[i, 3]),
  #    predict.type = "prob",
  #    fix.factors.prediction = TRUE,
  #    # parameters
  #    distribution = "bernoulli",
  #    n.trees = grid[i, 1],
  #    interaction.depth = grid[i, 2],
  #    shrinkage = grid[i, 3]
  #  )}
  #
  ###############################################################################
  # xgboost
  
  nrounds = as.integer(seq(500, 2500, length.out = 3))
  eta = 2^(-7:-4)
  max_depth = c(1, 5, 8, 20)
  colsample_bytree =  c(.7, .8, .9)
  subsample = c(0.8)
  
  grid = expand.grid(nrounds, eta, max_depth, colsample_bytree, subsample)
  grid.length = nrow(grid)
  
  lrns.xgboost = vector("list", length = grid.length)
  for (i in 1:grid.length) {
    lrns.xgboost[[i]] = makeLearner("classif.xgboost", verbose = 0,
      id = paste0("xgboost_", grid[i, 1], "_", grid[i, 2], "_", grid[i, 3], "_", grid[i, 4]), #, grid[i, 5])
      predict.type = "prob",
      fix.factors.prediction = TRUE,
      # parameters
      nrounds = grid[i, 1],
      eta = grid[i, 2],
      max_depth = grid[i, 3],
      colsample_bytree = grid[i, 4],
      subsample = grid[i, 5]

    )}


  BASELEARNERS = c(lrns.knn.rec, lrns.knn.inv, 
                   lrns.nnet, 
                   lrns.rf, 
                   lrns.xgboost
  )
  
  return(BASELEARNERS)
}






###############################################################################
# BoostedStacking
###############################################################################

#' MT: Make Model ultiplexer Setting
#' @import mlr
#' @export

createMM4b = function() {
  mm.lrns = list(
    makeLearner("classif.kknn", predict.type = "prob", fix.factors.prediction = TRUE),
    makeLearner("classif.nnet", predict.type = "prob", MaxNWts = 35000, fix.factors.prediction = TRUE, trace = FALSE),
    makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE),
    #makeLearner("classif.gbm", predict.type = "prob", fix.factors.prediction = TRUE),
    makeLearner("classif.xgboost", predict.type = "prob", fix.factors.prediction = TRUE, colsample_bytree = .8, subsample = .8, verbose = 0)
  )
  makeModelMultiplexer(mm.lrns)  
}


#' MT: Make Model Multiplexer Param Set Setting
#' @param mm Model Multiplexer (from MTmakeMM)
#' @param task Task 
#' @import mlr
#' @export

createMMPS4b = function(mm, task) {
  makeModelMultiplexerParamSet(mm,
   # knn
   classif.kknn = makeParamSet(
     makeIntegerParam("k", lower = 1, upper = 50, default = 3),
     makeDiscreteParam("kernel", values = c("inv", "rectangular"))
   ),
   # nnet
   classif.nnet = makeParamSet(
     makeIntegerParam("size", lower = 1, upper = 200), 
     makeIntegerParam("maxit", lower = 50, upper = 500),
     makeLogicalParam("skip"),
     makeNumericParam("decay", lower = 0, upper = 1e-3)
   ),
   # rF
   classif.randomForest = makeParamSet(
     makeIntegerParam("ntree", lower = 500L, upper = 2500L),
     makeIntegerParam("mtry", lower = 1L, upper = getTaskNFeats(task)),
     makeIntegerParam("nodesize", lower = 2L, upper = 128L),
     makeIntegerParam("maxnodes", lower = 10L, upper = 100L)
   ),
   # gbm
   #classif.gbm = makeParamSet(
   #   makeIntegerParam("n.trees", lower = 500L, upper = 2500L),
   #  makeIntegerParam("interaction.depth", lower = 1L, upper = 10L),
   #  makeNumericParam("shrinkage", lower = 10^-5, upper = 0.1)
   #),
   # xgb
   classif.xgboost = makeParamSet(
     makeNumericParam("eta", lower = 2^(-7), upper = 2^(-4)),
     makeIntegerParam("max_depth", lower = 1L, upper = 20L),
     makeIntegerParam("nrounds", lower = 500L, upper = 2500L),
     makeNumericParam("colsample_bytree", lower = .7, upper = .9)
     #makeNumericParam("subsample", lower = 0.7, upper = 0.9)
   )
   # svm
   #classif.svm = makeParamSet(
   #  makeDiscreteParam("kernel", values = c("linear", "polynomial", "radial")),
   #  makeNumericParam("cost", lower = 2^-12, upper = 2^12),
   #  makeIntegerParam("degree", lower = 2L, upper = 5L, requires = quote(kernel == "polynomial")),
   #  makeNumericParam("gamma", lower = 2^(-12), upper = 2^12, requires = quote(kernel == "radial"))
   #)
  )
}


