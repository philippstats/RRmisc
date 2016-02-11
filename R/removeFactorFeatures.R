#' Count number of factor levels for all variables, returns 0 for all non-factors
#' 
#' @param task [\code{Task}]
#' @param threshold [\code{integer(1)}] Indicates threshold which features 
#' should be returned
#' @param larger.threshold [\code{logical(1)}] If TRUE only features with 
#' number of levels larger-equal threshold are returned. If set to FALSE only 
#' smaller-equall features are returned
#' @export

countTaskClassCharacteristics = function(task, threshold = 0L, larger.threshold = TRUE) {
  assertClass(task, classes = "Task")
  assertInteger(threshold, lower = 0L, len = 1L)
  assertLogical(larger.threshold, len = 1L)
  data = task$env$data
  d = data.frame(
    variables = colnames(data),
    class = sapply(data, class),
    count = vapply(data, function(x) ifelse(class(x) == "factor", nlevels(x), 0L), numeric(1)),
    stringsAsFactors = FALSE
  )
  rownames(d) = NULL
  if(larger.threshold) {
    d = subset(d, subset = count >= threshold)
  } else {
    d = subset(d, subset = count <= threshold)
  }
  d
}

#' Remove Features from Task which have too man factor levels
#' 
#' @param task [\code{Task}] from mlr.
#' @param n.levels [\code{integer(1)}] threshold. Default is 32, which refers
#' to the maximal amount of levels allowed in \code{randomForest}.
#' @export

removeFactorFeatures = function(task, n.levels = 32) {
  d = countTaskClassCharacteristics(task, threshold = n.levels, TRUE)[, 1]
  keep = setdiff(getTaskFeatureNames(task), d)
  subsetTask(task, features = keep)
}

