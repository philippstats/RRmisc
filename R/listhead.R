#' Head of data.fames in list
#' 
#' @param x list
#' @param ... further head args
#' @export

listhead = function(x, ...) {
  lapply(x, function(y) head(y, ...))
}


#' Tail of data.fames in list
#' 
#' @param x list
#' @param ... further tail args
#' @export

listhead = function(x, ...) {
  lapply(x, function(y) tail(y, ....))
}
