#' Object size in MB
#' 
#' @param  obj obj
#' @export

objectsize = function(obj) {
  paste(round(object.size(obj) / 1000000, 2), "MB")
}