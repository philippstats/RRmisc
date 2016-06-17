#' Convert character features to factor features in data.frames
#' 
#' @param data [\code{data.frame}]
#' @import checkmate
#' 
#' @export
#' @examples 
#' \dontrun{
#' data("mpg", package = "ggplot2")
#' str(mpg)
#' convertCharacterToFactor(mpg)
#' str(mpg)}

convertCharacterToFactor = function(data) {
  assertClass(data, "data.frame")
  
  idx = sapply(data, is.character)
  data[i] = lapply(data[idx], as.factor)
  data
}

