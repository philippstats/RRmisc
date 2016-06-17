#' Extract list from list
#' 
#' @param xs list
#' @param element1 character from upper list
#' @param element2 character from inner list
#' @importFrom BBmisc extractSubList
#' @export

extractSubSubList = function(xs, element1, element2) {
  extractSubList(extractSubList(xs, element1), element2)
}