#' Paste function suppressing NAs
#' 
#' Source http://stackoverflow.com/questions/13673894/suppress-nas-in-paste (minor adjustments)
#' 
#' @param ... [\code{numeric/factor columns(n)}] Values to paste
#' @param sep [\code{character(1)}] Seperator
#' @importFrom stringr str_trim
#' @export

pasteSuppressNA = function(..., sep = "") {
  L = list(...)
  L = lapply(L, function(x) as.character(x))
  L = lapply(L, function(x) {x[is.na(x)] = ""; x})
  ret = gsub(paste0("(^", sep, "|", sep, "$)"),
    "", gsub(paste0(sep, sep), sep, do.call(paste, c(L, list(sep = sep)))))
  is.na(ret) = ret == ""
  str_trim(ret)
}
