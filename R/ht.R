#' Print head and tail
#' 
#' @param data data.frame
#' @param n n
#' @export
ht = function(data, n = 4) {
  rbind(head(data, n), tail(data, n))
}