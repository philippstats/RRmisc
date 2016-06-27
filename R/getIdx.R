#' get Index
#' 
#' @param len Index length
#' @param total.len Total lenfth
#' @param seed seed

getIdx = function(len, total.len, seed) {
	set.seed(seed)
	x = sample(x = 1:total.len, len)
	print(x)
	x
}