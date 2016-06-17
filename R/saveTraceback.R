#' Save traceback() as list
#' 
#' 
saveTraceback = function() {
   convPairlistToList(traceback())
}


convPairlistToList <- function(x) {
  # http://stackoverflow.com/questions/29417134/what-is-the-difference-between-a-list-and-a-pairlist-in-r
  if(is.call(x)) x <- as.list(x)
  if(length(x) > 1) for(i in 2:length(x)) x[[i]] <- Recall(x[[i]])
  return(x)
}
