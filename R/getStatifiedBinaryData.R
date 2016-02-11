#' Get stratified samples from data with binary target 
#' 
#' @param task [\code{Task}]
#' @param size [\code{integer(1)}] Total size of sample.
#' @export
 
getStratifiedBinaryData = function(task, size = 20L) {
  assertClass(task, classes = "Task")
  assertInteger(size, len = 1L, lower = 1L)

  data = getTaskData(task)
  datasize = getTaskSize(task)
  target = classif.task$task.desc$target
  #FIXME: make me nice
  if(length(levels(data[, target])) != 2) stop("target has more or less than 2 factor levels")

  half.size = size / 2
  data$tempdummy = as.integer(data[, target])
  
  s1 = subset(data, data$tempdummy == 1)
  s2 = subset(data, data$tempdummy == 2)
  
  d1 = sample_n(s1, size = half.size)
  d2 = sample_n(s2, size = half.size)
  
  d = rbind(d1, d2)
  d$tempdummy = NULL
  
  task$env$data = d
  task$task.desc$size = size
  task
}



