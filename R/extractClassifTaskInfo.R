#' Extract ClassifTask Info 
#' 
#' @import checkmate 
#' @importFrom dplyr count
#' @import mlr
#' @export 
#' @examples \dontrun{
#' library(mlr)
#' library(xtable)
#' tasks = list(pid.task, pid.task)
#' x = lapply(tasks, function(x) extractTaskInfo(x))
#' x = data.frame(Reduce(rbind, x))
#' xtable(x)
#' }


extractClassifTaskInfo = function(task) {
  assertClass(task, "ClassifTask")
  id = getTaskId(task)
  id = unlist(strsplit(id, "[.]"))
  id[2] = substr(id[2], 3, nchar(id[2]))
  id = t(id)
  obs = getTaskSize(task)
  feat = data.frame(t(task$task.desc$n.feat))
  target = task$task.desc$target
  x = count(task$env$data, target)
  pos.loc = which(x[, target] == task$task.desc$positive)
  neg.loc = which(x[, target] == task$task.desc$negative)
  obs.pos = x[pos.loc, 2]
  obs.neg = x[neg.loc, 2]
  
  # I do not need "Ordered"
  d = data.frame(id = id, obs = obs, feat = feat[, 1:2], target = target, obs.pos = obs.pos, obs.neg = obs.neg)
  #names(d) = c("ID", "Observations", "Numerics", "Factors", "Ordered", 
  #             "Target", "Obs. Positive", "Obs. Negative")
  names(d) = c("Task", "ID", "Obs.", "Num.", "Fac.", 
               "Target", "#Pos.", "#Neg.")
  d
}
