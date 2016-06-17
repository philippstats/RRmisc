# FIXME: 
# take pred with prob and response
# take classif, multiclass, etc.
# rename data2 (last thing to do)
# CHECK use same target name 
# assert's
# make me a S3 object

#' Adds feature to task
#' 
#' Adds a new feature to a \code{Task}. New feature can be simple coloum vectors or predictions
#' @param task \code{Task}
#' @param new.col \code{numeric} same length as observations available
#' @param pred \code{Prediction} result. 

makeTaskWithNewFeat = function(task, new.col = NULL, pred = NULL, predict.type = NULL, feat.name = "feat") {
  if (!is.null(pred) && predict.type == "prob") {
    new.col = getPredictionProbabilities(pred)
  }
  if (!is.null(new.col)) { # col
      n.new.col = NCOL(new.col)
      if (n.new.col > 1) feat.name = paste(feat.name, seq_len(n.new.col), sep = "_")
      data = getTaskData(task, target.extra = TRUE)
      data1 = cbind(data$data, new.col)
      colnames(data1)[(NCOL(data$data)+1):NCOL(data1)] = feat.name
      data2 = cbind(data1, target = data$target)
      target.name = task$task.desc$target
      colnames(data2)[ncol(data2)] = target.name 
    if (task$type == "classif") {
      task = makeClassifTask(id = task$task.desc$id, data = data2, target = target.name, positive = task$task.desc$positive)
      return(task = task)
      
    } else {
      stop("!=classif not implemented yet")
    }
  } else { # pred
    data = getTaskData(task, target.extra = TRUE)
    # FIXME response or pred.pos!?
    new.feat = pred$data$response
    data1 = cbind(data$data, new.feat)
    colnames(data1)[ncol(data1)] = feat.name
    data2 = cbind(data1, target = data$target)
    target.name = task$task.desc$target
    colnames(data2)[ncol(data2)] = target.name 
    if (task$type == "classif") {
      makeClassifTask(id = task$task.desc$id, data = data2, target = target.name, positive = task$task.desc$positive)
    } else {
      stop("not implemented yet")
    }
  }
}

