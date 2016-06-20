
#' Make Task from OpenML data set with some preprocessing
#' 
#' Converts OpenML data set in tasks, merging small factor levels, remove missings, 
#' and remove constant features. This function name the task and prompt the user 
#' to name the target if this is not specified in the OpenML data set.
#' @export
#' @import mlr
#' @importFrom OpenML getOMLDataSet
#' @importFrom stringr str_replace_all

createTaskFromOML = function(id, min.perc = 0.05, perc = 0.05, na.omit = TRUE) {
  d = getOMLDataSet(did = id)
  name.char = str_replace_all(d$desc$name, "[^[:alnum:]]", "")
  name.id = d$desc$id
  name = paste0(name.char, ".id", name.id)
  messagef("This is %s", name)
  if (length(d$default.target.attribute) == 0) { # Target names are sometimes missing
    if (length(d$target.features) == 0) {
      target.name = readline(prompt = "Target name is missing in OML onject.\n Plase enter the target name: ")
      d$desc$default.target.attribute = target.name
      d$target.features = target.name
    } else {
      d$default.target.attribute = d$target.features
    }
  } else {
    if (length(d$target.features) == 0)
      d$target.features = d$default.target.attribute
  }
  task = convertOMLDataSetToMlr(d)
  pos = getTaskDescription(task)$positive
  # remove missings
  data = getTaskData(task)
  if (na.omit) {
    data = data[complete.cases(data), ]
  } 
  # make task
  task = makeClassifTask(id = name, data = data, target = d$target.features, positive = pos)
  task = mergeSmallFactorLevels(task, min.perc = min.perc)
  task = removeConstantFeatures(task, perc = perc)
  task
}

#' Print OpenML Data Description
#' 
#' @importFrom OpenML getOMLDataSet
#' @importFrom BBmisc messagef
#' @export

printOMLDataDescription = function(id) {
  d = getOMLDataSet(did = id)
  name = d$desc$name
  desc = d$desc$description
  messagef("This is data set %s", name)
  print(desc)
}


