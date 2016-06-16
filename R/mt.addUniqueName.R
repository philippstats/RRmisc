# Add unique name to experiment result

mt.addUniqueName = function(data) {
  uniq.name = vector("character", nrow(data))
  for (i in 1:nrow(data)) {
    if (data$algo[i] == "AVE") {
      uniq.name[i] = "AVE"
    }
    if (data$algo[i] == "SUPERLEARNED_STACK") {
      uniq.name[i] = paste("SUPERLEARNER", data[i, "slrm"], ifelse(data[i, "use.feat"], "w/", "w/o")) 
    }
    if (data$algo[i] == "ENSEMBLE_SELECTION") {
      uniq.name[i] = paste("ENSEL", data[i, "bagprob"], "/", data[i, "bagtime"], "/", data[i, "init"], "/", data[i, "maxiter"]) 
    }
    if (data$algo[i] == "BEST_BL") {
      uniq.name[i] = "BEST BASELEARNER"
    }
    if (data$algo[i] == "BOOSTED_STACKING") {
      uniq.name[i] =  paste("BOOSTED STACK", data[i, "maxit"],"/", data[i, "niter"]) 
    }
  }
  data$uniq.name = uniq.name
  data
}
