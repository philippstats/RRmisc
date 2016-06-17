# Master's Thesis: Add unique name to experiment result

addUniqueNameMT = function(data) {
  uniq.name = vector("character", nrow(data))
  for (i in 1:nrow(data)) {
    if (data$algo[i] == "AVE") {
      uniq.name[i] = "AVE"
    }
    if (data$algo[i] == "SUPERLEARNED_STACK") {
      uniq.name[i] = paste("SL", data[i, "slrm"], ifelse(data[i, "use.feat"], "w/", "w/o")) 
    }
    if (data$algo[i] == "ENSEMBLE_SELECTION") {
      uniq.name[i] = paste("ES", data[i, "bagprob"], "/", data[i, "bagtime"], "/", data[i, "init"], "/", data[i, "maxiter"]) 
    }
    if (data$algo[i] == "BEST_BL") {
      uniq.name[i] = "BEST BL"
    }
    if (data$algo[i] == "BOOSTED_STACKING") {
      uniq.name[i] =  paste("BS", data[i, "maxit"],"/", data[i, "niter"]) 
    }
  }
  data$uniq.name = as.factor(uniq.name)
  data
}
