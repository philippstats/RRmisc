#' Makes Learner from a TuneResult Object
#' 
#' makeLearnerFromTuneResult 
#' @param result \code{TuneResult}

makeLearnerFromTuneResult = function(result = res) {
  assertClass(result, "TuneResult")
  if (!is.null(result$x$selected.learner)) { # from ModelMultiplexer
    lrn.char = result$x$selected.learner
    lrn.length = nchar(lrn.char)
    par.list = result$x[-1]
    par.names = substr(names(par.list), lrn.length + 2, nchar(names(par.list)))
    names(par.list) = par.names
    setHyperPars(makeLearner(lrn.char, 
      predict.type = result$learner$predict.type), par.vals = par.list)
  } else { # from "normal" tuning
    setHyperPars(makeLearner(class(result$learner)[1], 
      predict.type = result$learner$predict.type), par.vals = result$x)  
  }
}