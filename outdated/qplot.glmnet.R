#' Plot glmnet result in ggplot style
#' 
#' Code comes from glmnet::plot.glmnet
#' @import checkmate
#' @import ggplot2
#' @export

qplot.glmnet = function (x, xvar = c("norm", "lambda", "dev"), label = FALSE, plotly = TRUE, ...) {
  assertClass(x, "glmnet")
  assertChoice(xvar, c("norm", "lambda", "dev"))
  assertLogical(label)
  assertLogical(plotly)
  qplotCoef(x$beta, lambda = x$lambda, df = x$df, dev = x$dev.ratio, 
    label = label, xvar = xvar, plotly = plotly, ...)
}


#' qplotCoef
#' 
#' @import ggplot2

qplotCoef = function (beta, norm, lambda, df, dev, label = FALSE, xvar = c("norm", 
    "lambda", "dev"), xlab = iname, ylab = "Coefficients", plotly, ...) {
    which = nonzeroCoef(beta)
    nwhich = length(which)
    switch(nwhich + 1, `0` = {
        warning("No plot produced since all coefficients zero")
        return()
    }, `1` = warning("1 or less nonzero coefficients; glmnet plot is not meaningful"))
    beta = as.matrix(beta[which, , drop = FALSE])
    xvar = match.arg(xvar)
    switch(xvar, norm = {
        index = if (missing(norm)) apply(abs(beta), 2, sum) else norm
        iname = "L1 Norm"
        approx.f = 1
    }, lambda = {
        index = log(lambda)
        iname = "Log Lambda"
        approx.f = 0
    }, dev = {
        index = dev
        iname = "Fraction Deviance Explained"
        approx.f = 1
    })
    dotlist = list(...)
      data = data.frame(xval = index, t(beta))
      data = gather(data, Feature, value, -xval)
      data$Feature = substring(data$Feature, 9)
      g = ggplot(data, aes(x = xval, y = value, col = Feature)) + geom_line(...) + xlab(xlab) + ylab(ylab) 
      if (plotly) {g = ggplotly(g)}
      print(g)
}

#' nonzeroCoef

nonzeroCoef = function(beta, bystep = FALSE) {
    nr = nrow(beta)
    if (nr == 1) {
        if (bystep) 
            apply(beta, 2, function(x) if (abs(x) > 0) 
                1
            else NULL)
        else {
            if (any(abs(beta) > 0)) 
                1
            else NULL
        }
    }
    else {
        beta = abs(beta) > 0
        which = seq(nr)
        ones = rep(1, ncol(beta))
        nz = as.vector((beta %*% ones) > 0)
        which = which[nz]
        if (bystep) {
            if (length(which) > 0) {
                beta = as.matrix(beta[which, , drop = FALSE])
                nzel = function(x, which) if (any(x)) 
                  which[x]
                else NULL
                which = apply(beta, 2, nzel, which)
                if (!is.list(which)) 
                  which = data.frame(which)
                which
            }
            else {
                dn = dimnames(beta)[[2]]
                which = vector("list", length(dn))
                names(which) = dn
                which
            }
        }
        else which
    }
}