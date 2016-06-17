setwd("~/Documents/Rdevelop")
library(devtools); library(roxygen2)

unload("RRmisc")
detach("package:RRmisc", unload = TRUE)
remove.packages("RRmisc")

unlink("RRmisc.tar.gz")

document("RRmisc")

build("RRmisc", vignettes = FALSE)

install.packages("RRmisc_0.01-2.tar.gz", repos = NULL, type = "source")
library(RRmisc)
###
test(pkg = "RRmisc")
