rm(list = ls())

library(foreign)

wd <- "~/Dropbox/data/replicationFiles/cox-magar1999/"
setwd(wd)

d <- read.dta("apsrhou.dta")
head(d)
table(d$chfrosh)
