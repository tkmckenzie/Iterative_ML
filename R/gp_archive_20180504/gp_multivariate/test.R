library(ggplot2)

setwd("~/docs/Iterative_ML_paper/R/gp_multivariate")

rm(list = ls())

x = runif(5)
M = rWishart(1, 5, diag(5))[,,1]

t(x) %*% M %*% x
