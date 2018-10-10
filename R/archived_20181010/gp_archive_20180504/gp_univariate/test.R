library(ggplot2)

setwd("~/docs/Iterative_ML_paper/R/gp_univariate")

rm(list = ls())

source("gp_univariate_genfit.R")
load("data.RData")

N.pred = 100
alpha = 5
length.scale = 5
sigma = 1

x.is = x
y.is = y
x.pred = seq(min(x.is), max(x.is), length.out = N.pred)

fit = gp.fit(x.pred, y.is, x.is, alpha, length.scale, sigma)

point.df = data.frame(x, y)
fit.df = data.frame(x = x.pred, y = fit[,1])

ggplot(point.df, aes(x, y)) + geom_point() +
  geom_line(data = fit.df, color = "red")
