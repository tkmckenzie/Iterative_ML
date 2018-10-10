library(abind)
library(ggplot2)
library(rstan)

setwd("~/docs/Iterative_ML_paper/R/gp_univariate")

rm(list = ls())

source("create_data.R")
source("gp_univariate_genfit.R")

burn.iter = 1000
sample.iter = 1000

load("data.RData")
N = length(x)

N.pred = 100
x.pred = seq(min(x), max(x), length.out = N.pred)

stan.data = list(N = N, y = y, x = x)
stan.fit = stan("gp_univariate_fit.stan", data = stan.data,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
stan.extract = extract(stan.fit)

#Extract fit
fit = lapply(1:sample.iter, function(i) gp.fit(x.pred, y, x,
                                               stan.extract$alpha[i],
                                               stan.extract$length_scale[i],
                                               stan.extract$sigma[i]))
fit = Reduce(function(m1, m2) abind(m1, m2, along = 3), fit)
mean.fit = apply(fit, c(1, 2), mean)

#Plot
point.df = data.frame(x = x, y = y)
fit.df = data.frame(x = x.pred, y = mean.fit[,1])
ggplot(point.df, aes(x, y)) +
  geom_point() + 
  geom_line(data = fit.df, color = "red")

fit.df = data.frame(x = rep(x.pred, times = sample.iter), y = c(fit[,1,]), group = rep(1:sample.iter, each = N.pred))
ggplot(point.df, aes(x, y)) +
  geom_point() + 
  geom_line(data = fit.df, aes(group = group), alpha = 0.05)
