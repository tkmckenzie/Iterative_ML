library(rstan)

setwd("~/docs/Iterative_ML_paper/R/gp_univariate")

rm(list = ls())

load("data.RData")
N = length(x)
N.pred = 100
x.pred = seq(min(x), max(x), length.out = N.pred)
y.is = y
x.is = x
alpha = 1
length.scale = 0.15
sigma = 0.32

stan.data = list(N = N, N_pred = N.pred,
                 x_pred = x.pred, y_is = y.is, x_is = x.is,
                 alpha = alpha, length_scale = length.scale, sigma = sigma)
stan.fit = stan("stan_func_test.stan", data = stan.data,
                algorithm = "Fixed_param",
                chains = 1, iter = 2, warmup = 1)
stan.extract = extract(stan.fit)

stan.extract$res[1,,]
