library(rstan)

setwd("~/docs/Iterative_ML_paper/R/gp_multivariate")

rm(list = ls())

N = 5
x = runif(N)

stan.data = list(N = N, x = x)
stan.fit = stan("stan_func_test.stan", data = stan.data,
                algorithm = "Fixed_param",
                chains = 1, iter = 2, warmup = 1)
stan.extract = extract(stan.fit)

stan.extract$test_val
