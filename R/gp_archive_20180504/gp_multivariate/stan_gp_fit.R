library(abind)
library(ggplot2)
library(rstan)

setwd("~/docs/Iterative_ML_paper/R/gp_multivariate")

rm(list = ls())

source("create_data.R")
# source("gp_univariate_genfit.R")

burn.iter = 1000
sample.iter = 1000

load("data.RData")
N = nrow(x)
k = ncol(x)

N.pred = 100
x.pred = seq(min(x), max(x), length.out = N.pred)

stan.data = list(N = N, k = k, y = y, x = x)
stan.fit = stan("gp_multivariate_fit.stan", data = stan.data,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
stan.extract = extract(stan.fit)
