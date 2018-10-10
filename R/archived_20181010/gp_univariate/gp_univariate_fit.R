library(ggplot2)
library(rstan)

setwd("~/docs/Iterative_ML_paper/R/gp_univariate")

rm(list = ls())

source("create_data.R")

burn.iter = 2000
sample.iter = 1000

stan.data = list(N = N, x = x, y = y)
stan.fit = stan("stan_gp_univariate.stan", data = stan.data,
                # control = list(adapt_delta = 0.95),
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
stan.extract = extract(stan.fit)

#Plot
point.df = data.frame(x, y)
fit.df = data.frame(x, y = apply(stan.extract$f, 2, mean))
ggplot(point.df, aes(x, y)) +
  geom_point() +
  geom_line(data = fit.df)
