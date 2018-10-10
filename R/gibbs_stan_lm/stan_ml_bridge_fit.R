library(bridgesampling)
library(rstan)
library(quantreg)

setwd("~/git/Iterative_ML/R/gibbs_stan_lm")

# rm(list = ls())

#Load data
load("lmData.RData")

#Priors
mu.beta = rep(0, ncol(X))
sd.beta = sqrt(rep(100, ncol(X)))

tau.shape = 1
tau.rate = 1

#MCMC parameters:
burn.iter = 500
sample.iter = 5000

# burn.iter = 1
# sample.iter = 1

################
#Unrestricted model
load("unrestricted.stan.dso.RData")

model.data = list(N = nrow(X), k = ncol(X),
                  y = c(y), X = X,
                  mu_beta = mu.beta, sd_beta = sd.beta,
                  tau_shape = tau.shape, tau_rate = tau.rate)
stan.fit = stan("stan_model.stan",
                data = model.data,
                chains = 1,
                fit = unrestricted.stan.dso,
                warmup = burn.iter, iter = burn.iter + sample.iter)

log.marginal = bridge_sampler(stan.fit, method = "normal")$logml
