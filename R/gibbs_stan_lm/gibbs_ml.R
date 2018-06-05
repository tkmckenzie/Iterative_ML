library(MASS)
library(mvtnorm)

setwd("~/git/Iterative_ML/R/gibbs_stan_lm")

# rm(list = ls())

#MCMC parameters:
burn.iter = 500
sample.iter = 5000

load("lmData.RData")

#Data characteristics
N = nrow(X)
k = ncol(X)

#Priors
beta.mean.prior = rep(0, k)
beta.var.prior = 100 * diag(k)

tau.shape.prior = 1
tau.rate.prior = 1

beta.var.prior.inv = solve(beta.var.prior)

#Update functions
update.beta = function(tau){
  beta.var = solve(tau * t(X) %*% X + beta.var.prior.inv)
  beta.mean = beta.var %*% (tau * t(X) %*% y + beta.var.prior.inv %*% beta.mean.prior)
  
  return(mvrnorm(1, beta.mean, beta.var))
}
update.tau = function(beta){
  errors = y - X %*% beta
  
  tau.shape = 0.5 * N + tau.shape.prior - 1
  tau.rate = 0.5 * sum(errors^2) + tau.rate.prior
  
  return(rgamma(1, tau.shape, tau.rate))
}

#Initialize
beta = rep(0, k)
tau = 1

#Burn-in
for (i in 1:burn.iter){
  beta = update.beta(tau)
  tau = update.tau(beta)
}

#Sample
beta.sample = matrix(NA, nrow = k, ncol = sample.iter)
tau.sample = rep(NA, sample.iter)

for (i in 1:sample.iter){
  beta = update.beta(tau)
  tau = update.tau(beta)
  
  beta.sample[,i] = beta
  tau.sample[i] = tau
}

#Plot
# for (j in 1:k){
#   plot(beta.sample[j,], type = "l")
# }
# plot(tau.sample, type = "l")

#Mean estimates
beta.star = apply(beta.sample, 1, mean)
tau.star = mean(tau)

# beta.star
# 1 / sqrt(tau.star)

# ##############################
#Marginal likelihood estimation
#p(beta.star|y); must marginalize out tau
beta.star.var = lapply(1:sample.iter, function(i) solve(tau.sample[i] * t(X) %*% X + beta.var.prior.inv))
beta.star.mean = lapply(1:sample.iter, function(i) beta.star.var[[i]] %*% (tau.sample[i] * t(X) %*% y + beta.var.prior.inv %*% beta.mean.prior))

log.posterior.beta = log(mean(sapply(1:sample.iter, function(i) dmvnorm(beta.star, beta.star.mean[[i]], beta.star.var[[i]]))))

#p(tau.star|y, beta.star)
errors.star = y - X %*% beta.star
tau.star.shape = 0.5 * N + tau.shape.prior - 1
tau.star.rate = 0.5 * sum(errors.star^2) + tau.rate.prior
log.posterior.tau.beta = dgamma(tau.star, tau.star.shape, tau.star.rate, log = TRUE)

#p(y|beta.star, tau.star)
log.lik.star = sum(dnorm(y, X %*% beta.star, 1 / sqrt(tau.star), log = TRUE))

#p(beta.star)
log.prior.beta.star = dmvnorm(beta.star, beta.mean.prior, beta.var.prior, log = TRUE)

#p(tau)
log.prior.tau.star = dgamma(tau.star, tau.shape.prior, tau.rate.prior, log = TRUE)

#Marginal result
log.marginal = log.lik.star + log.prior.beta.star + log.prior.tau.star - log.posterior.beta - log.posterior.tau.beta

log.marginal

log.lik.star
log.prior.beta.star + log.prior.tau.star
log.posterior.beta + log.posterior.tau.beta

log.posterior.tau.beta
log.posterior.beta
# ##############################

##############################
#Marginal likelihood estimation
#p(tau.star|y); must marginalize out beta
errors = lapply(1:sample.iter, function(i) y - X %*% beta.sample[,i])
tau.star.shape = lapply(1:sample.iter, function(i) 0.5 * N + tau.shape.prior - 1)
tau.star.rate = lapply(1:sample.iter, function(i) 0.5 * sum(errors[[i]]^2) + tau.rate.prior)

log.posterior.tau = log(mean(sapply(1:sample.iter, function(i) dgamma(tau.star, tau.star.shape[[i]], tau.star.rate[[i]]))))

#p(beta.star|y, tau.star)
beta.star.var = solve(tau.star * t(X) %*% X + beta.var.prior.inv)
beta.star.mean = beta.star.var %*% (tau.star * t(X) %*% y + beta.var.prior.inv %*% beta.mean.prior)

log.posterior.beta.tau = dmvnorm(beta.star, beta.star.mean, beta.star.var, log = TRUE)

#p(y|beta.star, tau.star)
log.lik.star = sum(dnorm(y, X %*% beta.star, 1 / sqrt(tau.star), log = TRUE))

#p(beta.star)
log.prior.beta.star = dmvnorm(beta.star, beta.mean.prior, beta.var.prior, log = TRUE)

#p(tau)
log.prior.tau.star = dgamma(tau.star, tau.shape.prior, tau.rate.prior, log = TRUE)

#Marginal result
log.marginal = log.lik.star + log.prior.beta.star + log.prior.tau.star - log.posterior.beta.tau - log.posterior.tau

log.marginal

log.lik.star
log.prior.beta.star + log.prior.tau.star
log.posterior.beta.tau + log.posterior.tau

log.posterior.tau
log.posterior.beta.tau
##############################
