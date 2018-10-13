library(rstan)
library(quantreg)
library(mvtnorm)
library(invgamma)

setwd("~/git/Iterative_ML/R/gibbs_stan_bridge_lm")

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
stan.extract = extract(stan.fit)

################
#Bridge sampling
log.prior = function(beta, sigma.sq){
  result = sum(dnorm(beta, mu.beta, sd.beta, log = TRUE)) +
    dinvgamma(sigma.sq, tau.shape, tau.rate, log = TRUE)
  
  return(result)
}
log.lik = function(beta, sigma.sq){
  result = sum(dnorm(y, X %*% beta, sqrt(sigma.sq), log = TRUE))
  
  return(result)
}

#Setting up proposal distribution
beta.post.mean = apply(stan.extract$beta, 2, mean)
beta.post.cov = cov(stan.extract$beta)

log.sigma.sq.post.mean = mean(log(stan.extract$sigma_sq))
log.sigma.sq.post.sd = sd(log(stan.extract$sigma_sq))

log.dg = function(beta, sigma.sq){
  result = dmvnorm(beta, beta.post.mean, beta.post.cov, log = TRUE) +
    dlnorm(sigma.sq, log.sigma.sq.post.mean, log.sigma.sq.post.sd, log = TRUE)
  
  return(result)
}
rg = function(n){
  r.beta = rmvnorm(n, beta.post.mean, beta.post.cov)
  r.sigma.sq = rlnorm(n, log.sigma.sq.post.mean, log.sigma.sq.post.sd)
  
  return(list(beta = r.beta, sigma.sq = r.sigma.sq))
}

#Bridge function
h.inv = function(beta, sigma.sq, N.1, N.2, marginal.lik){
  s.1 = N.1 / (N.1 + N.2)
  s.2 = N.2 / (N.1 + N.2)
  
  return(s.1 * exp(log.lik(beta, sigma.sq)) * exp(log.prior(beta, sigma.sq)) + 
           s.2 * marginal.lik * exp(log.dg(beta, sigma.sq)))
}

#Iterating
#Parameters:
tol = 1e-10
log.error = Inf

N.1 = sample.iter
N.2 = N.1

log.marginal.lik = 1

#Packaging things up
beta.post = stan.extract$beta
sigma.sq.post = stan.extract$sigma_sq

prop = rg(N.2)
beta.prop = prop$beta
sigma.sq.prop = prop$sigma.sq

log.lik.post = sapply(1:N.1, function(i) log.lik(beta.post[i,], sigma.sq.post[i]))
log.prior.post = sapply(1:N.1, function(i) log.prior(beta.post[i,], sigma.sq.post[i]))
log.dg.post = sapply(1:N.1, function(i) log.dg(beta.post[i,], sigma.sq.post[i]))
lik.post = exp(log.lik.post)
prior.post = exp(log.prior.post)
dg.post = exp(log.dg.post)

log.lik.prop = sapply(1:N.2, function(i) log.lik(beta.prop[i,], sigma.sq.prop[i]))
log.prior.prop = sapply(1:N.2, function(i) log.prior(beta.prop[i,], sigma.sq.prop[i]))
log.dg.prop = sapply(1:N.1, function(i) log.dg(beta.prop[i,], sigma.sq.prop[i]))
lik.prop = exp(log.lik.prop)
prior.prop = exp(log.prior.prop)
dg.prop = exp(log.dg.prop)

s.1 = N.1 / (N.1 + N.2)
s.2 = N.2 / (N.1 + N.2)

while (log.error > tol){
  # numerator = sum(exp(log.lik.prop + log.prior.prop) / (s.1 * lik.prop * prior.prop + s.2 * exp(log.marginal.lik) * dg.prop)) * N.1
  # denominator = sum(dg.post / (s.1 * lik.post * prior.post + s.2 * exp(log.marginal.lik) * dg.post)) * N.2
  
  numerator = sum(exp(log.lik.prop + log.prior.prop - log(s.1 * lik.prop * prior.prop + s.2 * exp(log.marginal.lik) * dg.prop))) * N.1
  denominator = sum(exp(log.dg.post - log(s.1 * lik.post * prior.post + s.2 * exp(log.marginal.lik) * dg.post))) * N.2
  
  log.marginal.lik.new = log(N.1) + log(numerator) - log(N.2) - log(denominator)
  
  log.error = abs(log.marginal.lik - log.marginal.lik.new)
  log.marginal.lik = log.marginal.lik.new
}

log.marginal = log.marginal.lik
