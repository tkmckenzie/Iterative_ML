library(ggplot2)
library(rstan)
library(quantreg)

setwd("~/code/R/marginal_likelihood/stan_sf_data")

rm(list = ls())

load("data.RData")

k = ncol(X)

##############################
#Posterior estimates

#Unconditional
load("stan_par_fits/stan_par_sf_unconditional.RData")
load("stan_par_fits/restricted_params.RData")
stan.extract = extract(stan.fit)
traceplot(stan.fit)
readline("Press Enter to Continue.")

log.unconditional.posterior = log(akj(stan.extract$beta[,1], beta.restricted[1])$dens)

#Conditional
log.conditional.posterior = rep(NA, k + 2)

#partial beta
for (k.restricted in 1:(k-1)){
  load(sprintf("stan_par_fits/stan_par_sf_restr_beta_partial_%i.RData", k.restricted))
  stan.extract = extract(stan.fit)
  show(traceplot(stan.fit))
  readline("Press Enter to Continue.")
  
  log.conditional.posterior[k.restricted] = log(akj(stan.extract$beta_free[,1], beta.restricted[k.restricted + 1])$dens)
}

#beta (not including beta_const)
load("stan_par_fits/stan_par_sf_restr_beta.RData")
stan.extract = extract(stan.fit)
traceplot(stan.fit)
readline("Press Enter to Continue.")

log.conditional.posterior[k] = log(akj(stan.extract$beta_const, beta.const.restricted)$dens)

#beta and beta_const
load("stan_par_fits/stan_par_sf_restr_beta_beta_const.RData")
stan.extract = extract(stan.fit)
traceplot(stan.fit)
readline("Press Enter to Continue.")

log.conditional.posterior[k + 1] = log(akj(stan.extract$sigma_v, sigma.v.restricted)$dens)

#beta, beta_const, sigma_u
load("stan_par_fits/stan_par_sf_restr_beta_beta_const_sigma_v.RData")
stan.extract = extract(stan.fit)
traceplot(stan.fit)
readline("Press Enter to Continue.")

log.conditional.posterior[k + 2] = log(akj(stan.extract$sigma_u, sigma.u.restricted)$dens)

log.posterior = log.unconditional.posterior + sum(log.conditional.posterior)

##############################
#Prior evaluation
log.prior = dnorm(beta.const.restricted, sd = 10, log = TRUE) +
  sum(dnorm(beta.restricted, sd = 1, log = TRUE)) +
  dnorm(sigma.u.restricted, sd = 1, log = TRUE) +
  dnorm(sigma.v.restricted, sd = 1, log = TRUE)

##############################
#Likelihood evaluation
log.lik.func = function(epsilon, sigma.u, sigma.v){
  sigma.sq = sigma.u^2 + sigma.v^2
  sigma = sqrt(sigma.sq)
  lambda = sigma.u / sigma.v
  
  return(log(2) - log(sigma) + dnorm(epsilon / sigma, log = TRUE) + pnorm(epsilon * lambda / sigma, log = TRUE, lower.tail = FALSE))
}

epsilon = c(y - (beta.const.restricted + X %*% beta.restricted))
log.lik = sum(sapply(epsilon, log.lik.func, sigma.u = sigma.u.restricted, sigma.v = sigma.v.restricted))

##############################
#Full marginal likelihood
log.lik
log.prior
log.posterior

log.lik + log.prior - log.posterior
