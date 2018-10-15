library(ggplot2)
library(rstan)
library(quantreg)
library(invgamma)

setwd("~/git/Iterative_ML/R/stan_sf_data")

# rm(list = ls())

show.trace = FALSE

load("data.RData")

k = ncol(X)

##############################
#Posterior estimates

#Unconditional
load("stan_par_fits/stan_par_sf_unconditional.RData")
load("stan_par_fits/restricted_params.RData")
stan.extract = extract(stan.fit)
if (show.trace){
  show(traceplot(stan.fit))
  readline("Press Enter to Continue.")
}

log.unconditional.posterior = log(akj(stan.extract$sigma_u, sigma.u.restricted)$dens)

#Conditional
log.conditional.posterior = rep(NA, k + 2)

#sigma.u
load("stan_par_fits/stan_par_sf_restr_sigma_u.RData")
stan.extract = extract(stan.fit)
if (show.trace){
  show(traceplot(stan.fit))
  readline("Press Enter to Continue.")
}

log.conditional.posterior[1] = log(akj(stan.extract$sigma_v, sigma.v.restricted)$dens)

#sigma.u, sigma.v
load("stan_par_fits/stan_par_sf_restr_sigma_uv.RData")
stan.extract = extract(stan.fit)
if (show.trace){
  show(traceplot(stan.fit))
  readline("Press Enter to Continue.")
}

log.conditional.posterior[2] = log(akj(stan.extract$beta[,1], beta.restricted[1])$dens)

#sigma.u, sigma.v, partial beta
for (k.restricted in 1:(k-1)){
  load(sprintf("stan_par_fits/stan_par_sf_restr_sigma_uv_beta_partial_%i.RData", k.restricted))
  stan.extract = extract(stan.fit)
  if (show.trace){
    show(traceplot(stan.fit))
    readline("Press Enter to Continue.")
  }
  
  if (k.restricted < (k-1)){
    log.conditional.posterior[k.restricted + 2] = log(akj(stan.extract$beta_free[,1], beta.restricted[k.restricted + 1])$dens)
  } else{
    log.conditional.posterior[k.restricted + 2] = log(akj(stan.extract$beta_free, beta.restricted[k.restricted + 1])$dens)
  }
}

#sigma.u, sigma.v, beta (not including beta_const)
load("stan_par_fits/stan_par_sf_restr_sigma_uv_beta.RData")
stan.extract = extract(stan.fit)
if (show.trace){
  show(traceplot(stan.fit))
  readline("Press Enter to Continue.")
}

log.conditional.posterior[k + 2] = log(akj(stan.extract$beta_const, beta.const.restricted)$dens)

log.posterior = log.unconditional.posterior + sum(log.conditional.posterior)

##############################
#Prior evaluation
load("stan_par_fits/priors.RData")
log.prior = dnorm(beta.const.restricted, sd = beta_const_prior_sd, log = TRUE) +
  sum(dnorm(beta.restricted, sd = beta_prior_sd, log = TRUE)) +
  dinvgamma(sigma.u.restricted, shape = sigma_u_prior_shape, rate = sigma_u_prior_rate, log = TRUE) +
  dinvgamma(sigma.v.restricted, shape = sigma_v_prior_shape, rate = sigma_v_prior_rate, log = TRUE)

##############################
#Likelihood evaluation
log.lik.func = function(epsilon, sigma.u, sigma.v){
  sigma.sq = sigma.u^2 + sigma.v^2
  sigma = sqrt(sigma.sq)
  lambda = sigma.u / sigma.v
  
  return(length(epsilon) * (log(2) - log(sigma)) + dnorm(epsilon / sigma, log = TRUE) + pnorm(epsilon * lambda / sigma, log = TRUE, lower.tail = FALSE))
}

epsilon = c(y - (beta.const.restricted + X %*% beta.restricted))
log.lik = sum(sapply(epsilon, log.lik.func, sigma.u = sigma.u.restricted, sigma.v = sigma.v.restricted))

##############################
#Full marginal likelihood
log.lik
log.prior
log.posterior

log.lik + log.prior - log.posterior
