library(ggplot2)
library(rstan)
library(quantreg)

setwd("~/git/Iterative_ML/R/stan_sf_data")

rm(list = ls())

show.trace = FALSE

load("data.RData")

N = nrow(X)
k = ncol(X)

##############################
#Posterior estimates

#Unconditional
load("stan_gp_fits/stan_gp_sf_unconditional.RData")
load("stan_gp_fits/restricted_params.RData")
stan.extract = extract(stan.fit)
if (show.trace){
  show(traceplot(stan.fit))
  readline("Press Enter to Continue.")
}

log.unconditional.posterior = log(akj(stan.extract$sigma_u, sigma.u.restricted)$dens)

#Conditional
log.conditional.posterior = rep(NA, N + k + 2)

#sigma.u
load("stan_gp_fits/stan_gp_sf_restr_sigma_u.RData")
stan.extract = extract(stan.fit)
if (show.trace){
  show(traceplot(stan.fit))
  readline("Press Enter to Continue.")
}

log.conditional.posterior[1] = log(akj(stan.extract$sigma_v, sigma.v.restricted)$dens)

#sigma.u, sigma.v
load("stan_gp_fits/stan_gp_sf_restr_sigma_uv.RData")
stan.extract = extract(stan.fit)
if (show.trace){
  show(traceplot(stan.fit))
  readline("Press Enter to Continue.")
}

log.conditional.posterior[2] = log(akj(stan.extract$H_inv_diag[,1], H.inv.diag.restricted[1])$dens)

#sigma.u, sigma.v, partial H.inv
for (k.restricted in 1:(k - 1)){
  load(sprintf("stan_gp_fits/stan_gp_sf_restr_sigma_uv_H_inv_partial_%i.RData", k.restricted))
  stan.extract = extract(stan.fit)
  if (show.trace){
    show(traceplot(stan.fit))
    readline("Press Enter to Continue.")
  }
  
  if (k.restricted < (k - 1)){
    log.conditional.posterior[k.restricted + 2] = log(akj(stan.extract$H_inv_diag_free[,1], H.inv.diag.restricted[k.restricted + 1])$dens)
  } else{
    log.conditional.posterior[k.restricted + 2] = log(akj(stan.extract$H_inv_diag_free, H.inv.diag.restricted[k.restricted + 1])$dens)
  }
}

#sigma.u, sigma.v, H.inv
load("stan_gp_fits/stan_gp_sf_restr_sigma_uv_H_inv.RData")
stan.extract = extract(stan.fit)
if (show.trace){
  show(traceplot(stan.fit))
  readline("Press Enter to Continue.")
}

log.conditional.posterior[k + 2] = log(akj(stan.extract$eta[,1], eta.restricted[1])$dens)

#sigma.u, sigma.v, H.inv, partial eta
for (N.restricted in 1:(N - 1)){
  load(sprintf("stan_gp_fits/stan_gp_sf_restr_sigma_uv_H_inv_eta_partial_%i.RData", N.restricted))
  stan.extract = extract(stan.fit)
  if (show.trace){
    show(traceplot(stan.fit))
    readline("Press Enter to Continue.")
  }
  
  if (N.restricted < (N - 1)){
    log.conditional.posterior[N.restricted + k + 2] = log(akj(stan.extract$eta_free[,1], eta.restricted[N.restricted + 1])$dens)
  } else{
    log.conditional.posterior[N.restricted + k + 2] = log(akj(stan.extract$eta_free, eta.restricted[N.restricted + 1])$dens)
  }
}

#sigma.u, sigma.v, H.inv, eta
load("stan_gp_fits/stan_gp_sf_restr_sigma_uv_H_inv_eta.RData")
stan.extract = extract(stan.fit)
if (show.trace){
  show(traceplot(stan.fit))
  readline("Press Enter to Continue.")
}

log.conditional.posterior[N + k + 2] = log(akj(stan.extract$alpha, alpha.restricted)$dens)

log.posterior = log.unconditional.posterior + sum(log.conditional.posterior)

##############################
#Prior evaluation
log.prior = dnorm(alpha.restricted, sd = 1, log = TRUE) +
  sum(dgamma(H.inv.diag.restricted, shape = 1, rate = 0.1, log = TRUE)) +
  dnorm(sigma.u.restricted, sd = 1, log = TRUE) +
  dnorm(sigma.v.restricted, sd = 1, log = TRUE) +
  sum(dnorm(eta.restricted, sd = 1, log = TRUE))

##############################
#Likelihood evaluation
log.lik.func = function(epsilon, sigma.u, sigma.v){
  sigma.sq = sigma.u^2 + sigma.v^2
  sigma = sqrt(sigma.sq)
  lambda = sigma.u / sigma.v
  
  return(length(epsilon) * (log(2) - log(sigma)) + dnorm(epsilon / sigma, log = TRUE) + pnorm(epsilon * lambda / sigma, log = TRUE, lower.tail = FALSE))
}

normal.kernel = function(x, alpha, H.inv){
  return(alpha^2 * exp(-0.5 * t(x) %*% H.inv %*% x))
}
normal.cov = function(alpha, H.inv){
  result = matrix(NA, nrow = N, ncol = N)
  for (i in 1:(N - 1)){
    result[i, i] = alpha^2
    
    for (j in (i + 1):N){
      temp.result = normal.kernel(X[i,] - X[j,], alpha, H.inv)
      result[i, j] = temp.result
      result[j, i] = temp.result
    }
  }
  result[N, N] = alpha^2
  
  return(result)
}

cov = normal.cov(alpha.restricted, diag(H.inv.diag.restricted))
L = t(chol(cov))
f = L %*% eta.restricted

epsilon = c(y - f)
log.lik = sum(sapply(epsilon, log.lik.func, sigma.u = sigma.u.restricted, sigma.v = sigma.v.restricted))

##############################
#Full marginal likelihood
log.lik
log.prior
log.posterior

log.lik + log.prior - log.posterior
