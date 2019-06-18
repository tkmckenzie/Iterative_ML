library(ggplot2)
library(rstan)

setwd("~/git/Iterative_ML/R/stan_sf_data")

rm(list = ls())

load("data.RData")

#Transform data for translog
combos = combn(ncol(X), 2)
X = cbind(X, X^2,
          sapply(1:ncol(combos), function(i) X[,combos[1,i]] * X[,combos[2,i]]))

k = ncol(X)

#Priors
sigma_u_prior_scale = 1
sigma_v_prior_scale = 1
beta_const_prior_sd = 10
beta_prior_sd = 1

#MCMC Parameters
burn.iter = 15000
sample.iter = 5000

stan.model.file = "stan_par_sf.stan"
stan.dso.file = gsub(".stan$", ".dso", stan.model.file)

stan.data = list(N = nrow(X),
                 k = ncol(X),
                 X = X,
                 y = y,
                 beta_const_prior_sd = beta_const_prior_sd,
                 beta_prior_sd = beta_prior_sd,
                 sigma_u_prior_scale = sigma_u_prior_scale,
                 sigma_v_prior_scale = sigma_v_prior_scale)

if (!(stan.dso.file %in% list.files())){
  stan.dso = stan(stan.model.file, data = stan.data,
                  chains = 1, iter = 1, warmup = 1, refresh = 0)
  save(stan.dso, file = stan.dso.file)
} else{
  load(stan.dso.file)
}

stan.fit = stan(stan.model.file, data = stan.data,
                control = list(adapt_delta = 0.99, max_treedepth = 12),
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

save(stan.fit, file = "stan_par_translog_fits/stan_par_sf_unconditional.RData")
load("stan_par_translog_fits/stan_par_sf_unconditional.RData")

stan.extract = extract(stan.fit)

#Plot
e = apply(t(sapply(1:sample.iter, function(i) y - (stan.extract$beta_const[i] + X %*% stan.extract$beta[i,]))), 2, mean)
plot(e ~ y)

#Trace plot:
traceplot(stan.fit)

#Efficiency estimates
u.mode = function(i){
  epsilon = y - (stan.extract$beta_const[i] + X %*% stan.extract$beta[i,])
  sigma.u = stan.extract$sigma_u[i]
  sigma.v = stan.extract$sigma_v[i]
  
  sigma.sq = sigma.u^2 + sigma.v^2
  
  raw.epsilon = -epsilon * (sigma.u^2 / sigma.sq)
  
  return(ifelse(raw.epsilon < 0, raw.epsilon, 0))
}
u.mean = function(i){
  epsilon = y - (stan.extract$beta_const[i] + X %*% stan.extract$beta[i,])
  sigma.u = stan.extract$sigma_u[i]
  sigma.v = stan.extract$sigma_v[i]
  
  sigma.sq = sigma.u^2 + sigma.v^2
  sigma = sqrt(sigma.sq)
  
  mu.star = -sigma.u^2 * epsilon / sigma.sq
  sigma.sq.star = sigma.u^2 * sigma.v^2 / sigma.sq
  sigma.star = sqrt(sigma.sq.star)
  
  lambda = sigma.u / sigma.v
  
  eval.point = epsilon * lambda / sigma
  
  # return(mu.star + sigma.star * dnorm(-mu.star / sigma.star) / (1 - pnorm(-mu.star / sigma.star)))
  return(-sigma.star * (dnorm(eval.point) / (1 - pnorm(eval.point)) - eval.point))
}

u.posterior = t(sapply(1:sample.iter, u.mean))
apply(exp(u.posterior), 2, mean)
cbind(countries, apply(exp(u.posterior), 2, mean))

#Other posterior estimates
# mean(exp(sapply(1:sample.iter, function(i) -sqrt(2 / pi) * stan.extract$sigma_u[i])))
# mean(exp(-stan.extract$sigma_u))
median(stan.extract$sigma_u)
median(stan.extract$sigma_v)


##############################
#Likelihood evaluation
N = nrow(X)

log.lik.func = function(epsilon, sigma.u, sigma.v){
  sigma.sq = sigma.u^2 + sigma.v^2
  sigma = sqrt(sigma.sq)
  lambda = sigma.u / sigma.v
  
  return(length(epsilon) * (log(2) - log(sigma)) + dnorm(epsilon / sigma, log = TRUE) + pnorm(epsilon * lambda / sigma, log = TRUE, lower.tail = FALSE))
}

sigma.u.restricted = mean(stan.extract$sigma_u)
sigma.v.restricted = mean(stan.extract$sigma_v)
beta.const.restricted = mean(stan.extract$beta_const)
beta.restricted = apply(stan.extract$beta, 2, mean)

f = beta.const.restricted + X %*% beta.restricted

epsilon = c(y - f)

log.lik = sum(sapply(epsilon, log.lik.func, sigma.u = sigma.u.restricted, sigma.v = sigma.v.restricted))
log.lik
