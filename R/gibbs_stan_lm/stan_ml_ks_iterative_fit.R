library(rstan)
library(quantreg)

setwd("~/docs/Iterative_ML_paper/R/gibbs_stan_lm")

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

#Maximum parameters to perform kde at one time
max.kde.params = 1
if (max.kde.params != 1) stop("quantreg::akj can only use one parameter at a time.")

num.runs = ceiling(length(mu.beta) / max.kde.params)
parameter.partition = matrix(sapply(1:num.runs, function(run) ((run - 1) * max.kde.params + 1):(run * max.kde.params)), ncol = num.runs)
parameter.partition[parameter.partition > length(mu.beta)] = NA

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

# unrestricted.stan.dso = stan.fit
# save(unrestricted.stan.dso, file = "unrestricted.stan.dso.RData")

stan.extract = extract(stan.fit, permuted = TRUE)

#Trace plots:
# traceplot(stan.fit)

#Parameter estimates
beta.star = apply(stan.extract$beta, 2, mean)
tau.star = mean(1 / stan.extract$sigma_sq)

#Evaluate unconditional densities at sigma.sq.star
kde.result = akj(1 / stan.extract$sigma_sq, z = tau.star)
log.density.unconditional = log(kde.result$dens)



################
#Restricted models:
log.density.conditional = rep(NA, num.runs)
for (run in 1:(num.runs - 1)){
  parameters = parameter.partition[,run]
  free.parameters = c(parameter.partition[,1:run])
  # parameters = parameters[!is.na(parameters)]
  
  num.free.params = length(free.parameters)
  num.restricted.params = length(mu.beta) - num.free.params
  
  if (num.free.params == 1){
    stan.file = "stan_model_beta_restricted_singlefree.stan"
    load("beta.restricted.singlefree.stan.dso.RData")
  } else{
    stan.file = "stan_model_beta_restricted.stan"
    load("beta.restricted.stan.dso.RData")
  }
  
  model.data = list(N = nrow(X), k = ncol(X), num_free_params = num.free.params, num_restricted_params = num.restricted.params,
                    y = c(y), X = X[,free.parameters,drop=FALSE], X_restricted = X[,-free.parameters,drop=FALSE],
                    beta_restricted = as.array(beta.star[-free.parameters]), sigma_sq_restricted = 1 / tau.star,
                    mu_beta = as.array(mu.beta[free.parameters]), sd_beta = as.array(sd.beta[free.parameters]))
  stan.fit = stan(stan.file,
                  data = model.data,
                  chains = 1,
                  fit = restricted.stan.dso,
                  init = list(list(beta = beta.star[free.parameters])),
                  warmup = burn.iter, iter = burn.iter + sample.iter)
  
  # restricted.stan.dso = stan.fit
  # save(restricted.stan.dso, file = "beta.restricted.stan.dso.RData")
  # save(restricted.stan.dso, file = "beta.restricted.singlefree.stan.dso.RData")
  
  stan.extract = extract(stan.fit, permuted = TRUE)
  
  #Trace plots:
  # traceplot(stan.fit)
  
  #Evaluate conditional densities at beta.star
  if (num.free.params == 1){
    kde.result = akj(matrix(stan.extract$beta), z = beta.star[parameters])
  } else{
    kde.result = akj(stan.extract$beta[,parameters], z = beta.star[parameters])
  }
  
  log.density.conditional[run] = log(kde.result$dens)
}

#Finally, restricting only sigma.sq
parameters = parameter.partition[,num.runs]

load("sigma.sq.restricted.stan.dso.RData")

model.data = list(N = nrow(X), k = ncol(X),
                  y = c(y), X = X,
                  sigma_sq_restricted = 1 / tau.star,
                  mu_beta = as.array(mu.beta), sd_beta = as.array(sd.beta))
stan.fit = stan("stan_model_sigma_sq_restricted.stan",
                data = model.data,
                chains = 1,
                fit = restricted.stan.dso,
                warmup = burn.iter, iter = burn.iter + sample.iter)

# restricted.stan.dso = stan.fit
# save(restricted.stan.dso, file = "sigma.sq.restricted.stan.dso.RData")

stan.extract = extract(stan.fit, permuted = TRUE)

kde.result = akj(stan.extract$beta[,parameters], z = beta.star[parameters])

log.density.conditional[num.runs] = log(kde.result$dens)

################
#Marginal likelihood:
#Prior/likelihood functions:
log.lik = function(beta, tau){
  return(sum(dnorm(y, X %*% beta, sqrt(1 / tau), log = TRUE)))
}
log.prior = function(beta, tau){
  return(sum(dnorm(beta, mu.beta, sd.beta, log = TRUE)) +
           dgamma(tau, tau.shape, tau.rate, log = TRUE))
}

log.posterior.star = log.density.unconditional + sum(log.density.conditional)
log.lik.star = log.lik(beta.star, tau.star)
log.prior.star = log.prior(beta.star, tau.star)

log.marginal = log.lik.star + log.prior.star - log.posterior.star

log.marginal

log.lik.star
log.prior.star
log.posterior.star

log.density.unconditional
sum(log.density.conditional)
