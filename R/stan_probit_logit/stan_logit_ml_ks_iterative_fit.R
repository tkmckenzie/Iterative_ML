library(rstan)
library(ks)
library(quantreg)

setwd("~/code/R/marginal_likelihood/stan_probit_logit")

# rm(list = ls())

# Load data
# data.file = "probitData.RData"
# data.file = "logitData.RData"

load(data.file)

#Priors
mu.beta = rep(0, ncol(X))
sd.beta = sqrt(rep(100, ncol(X)))

#MCMC parameters:
burn.iter = 500
sample.iter = 5000

# burn.iter = 1
# sample.iter = 1

#Maximum parameters to perform kde at one time
max.kde.params = 1

num.runs = ceiling(length(mu.beta) / max.kde.params)
parameter.partition = matrix(sapply(1:num.runs, function(run) ((run - 1) * max.kde.params + 1):(run * max.kde.params)), ncol = num.runs)
parameter.partition[parameter.partition > length(mu.beta)] = NA

################
#Unrestricted model
load("logit.unrestricted.stan.dso.RData")

model.data = list(N = N, k = ncol(X),
                  y = c(y), X = X,
                  mu_beta = mu.beta, sd_beta = sd.beta)
stan.fit = stan("stan_logit_model.stan",
                data = model.data,
                chains = 1,
                fit = unrestricted.stan.dso,
                warmup = burn.iter, iter = burn.iter + sample.iter)

# unrestricted.stan.dso = stan.fit
# save(unrestricted.stan.dso, file = "logit.unrestricted.stan.dso.RData")

stan.extract = extract(stan.fit, permuted = TRUE)

#Trace plots:
# traceplot(stan.fit)

#Parameter estimates
beta.star = apply(stan.extract$beta, 2, mean)
# beta.star = rep(0, length(mu.beta))

#Evaluate unconditional densities at beta.star
parameters = parameter.partition[,num.runs]
parameters = parameters[!is.na(parameters)]

# kde.result = kde(stan.extract$beta[,parameters], eval.points = beta.star[parameters])
# log.density.unconditional = log(kde.result$estimate)

kde.result = akj(stan.extract$beta[,parameters], z = beta.star[parameters])
log.density.unconditional = log(kde.result$dens)


################
#Restricted models:
log.density.conditional = rep(NA, num.runs - 1)
for (run in 1:(num.runs - 1)){
  parameters = parameter.partition[,run]
  free.parameters = c(parameter.partition[,1:run])
  # parameters = parameters[!is.na(parameters)]
  
  num.free.params = length(free.parameters)
  num.restricted.params = length(mu.beta) - num.free.params
  
  if (num.free.params == 1){
    stan.file = "stan_logit_model_iterative_singlefree.stan"
    load("logit.restricted.singlefree.stan.dso.RData")
  } else{
    stan.file = "stan_logit_model_iterative.stan"
    load("logit.restricted.stan.dso.RData")
  }
  
  model.data = list(N = N, num_free_params = num.free.params, num_restricted_params = num.restricted.params,
                    y = c(y), X = X[,free.parameters,drop=FALSE], X_restricted = X[,-free.parameters,drop=FALSE],
                    beta_restricted = as.array(beta.star[-free.parameters]),
                    mu_beta = as.array(mu.beta[free.parameters]), sd_beta = as.array(sd.beta[free.parameters]))
  stan.fit = stan(stan.file,
                  data = model.data,
                  chains = 1,
                  fit = restricted.stan.dso,
                  init = list(list(beta = beta.star[free.parameters])),
                  warmup = burn.iter, iter = burn.iter + sample.iter)
  
  # restricted.stan.dso = stan.fit
  # save(restricted.stan.dso, file = "restricted.stan.dso.RData")
  # save(restricted.stan.dso, file = "logit.restricted.singlefree.stan.dso.RData")
  
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


################
#Marginal likelihood:
#Prior/likelihood functions:
log.sigmoid = function(z) -log(1 + exp(-z))
log.inv.sigmoid = function(z) -z - log(1 + exp(-z))
log.lik = function(beta){
  z = X %*% beta
  prob = log.sigmoid(z)
  prob.inv = log.inv.sigmoid(z)
  
  return(sum(y * prob + (1 - y) * prob.inv))
}
log.prior = function(beta){
  return(sum(dnorm(beta, mu.beta, sd.beta, log = TRUE)))
}

log.posterior.star = log.density.unconditional + sum(log.density.conditional)
log.lik.star = log.lik(beta.star)
log.prior.star = log.prior(beta.star)

log.marginal = log.lik.star + log.prior.star - log.posterior.star

log.marginal

log.prior.star
log.lik.star
log.posterior.star
