setwd("~/code/R/marginal_likelihood/stan_probit_logit")

# rm(list = ls())

#Load data
load("probitData.RData")
# load("logitData.RData")

#Priors
mu.beta = rep(0, ncol(X))
Sigma.beta.inv = solve(diag(rep(100, ncol(X))))

#MCMC parameters:
burn.iter = 10000
sample.iter = 100000

#Run model
source("probit.R")

#Trace plots:
for (i in 1:length(beta)){
  plot(beta.posterior[i,], type = "l")
}

#Summary stats:
apply(beta.posterior, 1, mean)
log.marginal

# [1] -30.58349
# [1] -30.17859
