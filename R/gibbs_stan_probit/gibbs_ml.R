setwd("~/git/Iterative_ML/R/gibbs_stan_probit")

# rm(list = ls())

#Load data
load("probitData.RData")

#Priors
mu.beta = rep(0, ncol(X))
Sigma.beta.inv = solve(diag(rep(100, ncol(X))))

#MCMC parameters:
burn.iter = 5000
sample.iter = 50000

#Run model
source("probit.R")

#Trace plots:
# for (i in 1:length(beta)){
#   plot(beta.posterior[i,], type = "l")
# }

#Summary stats:
apply(beta.posterior, 1, mean)
log.marginal

# [1] -30.58349
# [1] -30.17859
