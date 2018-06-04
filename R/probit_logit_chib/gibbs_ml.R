setwd("~/code/R/marginal_likelihood/probit_logit_chib")

rm(list = ls())

#Read, select data
df = read.csv("data.csv")

y = df$y

# X = cbind(rep(1, length(y)))
# X = cbind(rep(1, length(y)), df$X.1)
# X = cbind(rep(1, length(y)), log(df$X.2))
# X = cbind(rep(1, length(y)), df$X.3)
# X = cbind(rep(1, length(y)), df$X.4)
# X = cbind(rep(1, length(y)), df$X.5)
# X = cbind(rep(1, length(y)), log(df$X.2), df$X.4)
X = cbind(rep(1, length(y)), log(df$X.2), df$X.3, df$X.4)
# X = cbind(rep(1, length(y)), log(df$X.2), df$X.3, df$X.4, df$X.5)

N = nrow(X)

#Priors
mu.beta = rep(0.75, ncol(X))
Sigma.beta.inv = solve(25 * diag(ncol(X)))

#MCMC parameters:
burn.iter = 500
sample.iter = 5000

#Run model
source("probit.R")

#Trace plots:
for (i in 1:length(beta)){
  plot(beta.posterior[i,], type = "l")
}

#Summary stats:
apply(beta.posterior, 1, mean)
log.marginal
