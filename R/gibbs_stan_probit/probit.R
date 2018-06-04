library(MASS)
library(truncnorm)
library(mvtnorm)

#Requires previously defined variables:
#y, X
#mu.beta, Sigma.beta.inv
#burn.iter, sample.iter

#Sampling functions
sample.beta = function(z){
  Sigma = solve(t(X) %*% X + Sigma.beta.inv)
  mu = Sigma %*% (t(X) %*% z + Sigma.beta.inv %*% mu.beta)
  return(mvrnorm(1, mu, Sigma))
}
sample.z = function(beta){
  mu = X %*% beta
  lower.bound = ifelse(y == 1, 0, -Inf)
  upper.bound = ifelse(y == 1, Inf, 0)
  return(rtruncnorm(1, a = lower.bound, b = upper.bound, mean = mu, sd = 1))
}

calculate.posterior = function(beta.star, i){
  Sigma = solve(t(X) %*% X + Sigma.beta.inv)
  mu = Sigma %*% (t(X) %*% z.posterior[,i] + Sigma.beta.inv %*% mu.beta)
  return(dmvnorm(beta.star, mu, Sigma, log = FALSE))
}
calculate.log.lik = function(beta.star){
  log.cumulative.normal.lower = pnorm(X %*% beta.star, log = TRUE)
  log.cumulative.normal.upper = pnorm(X %*% beta.star, lower.tail = FALSE, log = TRUE)
  return(sum(y * log.cumulative.normal.lower + (1 - y) * log.cumulative.normal.upper))
}

#Initialization:
m = glm(y ~ 0 + X, binomial(link = "probit"))
# beta = m$coefficients
beta = rep(0, ncol(X))

#Burn-in:
for (i in 1:burn.iter){
  z = sample.z(beta)
  beta = sample.beta(z)
}

#Sampling:
beta.posterior = matrix(NA, nrow = ncol(X), ncol = sample.iter)
z.posterior = matrix(NA, nrow = N, ncol = sample.iter)
for (i in 1:sample.iter){
  z = sample.z(beta)
  beta = sample.beta(z)
  
  beta.posterior[,i] = beta
  z.posterior[,i] = z
}

#Bayes' factor:
beta.star = apply(beta.posterior, 1, mean)
posterior = rep(NA, sample.iter)
for (i in 1:sample.iter){
  posterior[i] = calculate.posterior(beta.star, i)
}

log.posterior.estimate = log(mean(posterior))
log.prior = dmvnorm(beta.star, mu.beta, solve(Sigma.beta.inv), log = TRUE)
log.lik = calculate.log.lik(beta.star)

log.marginal = log.lik + log.prior - log.posterior.estimate
