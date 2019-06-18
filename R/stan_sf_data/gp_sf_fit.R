library(ggplot2)
library(rstan)

setwd("~/git/Iterative_ML/R/stan_sf_data")

rm(list = ls())

load("data.RData")

#Priors
alpha_prior_scale = 1
H_inv_diag_prior_scale = 1
sigma_u_prior_scale = 1
sigma_v_prior_scale = 1

y.mean = mean(y)
y = y - y.mean

burn.iter = 9000
sample.iter = 1000

stan.model.file = "stan_gp_sf.stan"
stan.dso.file = gsub(".stan$", ".dso", stan.model.file)

stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 alpha_prior_scale = alpha_prior_scale,
                 H_inv_diag_prior_scale = H_inv_diag_prior_scale,
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
                control = list(adapt_delta = 0.9),
                refresh = floor((burn.iter + sample.iter) / 100),
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

save(stan.fit, file = "stan_gp_fits/stan_gp_sf_unconditional.RData")
load("stan_gp_fits/stan_gp_sf_unconditional.RData")

stan.extract = extract(stan.fit)

#Plot
e = y - apply(stan.extract$f, 2, mean)
plot(e ~ y)

#Trace plot:
traceplot(stan.fit)

#Efficiency estimates
u.mode = function(i){
  epsilon = y - stan.extract$f[i,]
  sigma.u = stan.extract$sigma_u[i]
  sigma.v = stan.extract$sigma_v[i]
  
  sigma.sq = sigma.u^2 + sigma.v^2
  
  raw.epsilon = -epsilon * (sigma.u^2 / sigma.sq)
  
  return(ifelse(raw.epsilon < 0, raw.epsilon, 0))
}
u.mean = function(i){
  epsilon = y - stan.extract$f[i,]
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

cbind(countries, apply(exp(u.posterior), 2, mean))

# cbind(countries, exp(apply(u.posterior, 2, mean)))

#Other posterior estimates
mean(stan.extract$sigma_u)
mean(stan.extract$sigma_v)


##############################
#Likelihood evaluation
N = nrow(X)

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

sigma.u.restricted = mean(stan.extract$sigma_u)
sigma.v.restricted = mean(stan.extract$sigma_v)
H.inv.diag.restricted = apply(stan.extract$H_inv_diag, 2, mean)
alpha.restricted = mean(stan.extract$alpha)
eta.restricted = apply(stan.extract$eta, 2, mean)

cov = normal.cov(alpha.restricted, diag(H.inv.diag.restricted))
L = t(chol(cov))
f = L %*% eta.restricted

epsilon = c(y - f)

log.lik = sum(sapply(epsilon, log.lik.func, sigma.u = sigma.u.restricted, sigma.v = sigma.v.restricted))
log.lik
