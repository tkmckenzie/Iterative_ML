library(ggplot2)
library(rstan)

setwd("~/git/Iterative_ML/R/stan_sf_data")

rm(list = ls())

load("data.RData")

burn.iter = 19000
sample.iter = 1000

# burn.iter = 1
# sample.iter = 1

load("stan_par_sf.dso")

stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 sigma_u_scale = 0.1, sigma_v_scale = 5)
stan.fit = stan("stan_par_sf.stan", data = stan.data,
                control = list(adapt_delta = 0.99, max_treedepth = 12),
                # fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
stan.extract = extract(stan.fit)

# save(stan.fit, file = "stan_par_sf.dso")

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
  return(sigma.star * (dnorm(eval.point) / (1 - pnorm(eval.point)) - eval.point))
}

u.posterior = t(sapply(1:sample.iter, u.mode))

# cbind(countries, exp(apply(u.posterior, 2, mean)))

#Other posterior estimates
mean(exp(sapply(1:sample.iter, function(i) -sqrt(2 / pi) * stan.extract$sigma_u[i])))
# mean(exp(-stan.extract$sigma_u))
median(stan.extract$sigma_u)
median(stan.extract$sigma_v)
