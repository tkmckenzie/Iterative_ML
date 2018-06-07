library(ggplot2)
library(rstan)

setwd("~/code/R/marginal_likelihood/stan_sf_data")

rm(list = ls())

load("data.RData")

#Transform data for translog
combos = combn(ncol(X), 2)
X = cbind(X, X^2,
          sapply(1:ncol(combos), function(i) X[,combos[1,i]] * X[,combos[2,i]]))

#MCMC parameters
burn.iter = 50000
sample.iter = 2500

adapt.delta = 0.9

# burn.iter = 1
# sample.iter = 1

# load("stan_par_translog_fits/restricted_params.RData")

#Unconditional run:
load("stan_par_sf.dso")
stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y)
stan.fit = stan("stan_par_sf.stan", data = stan.data,
                control = list(adapt_delta = adapt.delta),
                fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_par_sf.dso")
save(stan.fit, file = "stan_par_translog_fits/stan_par_sf_unconditional.RData")

#Restricted values:
stan.extract = extract(stan.fit)

sigma.u.restricted = mean(stan.extract$sigma_u)
sigma.v.restricted = mean(stan.extract$sigma_v)
beta.restricted = apply(stan.extract$beta, 2, mean)
beta.const.restricted = mean(stan.extract$beta_const)

save(sigma.u.restricted, sigma.v.restricted, beta.restricted, beta.const.restricted,
     file = "stan_par_translog_fits/restricted_params.RData")

#Conditional runs:
#partial beta
k.restricted = 2
for (k.restricted in 1:(ncol(X) - 1)){
  load("stan_par_sf_restr_beta_partial.dso")
  stan.data = list(N = nrow(X),
                   k_free = ncol(X) - k.restricted, k_restricted = k.restricted,
                   X_free = X[,-(1:k.restricted),drop = FALSE],
                   X_restricted = X[,1:k.restricted,drop = FALSE],
                   y = y,
                   beta_restricted = as.array(beta.restricted[1:k.restricted]))
  stan.fit = stan("stan_par_sf_restr_beta_partial.stan", data = stan.data,
                  control = list(adapt_delta = adapt.delta),
                  fit = stan.fit,
                  chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

  # save(stan.fit, file = "stan_par_sf_restr_beta_partial.dso")
  save(stan.fit, file = sprintf("stan_par_translog_fits/stan_par_sf_restr_beta_partial_%i.RData", k.restricted))
}

#full beta (not including beta_const)
load("stan_par_sf_restr_beta.dso")
stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 beta = beta.restricted)
stan.fit = stan("stan_par_sf_restr_beta.stan", data = stan.data,
                control = list(adapt_delta = adapt.delta),
                fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_par_sf_restr_beta.dso")
save(stan.fit, file = "stan_par_translog_fits/stan_par_sf_restr_beta.RData")

#beta and beta_const
load("stan_par_sf_restr_beta_beta_const.dso")
stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 beta = beta.restricted,
                 beta_const = beta.const.restricted)
stan.fit = stan("stan_par_sf_restr_beta_beta_const.stan", data = stan.data,
                control = list(adapt_delta = adapt.delta),
                fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_par_sf_restr_beta_beta_const.dso")
save(stan.fit, file = "stan_par_translog_fits/stan_par_sf_restr_beta_beta_const.RData")

#beta, beta_const, sigma_v
load("stan_par_sf_restr_beta_beta_const_sigma_v.dso")
stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 beta = beta.restricted,
                 beta_const = beta.const.restricted,
                 sigma_v = sigma.v.restricted)
stan.fit = stan("stan_par_sf_restr_beta_beta_const_sigma_v.stan", data = stan.data,
                control = list(adapt_delta = adapt.delta),
                fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_par_sf_restr_beta_beta_const_sigma_v.dso")
save(stan.fit, file = "stan_par_translog_fits/stan_par_sf_restr_beta_beta_const_sigma_v.RData")
