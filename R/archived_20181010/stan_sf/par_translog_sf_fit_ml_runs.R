library(ggplot2)
library(rstan)

setwd("~/git/Iterative_ML/R/stan_sf")

rm(list = ls())

load("data.RData")
# load("stan_par_translog_fits/restricted_params.RData")

#Transform data for translog
combos = combn(ncol(X), 2)
X = cbind(X, X^2,
          sapply(1:ncol(combos), function(i) X[,combos[1,i]] * X[,combos[2,i]]))

#MCMC parameters
burn.iter = 4000
sample.iter = 1000

adapt.delta = 0.9
max.treedepth = 12

# burn.iter = 1
# sample.iter = 1

#Unconditional run:
load("stan_par_sf.dso")
stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y)
stan.fit = stan("stan_par_sf.stan", data = stan.data,
                control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_par_sf.dso")
save(stan.fit, file = "stan_par_translog_fits/stan_par_sf_unconditional.RData")

traceplot(stan.fit)
if (readline("Enter y to accept initial sample: ") != "y") stop("Initial sample rejected.")

#Restricted values:
stan.extract = extract(stan.fit)

sigma.u.restricted = mean(stan.extract$sigma_u)
sigma.v.restricted = mean(stan.extract$sigma_v)
beta.restricted = apply(stan.extract$beta, 2, mean)
beta.const.restricted = mean(stan.extract$beta_const)

save(sigma.u.restricted, sigma.v.restricted, beta.restricted, beta.const.restricted,
     file = "stan_par_translog_fits/restricted_params.RData")

#Conditional runs:
#sigma.u
load("stan_par_sf_restr_sigma_u.dso")
stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 sigma_u = sigma.u.restricted)
stan.fit = stan("stan_par_sf_restr_sigma_u.stan", data = stan.data,
                control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                fit = stan.fit,
                init = list(list(beta_const = beta.const.restricted,
                                 beta = beta.restricted,
                                 sigma_v = sigma.v.restricted)),
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_par_sf_restr_sigma_u.dso")
save(stan.fit, file = "stan_par_translog_fits/stan_par_sf_restr_sigma_u.RData")

#sigma.u, sigma.v
load("stan_par_sf_restr_sigma_uv.dso")
stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 sigma_u = sigma.u.restricted,
                 sigma_v = sigma.v.restricted)
stan.fit = stan("stan_par_sf_restr_sigma_uv.stan", data = stan.data,
                control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                fit = stan.fit,
                init = list(list(beta_const = beta.const.restricted,
                                 beta = beta.restricted)),
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_par_sf_restr_sigma_uv.dso")
save(stan.fit, file = "stan_par_translog_fits/stan_par_sf_restr_sigma_uv.RData")

#sigma.u, sigma.v, partial beta
#sigma.u, sigma.v, partial beta
for (k.restricted in 1:(ncol(X) - 1)){
  if (k.restricted < (ncol(X) - 1)){
    load("stan_par_sf_restr_sigma_uv_beta_partial.dso")
    stan.data = list(N = nrow(X),
                     k_restricted = k.restricted, k_free = ncol(X) - k.restricted,
                     X = X, y = y,
                     sigma_u = sigma.u.restricted,
                     sigma_v = sigma.v.restricted,
                     beta_restricted = as.array(beta.restricted[1:k.restricted]))
    stan.fit = stan("stan_par_sf_restr_sigma_uv_beta_partial.stan", data = stan.data,
                    control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                    fit = stan.fit,
                    init = list(list(beta_const = beta.const.restricted,
                                     beta_free = beta.restricted[-(1:k.restricted)])),
                    chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
    
    # save(stan.fit, file = "stan_par_sf_restr_sigma_uv_beta_partial.dso")
    save(stan.fit, file = sprintf("stan_par_translog_fits/stan_par_sf_restr_sigma_uv_beta_partial_%i.RData", k.restricted))
  } else{
    load("stan_par_sf_restr_sigma_uv_beta_partial_singlefree.dso")
    stan.data = list(N = nrow(X),
                     k_restricted = k.restricted, k_free = ncol(X) - k.restricted,
                     X = X, y = y,
                     sigma_u = sigma.u.restricted,
                     sigma_v = sigma.v.restricted,
                     beta_restricted = as.array(beta.restricted[1:k.restricted]))
    stan.fit = stan("stan_par_sf_restr_sigma_uv_beta_partial_singlefree.stan", data = stan.data,
                    control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                    fit = stan.fit,
                    init = list(list(beta_const = beta.const.restricted,
                                     beta_free = beta.restricted[-(1:k.restricted)])),
                    chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
    
    # save(stan.fit, file = "stan_par_sf_restr_sigma_uv_beta_partial_singlefree.dso")
    save(stan.fit, file = sprintf("stan_par_translog_fits/stan_par_sf_restr_sigma_uv_beta_partial_%i.RData", k.restricted))
  }
}

#sigma.u, sigma.v, beta (not including beta_const)
load("stan_par_sf_restr_sigma_uv_beta.dso")
stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 sigma_u = sigma.u.restricted,
                 sigma_v = sigma.v.restricted,
                 beta = beta.restricted)
stan.fit = stan("stan_par_sf_restr_sigma_uv_beta.stan", data = stan.data,
                control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                fit = stan.fit,
                init = list(list(beta_const = beta.const.restricted)),
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_par_sf_restr_sigma_uv_beta.dso")
save(stan.fit, file = "stan_par_translog_fits/stan_par_sf_restr_sigma_uv_beta.RData")
