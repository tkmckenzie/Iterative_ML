library(ggplot2)
library(rstan)

setwd("~/git/Iterative_ML/R/stan_sf_data")

rm(list = ls())

load("data.RData")
# load("stan_gp_fits/restricted_params.RData")

#MCMC parameters
burn.iter = 9000
sample.iter = 1000

adapt.delta = 0.9
max.treedepth = 12

# burn.iter = 1
# sample.iter = 1

#Unconditional run
load("stan_gp_sf.dso")

stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y)
stan.fit = stan("stan_gp_sf.stan", data = stan.data,
                control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_gp_sf.dso")
save(stan.fit, file = "stan_gp_fits/stan_gp_sf_unconditional.RData")

traceplot(stan.fit)
if (readline("Enter y to accept initial sample: ") != "y") stop("Initial sample rejected.")

#Restricted values:
stan.extract = extract(stan.fit)

sigma.u.restricted = mean(stan.extract$sigma_u)
sigma.v.restricted = mean(stan.extract$sigma_v)
H.inv.diag.restricted = apply(stan.extract$H_inv_diag, 2, mean)
alpha.restricted = mean(stan.extract$alpha)
eta.restricted = apply(stan.extract$eta, 2, mean)

save(sigma.u.restricted, sigma.v.restricted, H.inv.diag.restricted, alpha.restricted, eta.restricted,
     file = "stan_gp_fits/restricted_params.RData")

#Conditional runs
#sigma.u
load("stan_gp_sf_restr_sigma_u.dso")

stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 sigma_u = sigma.u.restricted)
stan.fit = stan("stan_gp_sf_restr_sigma_u.stan", data = stan.data,
                control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                init = list(list(alpha = alpha.restricted,
                                 H_inv_diag = H.inv.diag.restricted,
                                 sigma_v = sigma.v.restricted,
                                 eta = eta.restricted)),
                fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_gp_sf_restr_sigma_u.dso")
save(stan.fit, file = "stan_gp_fits/stan_gp_sf_restr_sigma_u.RData")

#sigma.u, sigma.v
load("stan_gp_sf_restr_sigma_uv.dso")

stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 sigma_u = sigma.u.restricted,
                 sigma_v = sigma.v.restricted)
stan.fit = stan("stan_gp_sf_restr_sigma_uv.stan", data = stan.data,
                control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                init = list(list(alpha = alpha.restricted,
                                 H_inv_diag = H.inv.diag.restricted,
                                 eta = eta.restricted)),
                fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_gp_sf_restr_sigma_uv.dso")
save(stan.fit, file = "stan_gp_fits/stan_gp_sf_restr_sigma_uv.RData")

#sigma.u, sigma.v, partial H.inv
for (k.restricted in 1:(ncol(X) - 1)){
  stan.data = list(N = nrow(X),
                   k_restricted = k.restricted,
                   k_free = ncol(X) - k.restricted,
                   X = X, y = y,
                   sigma_u = sigma.u.restricted,
                   sigma_v = sigma.v.restricted,
                   H_inv_diag_restricted = as.array(H.inv.diag.restricted[1:k.restricted]))
  if (k.restricted < (ncol(X) - 1)){
    load("stan_gp_sf_restr_sigma_uv_H_inv_partial.dso")
    stan.fit = stan("stan_gp_sf_restr_sigma_uv_H_inv_partial.stan", data = stan.data,
                    control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                    init = list(list(alpha = alpha.restricted,
                                     H_inv_diag_free = H.inv.diag.restricted[-(1:k.restricted)],
                                     eta = eta.restricted)),
                    fit = stan.fit,
                    chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
    # save(stan.fit, file = "stan_gp_sf_restr_sigma_uv_H_inv_partial.dso")
  } else{
    load("stan_gp_sf_restr_sigma_uv_H_inv_partial_singlefree.dso")
    stan.fit = stan("stan_gp_sf_restr_sigma_uv_H_inv_partial_singlefree.stan", data = stan.data,
                    control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                    init = list(list(alpha = alpha.restricted,
                                     H_inv_diag_free = H.inv.diag.restricted[-(1:k.restricted)],
                                     eta = eta.restricted)),
                    fit = stan.fit,
                    chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
    save(stan.fit, file = "stan_gp_sf_restr_sigma_uv_H_inv_partial_singlefree.dso")
  }
  
  save(stan.fit, file = sprintf("stan_gp_fits/stan_gp_sf_restr_sigma_uv_H_inv_partial_%i.RData", k.restricted))
}

#sigma.u, sigma.v, H.inv
load("stan_gp_sf_restr_sigma_uv_H_inv.dso")

stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 sigma_u = sigma.u.restricted,
                 sigma_v = sigma.v.restricted,
                 H_inv_diag = H.inv.diag.restricted)
stan.fit = stan("stan_gp_sf_restr_sigma_uv_H_inv.stan", data = stan.data,
                control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                init = list(list(alpha = alpha.restricted,
                                 eta = eta.restricted)),
                fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_gp_sf_restr_sigma_uv_H_inv.dso")
save(stan.fit, file = "stan_gp_fits/stan_gp_sf_restr_sigma_uv_H_inv.RData")

#sigma.u, sigma.v, H.inv, partial eta
for (N.restricted in 1:(nrow(X) - 1)){
  stan.data = list(N_free = nrow(X) - N.restricted,
                   N_restricted = N.restricted,
                   k = ncol(X), X = X, y = y,
                   sigma_u = sigma.u.restricted,
                   sigma_v = sigma.v.restricted,
                   H_inv_diag = H.inv.diag.restricted,
                   eta_restricted = as.array(eta.restricted[1:N.restricted]))
  if (N.restricted < (nrow(X) - 1)){
    load("stan_gp_sf_restr_sigma_uv_H_inv_eta_partial.dso")
    stan.fit = stan("stan_gp_sf_restr_sigma_uv_H_inv_eta_partial.stan", data = stan.data,
                    control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                    init = list(list(alpha = alpha.restricted,
                                     eta_free = eta.restricted[-(1:N.restricted)])),
                    fit = stan.fit,
                    chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
    # save(stan.fit, file = "stan_gp_sf_restr_sigma_uv_H_inv_eta_partial.dso")
  } else{
    load("stan_gp_sf_restr_sigma_uv_H_inv_eta_partial_singlefree.dso")
    stan.fit = stan("stan_gp_sf_restr_sigma_uv_H_inv_eta_partial_singlefree.stan", data = stan.data,
                    control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                    init = list(list(alpha = alpha.restricted,
                                     eta_free = eta.restricted[-(1:N.restricted)])),
                    fit = stan.fit,
                    chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
    # save(stan.fit, file = "stan_gp_sf_restr_sigma_uv_H_inv_eta_partial_singlefree.dso")
  }
  
  save(stan.fit, file = sprintf("stan_gp_fits/stan_gp_sf_restr_sigma_uv_H_inv_eta_partial_%i.RData", N.restricted))
}

#sigma.u, sigma.v, H.inv, eta
load("stan_gp_sf_restr_sigma_uv_H_inv_eta.dso")

stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 sigma_u = sigma.u.restricted,
                 sigma_v = sigma.v.restricted,
                 H_inv_diag = H.inv.diag.restricted,
                 eta = eta.restricted)
stan.fit = stan("stan_gp_sf_restr_sigma_uv_H_inv_eta.stan", data = stan.data,
                control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                init = list(list(alpha = alpha.restricted)),
                fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_gp_sf_restr_sigma_uv_H_inv_eta.dso")
save(stan.fit, file = "stan_gp_fits/stan_gp_sf_restr_sigma_uv_H_inv_eta.RData")
