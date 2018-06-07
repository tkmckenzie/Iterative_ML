library(ggplot2)
library(rstan)

setwd("~/code/R/marginal_likelihood/stan_sf_data")

rm(list = ls())

load("data.RData")

burn.iter = 9000
sample.iter = 1000

burn.iter = 1
sample.iter = 1

#Unconditional run
load("stan_gp_sf.dso")
stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y)
stan.fit = stan("stan_gp_sf.stan", data = stan.data,
                control = list(adapt_delta = 0.9),
                fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

traceplot(stan.fit)
# if (readline("Enter 1 to accept initial sample.") != "1") stop("Initial sample not accepted.")

# save(stan.fit, file = "stan_gp_sf.dso")
save(stan.fit, file = "stan_gp_fits/stan_gp_sf_unconditional.RData")

#Restricted parameters
stan.extract = extract(stan.fit)

alpha.restricted = mean(stan.extract$alpha)
H.inv.diag.restricted = apply(stan.extract$H_inv_diag, 2, mean)
sigma.u.restricted = mean(stan.extract$sigma_u)
sigma.v.restricted = mean(stan.extract$sigma_v)
eta.restricted = apply(stan.extract$eta, 2, mean)

save(alpha.restricted, H.inv.diag.restricted, sigma.u.restricted, sigma.v.restricted, eta.restricted,
     file = "stan_gp_fits/restricted_params.RData")

asdf
#Conditional runs
#alpha
load("stan_gp_sf_restr_alpha.dso")
stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 alpha = alpha.restricted)
stan.fit = stan("stan_gp_sf_restr_alpha.stan", data = stan.data,
                control = list(adapt_delta = 0.9),
                fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_gp_sf_restr_alpha.dso")
save(stan.fit, file = "stan_gp_fits/stan_gp_sf_restr_alpha.RData")

#partial H.inv
for (k.restricted in 1:(ncol(X) - 1)){
  load("stan_gp_sf_restr_alpha_Hinv_partial.dso")
  stan.data = list(N = nrow(X), 
                   k_restricted = k.restricted, k_free = ncol(X) - k.restricted,
                   X = X, y = y,
                   alpha = alpha.restricted,
                   H_inv_diag_restricted = as.array(H.inv.diag.restricted[1:k.restricted]))
  stan.fit = stan("stan_gp_sf_restr_alpha_Hinv_partial.stan", data = stan.data,
                  control = list(adapt_delta = 0.9),
                  fit = stan.fit,
                  chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
  
  # save(stan.fit, file = "stan_gp_sf_restr_alpha_Hinv_partial.dso")
  save(stan.fit, file = sprintf("stan_gp_fits/stan_gp_sf_restr_alpha_Hinv_partial_%i.RData", k.restricted))
}

#full H.inv
load("stan_gp_sf_restr_alpha_Hinv.dso")
stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 alpha = alpha.restricted,
                 H_inv_diag = H.inv.diag.restricted)
stan.fit = stan("stan_gp_sf_restr_alpha_Hinv.stan", data = stan.data,
                control = list(adapt_delta = 0.9),
                fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_gp_sf_restr_alpha_Hinv.dso")
save(stan.fit, file = sprintf("stan_gp_fits/stan_gp_sf_restr_alpha_Hinv.RData", k.restricted))

#H.inv, sigma.v
load("stan_gp_sf_restr_alpha_Hinv_sigma_v.dso")
stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 alpha = alpha.restricted,
                 H_inv_diag = H.inv.diag.restricted,
                 sigma_v = sigma.v.restricted)
stan.fit = stan("stan_gp_sf_restr_alpha_Hinv_sigma_v.stan", data = stan.data,
                control = list(adapt_delta = 0.9),
                fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_gp_sf_restr_alpha_Hinv_sigma_v.dso")
save(stan.fit, file = sprintf("stan_gp_fits/stan_gp_sf_restr_alpha_Hinv_sigma_v.RData", k.restricted))

#H.inv, sigma.v, sigma.u
load("stan_gp_sf_restr_alpha_Hinv_sigma_uv.dso")
stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 alpha = alpha.restricted,
                 H_inv_diag = H.inv.diag.restricted,
                 sigma_v = sigma.v.restricted,
                 sigma_u = sigma.u.restricted)
stan.fit = stan("stan_gp_sf_restr_alpha_Hinv_sigma_uv.stan", data = stan.data,
                control = list(adapt_delta = 0.9),
                fit = stan.fit,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_gp_sf_restr_alpha_Hinv_sigma_uv.dso")
save(stan.fit, file = sprintf("stan_gp_fits/stan_gp_sf_restr_alpha_Hinv_sigma_uv.RData", k.restricted))

#H.inv, sigma.v, sigma.u, partial eta
for (N.restricted in 1:(nrow(X) - 1)){
  load("stan_gp_sf_restr_alpha_Hinv_sigma_uv_eta_partial.dso")
  stan.data = list(N_restricted = N.restricted,
                   N_free = nrow(X) - N.restricted,
                   k = ncol(X), X = X, y = y,
                   alpha = alpha.restricted,
                   H_inv_diag = H.inv.diag.restricted,
                   sigma_v = sigma.v.restricted,
                   sigma_u = sigma.u.restricted,
                   eta_restricted = as.array(eta.restricted[1:N.restricted]))
  stan.fit = stan("stan_gp_sf_restr_alpha_Hinv_sigma_uv_eta_partial.stan", data = stan.data,
                  control = list(adapt_delta = 0.9),
                  # fit = stan.fit,
                  chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
  
  # save(stan.fit, file = "stan_gp_sf_restr_alpha_Hinv_sigma_uv_eta_partial.dso")
  save(stan.fit, file = sprintf("stan_gp_fits/stan_gp_sf_restr_alpha_Hinv_sigma_uv_eta_partial.RData", k.restricted))
}
