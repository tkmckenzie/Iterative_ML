library(ggplot2)
library(rstan)

setwd("~/git/Iterative_ML/R/stan_sf_data")

rm(list = ls())

#Priors
sigma_u_prior_scale = 1
sigma_v_prior_scale = 1
beta_const_prior_sd = 10
beta_prior_sd = 1

save(sigma_u_prior_scale,
     sigma_v_prior_scale,
     beta_const_prior_sd,
     beta_prior_sd,
     file = "stan_par_fits/priors.RData")

load("data.RData")
# load("stan_par_fits/restricted_params.RData")

burn.iter = 10000
sample.iter = 5000

adapt.delta = 0.9

#Unconditional run:
stan.model.file = "stan_par_sf.stan"
stan.dso.file = gsub(".stan$", ".dso", stan.model.file)

stan.data = list(N = nrow(X),
                 k = ncol(X),
                 X = X,
                 y = y,
                 beta_const_prior_sd = beta_const_prior_sd,
                 beta_prior_sd = beta_prior_sd,
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
                control = list(adapt_delta = adapt.delta),
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

save(stan.fit, file = "stan_par_fits/stan_par_sf_unconditional.RData")

traceplot(stan.fit)
if (readline("Enter y to accept initial sample: ") != "y") stop("Initial sample rejected.")

#Restricted values:
stan.extract = extract(stan.fit)

sigma.u.restricted = mean(stan.extract$sigma_u)
sigma.v.restricted = mean(stan.extract$sigma_v)
beta.restricted = apply(stan.extract$beta, 2, mean)
beta.const.restricted = mean(stan.extract$beta_const)

save(sigma.u.restricted, sigma.v.restricted, beta.restricted, beta.const.restricted,
     file = "stan_par_fits/restricted_params.RData")

#Conditional runs:
#sigma.u
stan.model.file = "stan_par_sf_restr_sigma_u.stan"
stan.dso.file = gsub(".stan$", ".dso", stan.model.file)

stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 beta_const_prior_sd = beta_const_prior_sd,
                 beta_prior_sd = beta_prior_sd,
                 sigma_v_prior_scale = sigma_v_prior_scale,
                 sigma_u = sigma.u.restricted)

if (!(stan.dso.file %in% list.files())){
  stan.dso = stan(stan.model.file, data = stan.data,
                  chains = 1, iter = 1, warmup = 1, refresh = 0)
  save(stan.dso, file = stan.dso.file)
} else{
  load(stan.dso.file)
}

stan.fit = stan(stan.model.file, data = stan.data,
                control = list(adapt_delta = adapt.delta),
                init = list(list(beta_const = beta.const.restricted,
                                 beta = beta.restricted,
                                 sigma_v = sigma.v.restricted)),
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

save(stan.fit, file = "stan_par_fits/stan_par_sf_restr_sigma_u.RData")

#sigma.u, sigma.v
stan.model.file = "stan_par_sf_restr_sigma_uv.stan"
stan.dso.file = gsub(".stan$", ".dso", stan.model.file)

stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 beta_const_prior_sd = beta_const_prior_sd,
                 beta_prior_sd = beta_prior_sd,
                 sigma_u = sigma.u.restricted,
                 sigma_v = sigma.v.restricted)

if (!(stan.dso.file %in% list.files())){
  stan.dso = stan(stan.model.file, data = stan.data,
                  chains = 1, iter = 1, warmup = 1, refresh = 0)
  save(stan.dso, file = stan.dso.file)
} else{
  load(stan.dso.file)
}

stan.fit = stan(stan.model.file, data = stan.data,
                control = list(adapt_delta = adapt.delta),
                init = list(list(beta_const = beta.const.restricted,
                                 beta = beta.restricted)),
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

save(stan.fit, file = "stan_par_fits/stan_par_sf_restr_sigma_uv.RData")

#sigma.u, sigma.v, partial beta
for (k.restricted in 1:(ncol(X) - 1)){
  if (k.restricted < (ncol(X) - 1)){
    stan.model.file = "stan_par_sf_restr_sigma_uv_beta_partial.stan"
    stan.dso.file = gsub(".stan$", ".dso", stan.model.file)
    
    stan.data = list(N = nrow(X),
                     k_restricted = k.restricted, k_free = ncol(X) - k.restricted,
                     X = X, y = y,
                     beta_const_prior_sd = beta_const_prior_sd,
                     beta_prior_sd = beta_prior_sd,
                     sigma_u = sigma.u.restricted,
                     sigma_v = sigma.v.restricted,
                     beta_restricted = as.array(beta.restricted[1:k.restricted]))
    
    if (!(stan.dso.file %in% list.files())){
      stan.dso = stan(stan.model.file, data = stan.data,
                      chains = 1, iter = 1, warmup = 1, refresh = 0)
      save(stan.dso, file = stan.dso.file)
    } else{
      load(stan.dso.file)
    }
    
    stan.fit = stan(stan.model.file, data = stan.data,
                    control = list(adapt_delta = adapt.delta),
                    init = list(list(beta_const = beta.const.restricted,
                                     beta_free = beta.restricted[-(1:k.restricted)])),
                    chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
    
    save(stan.fit, file = sprintf("stan_par_fits/stan_par_sf_restr_sigma_uv_beta_partial_%i.RData", k.restricted))
  } else{
    stan.model.file = "stan_par_sf_restr_sigma_uv_beta_partial_singlefree.stan"
    stan.dso.file = gsub(".stan$", ".dso", stan.model.file)
    
    stan.data = list(N = nrow(X),
                     k_restricted = k.restricted, k_free = ncol(X) - k.restricted,
                     X = X, y = y,
                     beta_const_prior_sd = beta_const_prior_sd,
                     beta_prior_sd = beta_prior_sd,
                     sigma_u = sigma.u.restricted,
                     sigma_v = sigma.v.restricted,
                     beta_restricted = as.array(beta.restricted[1:k.restricted]))
    
    if (!(stan.dso.file %in% list.files())){
      stan.dso = stan(stan.model.file, data = stan.data,
                      chains = 1, iter = 1, warmup = 1, refresh = 0)
      save(stan.dso, file = stan.dso.file)
    } else{
      load(stan.dso.file)
    }
    
    stan.fit = stan(stan.model.file, data = stan.data,
                    control = list(adapt_delta = adapt.delta),
                    init = list(list(beta_const = beta.const.restricted,
                                     beta_free = beta.restricted[-(1:k.restricted)])),
                    chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
    
    save(stan.fit, file = sprintf("stan_par_fits/stan_par_sf_restr_sigma_uv_beta_partial_%i.RData", k.restricted))
  }
}

#sigma.u, sigma.v, beta (not including beta_const)
stan.model.file = "stan_par_sf_restr_sigma_uv_beta.stan"
stan.dso.file = gsub(".stan$", ".dso", stan.model.file)

stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 sigma_u = sigma.u.restricted,
                 sigma_v = sigma.v.restricted,
                 beta_const_prior_sd = beta_const_prior_sd,
                 beta = beta.restricted)

if (!(stan.dso.file %in% list.files())){
  stan.dso = stan(stan.model.file, data = stan.data,
                  chains = 1, iter = 1, warmup = 1, refresh = 0)
  save(stan.dso, file = stan.dso.file)
} else{
  load(stan.dso.file)
}

stan.fit = stan(stan.model.file, data = stan.data,
                control = list(adapt_delta = adapt.delta),
                init = list(list(beta_const = beta.const.restricted)),
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

save(stan.fit, file = "stan_par_fits/stan_par_sf_restr_sigma_uv_beta.RData")
