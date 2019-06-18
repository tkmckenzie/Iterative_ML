library(ggplot2)
library(rstan)

setwd("~/git/Iterative_ML/R/stan_sf_data")

rm(list = ls())

load("data.RData")
load("stan_gp_fits/restricted_params.RData")

#Priors
alpha_prior_shape = 1
alpha_prior_rate = 1
H_inv_diag_prior_shape = 10
H_inv_diag_prior_rate = 1
sigma_u_prior_shape = 1
sigma_u_prior_rate = 1
sigma_v_prior_shape = 1
sigma_v_prior_rate = 1

y.mean = mean(y)
y = y - y.mean

save(sigma_u_prior_shape,
     sigma_u_prior_rate,
     sigma_v_prior_shape,
     sigma_v_prior_rate,
     alpha_prior_shape,
     alpha_prior_rate,
     H_inv_diag_prior_shape,
     H_inv_diag_prior_rate,
     y.mean,
     file = "stan_gp_fits/priors.RData")

#MCMC parameters
burn.iter = 9000
sample.iter = 1000

adapt.delta = 0.9
max.treedepth = 12


#Unconditional run
stan.model.file = "stan_gp_sf.stan"
stan.dso.file = gsub(".stan$", ".dso", stan.model.file)

stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 alpha_prior_shape = alpha_prior_shape,
                 alpha_prior_rate = alpha_prior_rate,
                 H_inv_diag_prior_shape = H_inv_diag_prior_shape,
                 H_inv_diag_prior_rate = H_inv_diag_prior_rate,
                 sigma_u_prior_shape = sigma_u_prior_shape,
                 sigma_u_prior_rate = sigma_u_prior_shape,
                 sigma_v_prior_shape = sigma_v_prior_shape,
                 sigma_v_prior_rate = sigma_v_prior_shape)

if (!(stan.dso.file %in% list.files())){
  stan.dso = stan(stan.model.file, data = stan.data,
                  chains = 1, iter = 1, warmup = 1, refresh = 0)
  save(stan.dso, file = stan.dso.file)
} else{
  load(stan.dso.file)
}

stan.fit = stan(stan.model.file, data = stan.data,
                control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_gp_sf.dso")
save(stan.fit, file = "stan_gp_fits/stan_gp_sf_unconditional.RData")

load("stan_gp_fits/stan_gp_sf_unconditional.RData")

traceplot(stan.fit)
# if (readline("Enter y to accept initial sample: ") != "y") stop("Initial sample rejected.")

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
stan.model.file = "stan_gp_sf_restr_sigma_u.stan"
stan.dso.file = gsub(".stan$", ".dso", stan.model.file)

stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 alpha_prior_shape = alpha_prior_shape,
                 alpha_prior_rate = alpha_prior_rate,
                 H_inv_diag_prior_shape = H_inv_diag_prior_shape,
                 H_inv_diag_prior_rate = H_inv_diag_prior_rate,
                 sigma_v_prior_shape = sigma_v_prior_shape,
                 sigma_v_prior_rate = sigma_v_prior_shape,
                 sigma_u = sigma.u.restricted)

if (!(stan.dso.file %in% list.files())){
  stan.dso = stan(stan.model.file, data = stan.data,
                  chains = 1, iter = 1, warmup = 1, refresh = 0)
  save(stan.dso, file = stan.dso.file)
} else{
  load(stan.dso.file)
}

stan.fit = stan(stan.model.file, data = stan.data,
                control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                init = list(list(alpha = alpha.restricted,
                                 H_inv_diag = H.inv.diag.restricted,
                                 sigma_v = sigma.v.restricted,
                                 eta = eta.restricted)),
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_gp_sf_restr_sigma_u.dso")
save(stan.fit, file = "stan_gp_fits/stan_gp_sf_restr_sigma_u.RData")

#sigma.u, sigma.v
stan.model.file = "stan_gp_sf_restr_sigma_uv.stan"
stan.dso.file = gsub(".stan$", ".dso", stan.model.file)

stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 alpha_prior_shape = alpha_prior_shape,
                 alpha_prior_rate = alpha_prior_rate,
                 H_inv_diag_prior_shape = H_inv_diag_prior_shape,
                 H_inv_diag_prior_rate = H_inv_diag_prior_rate,
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
                control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                init = list(list(alpha = alpha.restricted,
                                 H_inv_diag = H.inv.diag.restricted,
                                 eta = eta.restricted)),
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_gp_sf_restr_sigma_uv.dso")
save(stan.fit, file = "stan_gp_fits/stan_gp_sf_restr_sigma_uv.RData")

#sigma.u, sigma.v, partial H.inv
for (k.restricted in 1:(ncol(X) - 1)){
  stan.data = list(N = nrow(X),
                   k_restricted = k.restricted,
                   k_free = ncol(X) - k.restricted,
                   X = X, y = y,
                   alpha_prior_shape = alpha_prior_shape,
                   alpha_prior_rate = alpha_prior_rate,
                   H_inv_diag_prior_shape = H_inv_diag_prior_shape,
                   H_inv_diag_prior_rate = H_inv_diag_prior_rate,
                   sigma_u = sigma.u.restricted,
                   sigma_v = sigma.v.restricted,
                   H_inv_diag_restricted = as.array(H.inv.diag.restricted[1:k.restricted]))
  if (k.restricted < (ncol(X) - 1)){
    stan.model.file = "stan_gp_sf_restr_sigma_uv_H_inv_partial.stan"
    stan.dso.file = gsub(".stan$", ".dso", stan.model.file)
    
    if (!(stan.dso.file %in% list.files())){
      stan.dso = stan(stan.model.file, data = stan.data,
                      chains = 1, iter = 1, warmup = 1, refresh = 0)
      save(stan.dso, file = stan.dso.file)
    } else{
      load(stan.dso.file)
    }
    
    stan.fit = stan(stan.model.file, data = stan.data,
                    control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                    init = list(list(alpha = alpha.restricted,
                                     H_inv_diag_free = H.inv.diag.restricted[-(1:k.restricted)],
                                     eta = eta.restricted)),
                    chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
    # save(stan.fit, file = "stan_gp_sf_restr_sigma_uv_H_inv_partial.dso")
  } else{
    stan.model.file = "stan_gp_sf_restr_sigma_uv_H_inv_partial_singlefree.stan"
    stan.dso.file = gsub(".stan$", ".dso", stan.model.file)
    
    if (!(stan.dso.file %in% list.files())){
      stan.dso = stan(stan.model.file, data = stan.data,
                      chains = 1, iter = 1, warmup = 1, refresh = 0)
      save(stan.dso, file = stan.dso.file)
    } else{
      load(stan.dso.file)
    }
    
    stan.fit = stan(stan.model.file, data = stan.data,
                    control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                    init = list(list(alpha = alpha.restricted,
                                     H_inv_diag_free = H.inv.diag.restricted[-(1:k.restricted)],
                                     eta = eta.restricted)),
                    chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
    save(stan.fit, file = "stan_gp_sf_restr_sigma_uv_H_inv_partial_singlefree.dso")
  }
  
  save(stan.fit, file = sprintf("stan_gp_fits/stan_gp_sf_restr_sigma_uv_H_inv_partial_%i.RData", k.restricted))
}

#sigma.u, sigma.v, H.inv
stan.model.file = "stan_gp_sf_restr_sigma_uv_H_inv.stan"
stan.dso.file = gsub(".stan$", ".dso", stan.model.file)

stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 alpha_prior_shape = alpha_prior_shape,
                 alpha_prior_rate = alpha_prior_rate,
                 sigma_u = sigma.u.restricted,
                 sigma_v = sigma.v.restricted,
                 H_inv_diag = H.inv.diag.restricted)

if (!(stan.dso.file %in% list.files())){
  stan.dso = stan(stan.model.file, data = stan.data,
                  chains = 1, iter = 1, warmup = 1, refresh = 0)
  save(stan.dso, file = stan.dso.file)
} else{
  load(stan.dso.file)
}

stan.fit = stan(stan.model.file, data = stan.data,
                control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                init = list(list(alpha = alpha.restricted,
                                 eta = eta.restricted)),
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_gp_sf_restr_sigma_uv_H_inv.dso")
save(stan.fit, file = "stan_gp_fits/stan_gp_sf_restr_sigma_uv_H_inv.RData")

#sigma.u, sigma.v, H.inv, partial eta
for (N.restricted in 1:(nrow(X) - 1)){
  stan.data = list(N_free = nrow(X) - N.restricted,
                   N_restricted = N.restricted,
                   k = ncol(X), X = X, y = y,
                   alpha_prior_shape = alpha_prior_shape,
                   alpha_prior_rate = alpha_prior_rate,
                   sigma_u = sigma.u.restricted,
                   sigma_v = sigma.v.restricted,
                   H_inv_diag = H.inv.diag.restricted,
                   eta_restricted = as.array(eta.restricted[1:N.restricted]))
  if (N.restricted < (nrow(X) - 1)){
    stan.model.file = "stan_gp_sf_restr_sigma_uv_H_inv_eta_partial.stan"
    stan.dso.file = gsub(".stan$", ".dso", stan.model.file)
    
    if (!(stan.dso.file %in% list.files())){
      stan.dso = stan(stan.model.file, data = stan.data,
                      chains = 1, iter = 1, warmup = 1, refresh = 0)
      save(stan.dso, file = stan.dso.file)
    } else{
      load(stan.dso.file)
    }
    
    stan.fit = stan(stan.model.file, data = stan.data,
                    control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                    init = list(list(alpha = alpha.restricted,
                                     eta_free = eta.restricted[-(1:N.restricted)])),
                    chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
    # save(stan.fit, file = "stan_gp_sf_restr_sigma_uv_H_inv_eta_partial.dso")
  } else{
    stan.model.file = "stan_gp_sf_restr_sigma_uv_H_inv_eta_partial_singlefree.stan"
    stan.dso.file = gsub(".stan$", ".dso", stan.model.file)
    
    if (!(stan.dso.file %in% list.files())){
      stan.dso = stan(stan.model.file, data = stan.data,
                      chains = 1, iter = 1, warmup = 1, refresh = 0)
      save(stan.dso, file = stan.dso.file)
    } else{
      load(stan.dso.file)
    }
    
    stan.fit = stan(stan.model.file, data = stan.data,
                    control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                    init = list(list(alpha = alpha.restricted,
                                     eta_free = eta.restricted[-(1:N.restricted)])),
                    chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)
    # save(stan.fit, file = "stan_gp_sf_restr_sigma_uv_H_inv_eta_partial_singlefree.dso")
  }
  
  save(stan.fit, file = sprintf("stan_gp_fits/stan_gp_sf_restr_sigma_uv_H_inv_eta_partial_%i.RData", N.restricted))
}

#sigma.u, sigma.v, H.inv, eta
stan.model.file = "stan_gp_sf_restr_sigma_uv_H_inv_eta.stan"
stan.dso.file = gsub(".stan$", ".dso", stan.model.file)

stan.data = list(N = nrow(X), k = ncol(X), X = X, y = y,
                 alpha_prior_shape = alpha_prior_shape,
                 alpha_prior_rate = alpha_prior_rate,
                 sigma_u = sigma.u.restricted,
                 sigma_v = sigma.v.restricted,
                 H_inv_diag = H.inv.diag.restricted,
                 eta = eta.restricted)

if (!(stan.dso.file %in% list.files())){
  stan.dso = stan(stan.model.file, data = stan.data,
                  chains = 1, iter = 1, warmup = 1, refresh = 0)
  save(stan.dso, file = stan.dso.file)
} else{
  load(stan.dso.file)
}

stan.fit = stan(stan.model.file, data = stan.data,
                control = list(adapt_delta = adapt.delta, max_treedepth = max.treedepth),
                init = list(list(alpha = alpha.restricted)),
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter)

# save(stan.fit, file = "stan_gp_sf_restr_sigma_uv_H_inv_eta.dso")
save(stan.fit, file = "stan_gp_fits/stan_gp_sf_restr_sigma_uv_H_inv_eta.RData")
