setwd("~/git/Iterative_ML/R/stan_sf_data")

rm(list = ls())

df = read.csv("gp_results_20180615.csv")

m = lm(log_lik ~ log(alpha_prior_sd) + log(H_inv_diag_prior_rate), df)
summary(m)