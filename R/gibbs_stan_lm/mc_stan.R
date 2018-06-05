setwd("~/git/Iterative_ML/R/gibbs_stan_lm")

rm(list = ls())

num.mc.iter = 100

log.marginal.results = rep(NA, num.mc.iter)

t = c()

load("stan_mc_results.RData")
mc.iter = sum(!is.na(log.marginal.results))

while (mc.iter < num.mc.iter){
  t1 = proc.time()
  sink("temp.txt")
  source("stan_ml_ks_iterative_fit.R")
  sink()
  t2 = proc.time()
  
  t = c(t, (t2 - t1)[3])
  
  log.marginal.results[mc.iter + 1] = log.marginal
  print(sprintf("Run %i: %f;   Avg. time (s) = %f; finishing at %s", mc.iter + 1, log.marginal, mean(t), Sys.time() + (num.mc.iter - mc.iter) * mean(t)))
  
  save(log.marginal.results, file = "stan_mc_results.RData")
  mc.iter = sum(!is.na(log.marginal.results))
}
