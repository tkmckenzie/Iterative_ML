setwd("~/code/R/marginal_likelihood/gibbs_stan_probit")

rm(list = ls())

num.mc.iter = 100

log.marginal.results = rep(NA, num.mc.iter)

t = c()

for (mc.iter in 1:num.mc.iter){
  t1 = proc.time()
  source("gibbs_ml.R")
  t2 = proc.time()
  
  t = c(t, (t2 - t1)[3])
  
  log.marginal.results[mc.iter] = log.marginal
  print(sprintf("Run %i: %f;   Avg. time (s) = %f; finishing at %s", mc.iter, log.marginal, mean(t), Sys.time() + (num.mc.iter - mc.iter) * mean(t)))
}

save(log.marginal.results, file = "gibbs_mc_results.RData")
