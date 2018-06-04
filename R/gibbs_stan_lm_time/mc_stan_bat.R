setwd("~/code/R/marginal_likelihood/gibbs_stan_lm_time")

num.mc.iter = 100

if ("stan_mc_results.RData" %in% list.files()){
  load("stan_mc_results.RData")
} else{
  log.marginal.results = rep(NA, num.mc.iter)
  t = c()
}
mc.iter = sum(!is.na(log.marginal.results))

if (mc.iter < num.mc.iter){
  t1 = proc.time()
  sink("temp.txt")
  source("stan_ml_ks_iterative_fit.R")
  sink()
  t2 = proc.time()
  
  t = c(t, (t2 - t1)[3])
  
  log.marginal.results[mc.iter + 1] = log.marginal
  print(sprintf("Run %i: %f;   Avg. time (s) = %f; finishing at %s", mc.iter + 1, log.marginal, mean(t), Sys.time() + (num.mc.iter - mc.iter) * mean(t)))
  
  save(log.marginal.results, t, file = "stan_mc_results.RData")
} else{
  print("num.mc.iter reached.")
}
