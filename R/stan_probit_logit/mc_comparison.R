setwd("~/code/R/marginal_likelihood/stan_probit_logit")

rm(list = ls())

num.mc.iter = 100

if ("comparison_mc_results.RData" %in% list.files()){
  load("comparison_mc_results.RData")
} else{
  #Variable is of form model.data
  #e.g., probit.logit is a log.marginal from probit model run on logit data
  log.marginal.results = data.frame(probit.probit = rep(NA, num.mc.iter),
                                    logit.probit = rep(NA, num.mc.iter),
                                    probit.logit = rep(NA, num.mc.iter),
                                    logit.logit = rep(NA, num.mc.iter))
  t = c()
}

mc.iter = sum(complete.cases(log.marginal.results))

if (mc.iter < num.mc.iter){
  t1 = proc.time()
  
  sink("temp.txt")
  source("create_data.R")
  
  data.file = "probitData.RData"
  source("stan_probit_ml_ks_iterative_fit.R")
  log.marginal.results$probit.probit[mc.iter + 1] = log.marginal
  source("stan_logit_ml_ks_iterative_fit.R")
  log.marginal.results$logit.probit[mc.iter + 1] = log.marginal
  
  data.file = "logitData.RData"
  source("stan_probit_ml_ks_iterative_fit.R")
  log.marginal.results$probit.logit[mc.iter + 1] = log.marginal
  source("stan_logit_ml_ks_iterative_fit.R")
  log.marginal.results$logit.logit[mc.iter + 1] = log.marginal
  
  sink()
  
  t2 = proc.time()
  
  t = c(t, (t2 - t1)[3])
  
  print(sprintf("Run %i;   Avg. time (s) = %f; finishing at %s", mc.iter + 1, mean(t), Sys.time() + (num.mc.iter - mc.iter) * mean(t)))
  
  save(log.marginal.results, t, file = "comparison_mc_results.RData")
} else{
  print("num.mc.iter reached.")
}
