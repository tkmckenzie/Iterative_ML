library(snow)

rm(list = ls())

num.mc.iter = 1000

eval.func = function(i){
  setwd("~/git/Iterative_ML/R/gibbs_stan_lm")

  t1 = proc.time()
  source("gibbs_ml.R")
  t2 = proc.time()
  
  t = (t2 - t1)[3]
  return(c(log.marginal, t))
}


cl = makeCluster(rep("localhost", 4), "SOCK")
log.marginal.results = parSapply(cl, 1:num.mc.iter, eval.func)

save(log.marginal.results, file = "gibbs_mc_results.RData")
