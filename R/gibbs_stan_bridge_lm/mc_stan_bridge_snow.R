library(snow)

rm(list = ls())

setwd("~/git/Iterative_ML/R/gibbs_stan_bridge_lm")

num.mc.iter = 100

sim.files = grep("^stan_bridge_snow_[0-9]+.csv$", list.files("stan_bridge_snow"), value = TRUE)
sim.nums = as.numeric(gsub("stan_bridge_snow_|.csv", "", sim.files))

remaining.sims = setdiff(1:num.mc.iter, sim.nums)
print(length(remaining.sims))

eval.func = function(i){
  setwd("~/git/Iterative_ML/R/gibbs_stan_bridge_lm")
  
  t1 = proc.time()
  source("stan_ml_bridge_fit.R")
  t2 = proc.time()
  
  t = (t2 - t1)[3]
  
  write.csv(c(log.marginal, t), paste0("stan_bridge_snow/stan_bridge_snow_", i, ".csv"), row.names = FALSE)
  
  return(c(log.marginal, t))
}

if (length(remaining.sims) > 0){
  cl = makeCluster(rep("localhost", 4), "SOCK")
  log.marginal.results = parSapply(cl, remaining.sims, eval.func)
}

#Run after getting all sims
sim.files = grep("^stan_bridge_snow_[0-9]+.csv$", list.files("stan_bridge_snow"), value = TRUE)
sims = lapply(sim.files, function(s) read.csv(paste0("stan_bridge_snow/", s)))
log.marginal.results = as.matrix(Reduce(cbind, sims))
save(log.marginal.results, file = "stan_bridge_mc_results.RData")
