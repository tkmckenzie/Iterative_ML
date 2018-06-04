setwd("~/code/R/marginal_likelihood/probit_logit_chib")

rm(list = ls())

#Read, select data
df = read.csv("data.csv")

y = df$y
X = cbind(rep(1, length(y)), df$X.5)

#Load old results, if existent
num.reps = 100

if ("chib_model_6_results.RData" %in% list.files()){
  load("chib_model_6_results.RData")
} else{
  log.marginal.results = data.frame(logit = rep(NA, num.reps),
                                    probit = rep(NA, num.reps),
                                    model = "C + X.5")
}
rep = sum(complete.cases(log.marginal.results))

while (rep < num.reps){
  print(sprintf("########## REP %i ##########", rep + 1))
  
  source("stan_logit_ml_ks_iterative_fit.R")
  log.marginal.results$logit[rep + 1] = log.marginal
  
  source("stan_probit_ml_ks_iterative_fit.R")
  log.marginal.results$probit[rep + 1] = log.marginal
  
  rep = sum(complete.cases(log.marginal.results))
  save(log.marginal.results, file = "chib_model_6_results.RData")
}
