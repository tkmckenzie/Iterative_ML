setwd("~/docs/Iterative_ML_paper/R/stan_probit_logit");

rm(list = ls())

#Variable is of form model.data
#e.g., probit.logit is a log.marginal from probit model run on logit data
load("comparison_mc_results.RData")

#Clean and calculate model probabilities
log.marginal.results = log.marginal.results[complete.cases(log.marginal.results),]

log.marginal.results$prob.probit.probit = with(log.marginal.results,
                                               exp(probit.probit) / (exp(probit.probit) + exp(logit.probit)))
log.marginal.results$prob.logit.probit = 1 - log.marginal.results$prob.probit.probit

log.marginal.results$prob.logit.logit = with(log.marginal.results,
                                               exp(logit.logit) / (exp(logit.logit) + exp(probit.logit)))
log.marginal.results$prob.probit.logit = 1 - log.marginal.results$prob.logit.logit

#Statistics
#Probit model on probit data
mean(log.marginal.results$prob.probit.probit)
mean(log.marginal.results$prob.probit.probit > 0.5)

#Logit model on logit data
mean(log.marginal.results$prob.logit.logit)
mean(log.marginal.results$prob.logit.logit > 0.5)
