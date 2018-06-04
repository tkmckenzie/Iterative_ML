library(ggplot2)

setwd("~/docs/Iterative_ML_paper/R/gibbs_stan_probit")

rm(list = ls())

#Load MC results
load("gibbs_mc_results.RData")
gibbs.results = log.marginal.results[1,]

load("stan_mc_results.RData")
stan.results = log.marginal.results[1,]

rm(log.marginal.results)

#Test for mean equality
t.test(gibbs.results, stan.results)
var.test(gibbs.results, stan.results, alternative = "two.sided")

#Statistics
mean(gibbs.results)
mean(stan.results)

sd(gibbs.results)
sd(stan.results)

#Plot
plot.data = data.frame(x = c(gibbs.results, stan.results),
                       variable = c(rep("gibbs", length(gibbs.results)), rep("stan", length(stan.results))))
ggplot(plot.data, aes(x)) + geom_density(aes(color = variable, fill = variable), alpha = 0.25)
