library(ggplot2)

setwd("~/git/Iterative_ML/R/gibbs_stan_lm")

rm(list = ls())

#Load MC results
load("gibbs_mc_results.RData")
gibbs.results = log.marginal.results[1,1:500]

load("stan_mc_results.RData")
stan.results = log.marginal.results[1,1:500]

rm(log.marginal.results)

#Test for mean equality
t.test(gibbs.results, stan.results)
var.test(gibbs.results, stan.results, alternative = "greater")

#Statistics
mean(gibbs.results, na.rm = TRUE)
mean(stan.results, na.rm = TRUE)

sd(gibbs.results, na.rm = TRUE)
sd(stan.results, na.rm = TRUE)

#Plot
plot.data = data.frame(x = c(gibbs.results, stan.results),
                       variable = c(rep("gibbs", length(gibbs.results)), rep("stan", length(stan.results))))
ggplot(plot.data, aes(x)) + geom_density(aes(color = variable, fill = variable), alpha = 0.25)
