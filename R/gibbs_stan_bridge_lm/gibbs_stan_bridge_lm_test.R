library(ggplot2)

setwd("~/git/Iterative_ML/R/gibbs_stan_bridge_lm")

rm(list = ls())

#Load MC results
load("gibbs_mc_results.RData")
gibbs.results = log.marginal.results[1,]

load("stan_mc_results.RData")
stan.results = log.marginal.results[1,]

load("stan_bridge_mc_results.RData")
bridge.results = log.marginal.results[1,]

rm(log.marginal.results)

#Test for mean equality
t.test(gibbs.results, stan.results)
t.test(gibbs.results, bridge.results)
var.test(bridge.results, stan.results, alternative = "less")

#Statistics
mean(gibbs.results, na.rm = TRUE)
mean(stan.results, na.rm = TRUE)
mean(bridge.results, na.rm = TRUE)

sd(gibbs.results, na.rm = TRUE)
sd(stan.results, na.rm = TRUE)
sd(bridge.results, na.rm = TRUE)

#Plot
plot.data = data.frame(x = c(gibbs.results, stan.results, bridge.results),
                       variable = c(rep("gibbs", length(gibbs.results)), rep("stan", length(stan.results)), rep("bridge", length(bridge.results))))
ggplot(plot.data, aes(x)) + geom_density(aes(color = variable, fill = variable), alpha = 0.25)
ggplot(subset(plot.data, variable != "bridge"), aes(x)) + geom_density(aes(color = variable, fill = variable), alpha = 0.25)
