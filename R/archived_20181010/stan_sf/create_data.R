library(dplyr)
library(frontier)
library(truncnorm)

setwd("~/git/Iterative_ML/R/stan_sf")

rm(list = ls())

N = 100
k = 1

a = 5
b = runif(k)

f = function(log.x){
  return(a + log.x %*% b)
}

X = matrix(rlnorm(N * k), ncol = k)
y = c(f(X)) + rnorm(N, sd = 1) - rtruncnorm(N, sd = 0.25)

#Simple lm
summary(lm(y ~ X))
summary(sfa(y ~ X))

#Save data
save(X, y, a, b, file = "data.RData")
