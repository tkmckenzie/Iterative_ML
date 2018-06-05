setwd("~/git/Iterative_ML/R/gibbs_stan_lm")

rm(list = ls())

#Parameters
N = 100
beta = c(-2, 5, 3)
sd = 25

#Data creation
X = matrix(c(rep(1, N), runif((length(beta) - 1) * N, -10, 10)), ncol = length(beta))
y = X %*% beta + rnorm(N, sd = sd)

#Save:
save(X, y, file = "lmData.RData")
