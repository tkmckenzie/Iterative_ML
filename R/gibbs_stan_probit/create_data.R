setwd("~/git/Iterative_ML/R/gibbs_stan_probit")

rm(list = ls())

#Parameters
N = 100
beta = c(-2, 5)

#Data creation
X = matrix(c(rep(1, N), runif((length(beta) - 1) * N, -1, 1)), ncol = length(beta))
z = X %*% beta + rnorm(N)
y = ifelse(z >= 0, 1, 0)
# z = pnorm(X %*% beta)
# y = rbinom(N, 1, z)

summary(glm(y ~ 0 + X, family = binomial(link = "probit")))

#Save:
save(X, y, N, file = "probitData.RData")
