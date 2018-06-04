setwd("~/docs/Iterative_ML_paper/R/stan_probit_logit")

# rm(list = ls())

sigmoid = function(x) 1 / (1 + exp(-x))

#Parameters
N = 100
beta = c(-5, 13)

#Data creation
X = matrix(c(rep(1, N), runif((length(beta) - 1) * N, -1, 1)), ncol = length(beta))
z = X %*% beta

p.probit = pnorm(z)
p.logit = sigmoid(z)

y.probit = ifelse(runif(N) < p.probit, 1, 0)
y.logit = ifelse(runif(N) < p.logit, 1, 0)

summary(glm(y.probit ~ 0 + X, family = binomial(link = "probit")))
summary(p.probit)

summary(glm(y.logit ~ 0 + X, family = binomial(link = "logit")))
summary(p.logit)

#Save:
y = y.probit
save(X, y, N, file = "probitData.RData")

y = y.logit
save(X, y, N, file = "logitData.RData")
