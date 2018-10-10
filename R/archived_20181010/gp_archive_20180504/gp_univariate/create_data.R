setwd("~/docs/Iterative_ML_paper/R/gp_univariate")

rm(list = ls())

N = 30

f = function(x){
  return(10 * x^(1/3) - 250 * dnorm(x, mean = 75, sd = 10))
}

x = runif(N, 10, 100)
y = sapply(x, f) + rnorm(N, sd = 1)

plot(y ~ x)

save(x, y, file = "data.RData")

