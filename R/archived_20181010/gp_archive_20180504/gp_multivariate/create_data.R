setwd("~/docs/Iterative_ML_paper/R/gp_multivariate")

rm(list = ls())

N = 30

f = function(x){
  return(10 * x[,1]^(1/3) * x[,2]^(1/4))
}
# f = Vectorize(f)

x = matrix(runif(2 * N, 10, 100), ncol = 2)
y = f(x) + rnorm(N, sd = 1)

plot(y ~ x[,1])
summary(lm(log(y) ~ log(x)))

save(x, y, file = "data.RData")
