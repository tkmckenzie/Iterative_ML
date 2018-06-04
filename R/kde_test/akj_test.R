library(quantreg)

rm(list = ls())

eval.point = 0
d.true = dnorm(eval.point)

N.reps = 1000
N = 1000
d.est = rep(NA, N.reps)
for (i in 1:N.reps){
  x = rnorm(N)
  d.est[i] = akj(x, eval.point)$dens
}

t.test(d.est, mu = d.true)
