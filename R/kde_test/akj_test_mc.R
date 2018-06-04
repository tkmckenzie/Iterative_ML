library(quantreg)
library(snow)

rm(list = ls())

cl = makeCluster(rep("localhost", 4), "SOCK")

eval.point = 0
d.true = dnorm(eval.point)

N.reps = 1000
eval.func = function(i){
  library(quantreg)
  
  N = 1000
  eval.point = 0
  
  x = rnorm(N)
  return(akj(x, eval.point)$dens)
}

d.est = parSapply(cl, 1:N.reps, eval.func)

t.test(d.est, mu = d.true)
