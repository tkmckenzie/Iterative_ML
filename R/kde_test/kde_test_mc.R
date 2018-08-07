library(ks)
library(snow)

rm(list = ls())

cl = makeCluster(rep("localhost", 4), "SOCK")

eval.point = 0
d.true = dnorm(eval.point)

N.reps = 10000
eval.func = function(i){
  library(ks)
  
  N = 1000
  eval.point = 0
  
  x = rnorm(N)
  return(kde(x, eval.points = eval.point)$estimate)
}

d.est = parSapply(cl, 1:N.reps, eval.func)

t.test(d.est, mu = d.true)
