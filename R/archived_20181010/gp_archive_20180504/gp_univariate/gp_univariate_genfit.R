dist = function(x.1, x.2){
  nrow = length(x.1)
  ncol = length(x.2)
  
  m = matrix(NA, nrow = nrow, ncol = ncol)
  for (i in 1:nrow){
    for (j in 1:ncol){
      m[i, j] = (x.1[i] - x.2[j])^2
    }
  }
  
  return(m)
}
cov.exp.quad = function(x.1, x.2, alpha, rho){
  return(exp(2 * log(alpha) - dist(x.1, x.2) / (2 * rho^2)))
}
tri = function(M){
  #Provides lower triangular version of M
  M[upper.tri(M)] = 0
  return(M)
}

gp.fit = function(x.pred, 
                  y.is, x.is,
                  alpha, length.scale, sigma){
  f.pred = matrix(NA, ncol = 2, nrow = length(x.pred))
  
  N.pred = length(x.pred)
  N = length(y.is)
  
  Sigma = cov.exp.quad(x.is, x.is, alpha, length.scale) + sigma^2 * diag(N)
  L.Sigma = t(chol(Sigma))
  # K.div.y.is = solve(tri(L.Sigma)) %*% y.is
  K.div.y.is = solve(tri(L.Sigma), y.is)
  # K.div.y.is = t(K.div.y.is) %*% solve(L.Sigma)
  K.div.y.is = solve(t(L.Sigma), K.div.y.is)
  k.x.is.x.pred = cov.exp.quad(x.is, x.pred, alpha, length.scale)
  f.pred.mu = t(k.x.is.x.pred) %*% K.div.y.is
  v.pred = solve(tri(L.Sigma), k.x.is.x.pred)
  cov.f.pred = cov.exp.quad(x.pred, x.pred, alpha, length.scale) - t(v.pred) %*% v.pred
  
  f.pred[,1] = f.pred.mu
  f.pred[,2] = sqrt(diag(cov.f.pred) + sigma^2)
  
  return(f.pred)
}