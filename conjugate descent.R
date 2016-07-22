##########conjugate descent############

conjugate_des = function(f,f_g,d,eps,niter){
  k = 1 ; x = matrix(,nrow = 500, ncol = d); dx = matrix(,nrow = 500, ncol = d)
  x[k,] = rnorm(d)
  dx[k,] = -f_g(x[k,])
  num = 0
  while((k<500) & (sum(dx[k,]^2)>eps)){
    
    # restarter
    if((k%%niter==1)&(k>1)){
      x[(k+1),] = x[k,] - 0.01 * f_g(x[(k+1),])
      num = num+1
    }
    f_x = function(alpha){
      f(x[k,] + alpha * dx[k,])
    }
    out = golden_sec(f_x, eps, 1e-06)
    alpha = out[1]
    num = num+ out[2]
    x[(k+1),] = x[k,] + alpha * dx[k,]
    dx[(k+1),] = -f_g(x[(k+1),]) + dx[k,]* sum(f_g(x[(k+1),])^2)/sum(f_g(x[k,])^2)
    k = k+1
    
  }
  #print(num)
  return(list('num' = num, 'vec' = x[k,]))
}

