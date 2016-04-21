g_f_1 = function(x){
  c(3*x[1] + x[2] -5, x[1] + 3*x[2] -1)
}

f_2(c(1,2,3,4,5,6,7,8,9,0))
g_f_2(c(1,2,3,4,5,6,7,8,9,0))

f_1 = function(x){
  0.5* (x[1] + x[2] -1)^2 + (x[1]-2)^2 + x[2]^2
}

f_2 = function(x){
 sum((c(1,2,3,3,2,1,4,5,3,2)*x-c(1,2,3,4,5,6,7,8,9,0))^2) + 1/sum((x^2+1))
}

g_f_2 = function(x){
  ((c(1,2,3,3,2,1,4,5,3,2)*x-c(1,2,3,4,5,6,7,8,9,0))*2*c(1,2,3,3,2,1,4,5,3,2) 
  - 2*x[1]/sum((x^2+1))^2)
}

f_3 = function(x){
  sum((c(1,2,3,3,2,1,4,5,3,2)*x-c(1,2,3,4,5,6,7,8,9,0))^2) + 1/sum((x^2+1))
}

g_f_3 = function(x){
  ((c(1,2,3,3,2,1,4,5,3,2)*x-c(1,2,3,4,5,6,7,8,9,0))*2*c(1,2,3,3,2,1,4,5,3,2) 
   - 2*x[1]/sum((x^2+1))^2)
}

conjugate_des(f_2,g_f_2,10,0.001,10)

coordinate_des = function(f, eps,epsm){
  a = 0; b = 0; olda = 1; oldb = 1; i = 0; num = 0
  while((dist(rbind(c(a,b),c(olda,oldb))) > (eps + epsm * abs(min(a,b)))) & (i < 5000)){
    i = i + 1
    olda = a; oldb = b
    f_x = function(x){
      f(c(x,b))
    }
    out = golden_sec(f_x, eps, epsm)
    a = out[1]
    num = num + out[2]
    f_y = function(y){
      f(c(a,y))
    }
    out = golden_sec(f_x, eps, epsm)
    b = out[1]
    num = num + out[2]
  }
  return(list('vec' = c(a,b),'num' = num))
}


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



golden_sec = function(f, eps, epsm){
  golden_r = (sqrt(5) - 1)/2
  #initialize
  a = rnorm(1) ; 
  b = a; c = a; i = 0
  f_a = f(a); f_b = f(b); f_c = f(c)
  while(((f_c >= f_b) | (f_c >= f_a)) & (i < 500)){
    b = b + 2^i 
    a = a - 2^i * golden_r 
    i = i + 1
    f_a = f(a); f_b = f(b); f_c = f(c)
  }
  # golden section 
  i = 0
  while((abs(a - b) > (eps + epsm * abs(c))) & (i < 1000)){
    i = i + 1;# print(c(a,b,c))
    temp = b + a - c
    if( f(temp) > f(c)){
      if(temp > c){
        b = temp
      }else{
        a = temp
      }
    }else{
      if( temp > c){
        a = c
      }else{
        b = c
      }
      c = temp
    }
  }
  return(c(c,i))
}
