
# golden_sec(f_1, eps, epsm)

# coordinate_des(f_3, eps, epsm)

f_1  = function(x){
  (x-5)^2 
}

f_2 = function(x){
  1/(x^2 + 1) + x^2
}

f_1 = function(x){
  (x[1]-2)^2 + (x[2] + 1)^2
}

g_f_3 = function(x){
  c(2*(x[1]-2),2*(x[2]+1))
}

f_4 = function(x,y){
  (x - 1)^2 + 3 * (y - x^2)^2
}

golden_r = (sqrt(5) - 1)/2

######### golden section #########

golden_sec = function(f, eps, epsm){
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

########### coordinate descent ##########

coordinate_des = function(f, eps,epsm){
  a = 0; b = 0; olda = 1; oldb = 1; i = 0
 while((dist(rbind(c(a,b),c(olda,oldb))) > (eps + epsm * abs(min(a,b)))) & (i < 5000)){
   i = i + 1
   olda = a; oldb = b
   f_x = function(x){
     f(x,b)
   }
   a = golden_sec(f_x, eps, epsm)[1]
   f_y = function(y){
     f(a,y)
   }
   b = golden_sec(f_y, eps, epsm)[1]
 }
 return(c(a,b,i))
}

##################### input #################
est = matrix(, nrow = 50, ncol = 2)
time = c()
for (i in 1:50){
  starttime = proc.time()
  est[i,] = golden_sec(f_1, 0.01, 0.000001)
  time[i] = (proc.time() - starttime)[3]
}
sqrt(var(est)); colMeans(est); sqrt(var(time));mean(time)
#2
est = matrix(, nrow = 50, ncol = 2)
time = c()
for (i in 1:50){
  starttime = proc.time()
  est[i,] = golden_sec(f_2, 0.01, 0.000001)
  time[i] = (proc.time() - starttime)[3]
}
sqrt(var(est)); colMeans(est); sqrt(var(time));mean(time)
#3
est = matrix(, nrow = 50, ncol = 3)
time = c()
for (i in 1:50){
  starttime = proc.time()
  est[i,] = coordinate_des(f_3, 0.01, 0.000001) + c(-2, 1 ,0)
  time[i] = (proc.time() - starttime)[3]
}
sqrt(var(est[,3])); mean(est[,3]); sqrt(var(time));mean(time)
dist = sqrt(est[,2]^2 + est[,1]^2)
mean(dist); sqrt(var(dist))
#4
est = matrix(, nrow = 50, ncol = 3)
time = c()
for (i in 1:50){
  starttime = proc.time()
  est[i,] = coordinate_des(f_4, 0.01, 0.000001) - c(1, 1 ,0)
  time[i] = (proc.time() - starttime)[3]
}
sqrt(var(est[,3])); mean(est[,3]); sqrt(var(time));mean(time)
dist = sqrt(est[,2]^2 + est[,1]^2)
mean(dist); sqrt(var(dist))
# extra
est = c();err = c()
for (j in 1:10){
  for (i in 1:50){
    est[i] = golden_sec(f_1, 2^(-j), 0.000001)[1]
  }
  err[j] = mean((est-5)^2)
}
plot(-1:-10, abs(err),'l', ylab = 'square- err',xlab = 'log2(epsilon)')

