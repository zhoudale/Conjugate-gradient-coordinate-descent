# test example:
# golden_sec(f_1, eps, epsm)
# coordinate_des(f_3, eps, epsm)


f_1  = function(x){
  (x-5)^2 
}

f_2 = function(x){
  1/(x^2 + 1) + x^2
}

f_3 = function(x){
  (x[1]-2)^2 + (x[2] + 1)^2
}

f_4 = function(x){
  c(2*(x[1]-2),2*(x[2]+1))
}

f_5 = function(x,y){
  (x - 1)^2 + 3 * (y - x^2)^2
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

