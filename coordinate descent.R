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