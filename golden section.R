######### golden section #########

golden_r = (sqrt(5) - 1)/2

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



