twiddle_factors <- function(N){
  m <- N%/%2
  n <- log2(N)
  p2 = -2*pi
  twiddles = matrix(0, nrow=m, ncol = n)
  for(i in 1:m){
    ex = ceiling(log2(i))
    end = n-ex
    rx = vector(length=(end))
    p3 = p2*i
    if(twiddles[i, ex+1] == 0){
      pow = 2^(ex + 1)
      for(j in 1:end){
        rx[j] = complex(argument = p3/pow)
        pow = pow*2
      }
      twiddles[i, c((ex+1):n)] = rx
      v = 2*i
      while(v <= m){
        twiddles[v, c((ceiling(log2(v))+1):n)] = rx[c(1:(n-ceiling(log2(v))))]
        v = v*2
      }
    }
  }
  return(twiddles)
}
