dtfft <- function(x, N){
  if(N == 1){
    return(x[1])
  }
  else{
    len1 <- seq(1,length(x), 2)
    len2 <- seq(2, length(x), 2)
    x_odd <- x[c(len1)]
    x_even <- x[c(len2)]
    mid = N%/%2
    x_even <- dtfft(x_even, mid)
    x_odd <- dtfft(x_odd, mid)
    
    for(k in 1:mid){
      theta <- -2*pi*k/N
      p <- x_even[k]
      q <- complex(argument = theta)
      q <- q*x_odd[k]
      x[k] = p + q
      x[k + mid] = p - q
    }
    return(x)
    
  }
}