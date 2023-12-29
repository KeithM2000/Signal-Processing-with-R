exp_wavelet <- function(len, n, res, alpha){
  
  mid = len%/%2
  nums = seq(from = -mid, to = mid)
  t = nums/len
  theta = pi*res*n
  x = complex(argument = -2*theta*t)*exp(-alpha*theta*abs(t))
  x = x*(theta*alpha)
  return(x)
  
}