
wavelet_transform <- function(x, len, iter, res){
  output <- matrix(nrow = iter, ncol = (length(x)+len))
  for(n in 1:iter){
    wavelet <- exp_wavelet(len, n, res, 0.2)
    output_vec <- convolve(wavelet, x, type = "open")
    output[n,] = output_vec
  }
  return(output)
}

exp_wavelet <- function(len, n, res, alpha){
  
  mid = len%/%2
  nums = seq(from = -mid, to = mid)
  t = nums/len
  theta = pi*res*n
  x = complex(argument = -2*theta*t)*exp(-alpha*theta*abs(t))
  x = x*(theta*alpha)
  return(x)
  
}