
wavelet_transform <- function(x, len, iter, res){
  output <- matrix(nrow = iter, ncol = (length(x)+len))
  for(n in 1:iter){
    wavelet <- SWG(len, n, res)
    output_vec <- convolve(wavelet, x, type = "open")
    output[n,] = output_vec
  }
  return(output)
}

SWG <- function(len, n){
  
  mid = len%/%2
  nums = seq(from = -mid, to = mid)
  t_neg = nums[c(1:mid)]/1000
  t_pos = nums[c((mid+2):(len+1))]/1000
  x_neg = complex(argument = -2*pi*t_neg)*sin((2^n)*pi*t_neg)/((2^n)*pi*t_neg)
  x_pos = complex(argument = -2*pi*t_pos)*sin((2^n)*pi*t_pos)/((2^n)*pi*t_pos)
  x_zero = 1 +0i
  
  x = append(x_neg, x_zero)
  x = append(x, x_pos)
  x = x *(2^(n/2))
  
}