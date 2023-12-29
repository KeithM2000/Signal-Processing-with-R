
SWG <- function(len, n, res){
  
  mid = len%/%2
  nums = seq(from = -mid, to = mid)
  t_neg = nums[c(1:mid)]/len
  t_pos = nums[c((mid+2):(len+1))]/len
  x_neg = complex(argument = -2*pi*t_neg)*sin((res*n)*2*pi*t_neg)/((res*n)*2*pi*t_neg)
  x_pos = complex(argument = -2*pi*t_pos)*sin((res*n)*2*pi*t_pos)/((res*n)*2*pi*t_pos)
  x_zero = 1 +0i
  
  x = append(x_neg, x_zero)
  x = append(x, x_pos)
  x = x *sqrt(res*n)
  
}