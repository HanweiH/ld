log_sum_exp <- function(list){ #get a vector of
  b_max <- max(list)
  temp <- 0
  for(k in 1:length(list)){
    temp = temp + exp(list[k] - b_max)
  } # get the sum inner log
  result <- b_max + log(temp) # return a value
  return(result)
}

