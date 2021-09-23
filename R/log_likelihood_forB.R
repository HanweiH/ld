log_likelihood_forB <- function(B, pik = log(rep(1/ncol(B),ncol(B)))){

  if(is.null(B)){
    return (1)
  }

  n <- nrow(B)
  k <- ncol(B)
  temp <- 0

  for(i in 1:n){

      temp <- temp + log_sum_exp(pik+B[i,])

  }

  return(temp)

}
