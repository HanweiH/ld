update_log_pi <- function(B, piks = log(rep(1/ncol(B),ncol(B)))){

  if(is.null(B)){
    return (1)
  }

  n <- nrow(B)
  k <- ncol(B)


  tau <- matrix(1:(n*k),nrow = n,ncol = k)   #log(W_ik)

  for(i in 1:n){
    for(j in 1:k){
      tau[i,j] <- ((piks[j]+B[i,j]) - log_sum_exp(B[i,] +piks))
    }
  }

  new_log_pi <- c()
  temp <- 0
  for(h in 1:k){
    temp <- (-log(n) + log_sum_exp(tau[,h]))
    new_log_pi[h] <- temp
  }

  return(new_log_pi)
}
