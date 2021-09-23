get_probability <- function(matrix_a){
  n <- nrow(matrix_a)
  pik <- rep(1/ncol(matrix_a),ncol(matrix_a))

  # get sum of a_ik * pi_k
  sum <- 0
  for(i in 1:n){
    for(j in 1:ncol(matrix_a)){
      sum <- sum + (matrix_a[i,j]*pik[j])
    }
  }

  # get probability
  dik <- c()
  for(i in 1:n){
    for(j in 1:ncol(matrix_a)){
      dik[j] <- (matrix_a[i,j]*pik[j]/sum) # list of p_k_givenData
    }
  }
  return(dik)
}
