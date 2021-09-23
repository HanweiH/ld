get_expected_value <- function(matrix_a){
  n <- nrow(matrix_a)
  pik <- rep(1/ncol(matrix_a),ncol(matrix_a))

  # get list of probability_of_k_given_data
  dik <- get_probability(matrix_a)

  # calculate expected value
  expected_value <- c() # get E for each Gi
  for(i in 1:ncol(matrix_a)){  #for Gi
    temp <- 0
    for(j in 1:i){
      temp = temp + (j* dik[j]) # E for gtype k
    }
    expected_value[i] <- temp # add to list

  }
  return(expected_value)
}
