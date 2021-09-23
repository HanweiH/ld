update_pi <- function(A,piks = rep(1/(ncol(A)+1),ncol(A)+1)){
  # Estimate pi by MLE, which pi is the probability of the probability of genetype
  # A is a matrix, and k should be the ploidy

  # Check if A is null.
  if(is.null(A)){
    return (1)
  }
  n <- nrow(A)  # get the number of rows of matrix A, the sample size
  k <- ncol(A)  # get the many of genetype
  piks_new <- c() # to store new piks and return results

  for(h in 1:k){ # for each pi_k(element of new pik_new)
    temp <-  0
    for(i in 1:n){

      # Try to get the bottom of formula
      bottom <- 0            # initialize the bottom.
      for(j in 1:k){
        bottom = bottom + piks[j] * A[i,j]
      }
      temp = temp + ((piks[h] * A[i,h])/bottom)
    }
    piks_new = append(piks_new,temp/n)
  }
  return(piks_new)
}
