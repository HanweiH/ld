log_like <- function(A, piks = rep(1/ncol(A),ncol(A))){
  if(is.null(A)){
    return (1)
  }
  n <- nrow(A)
  k <- ncol(A)
  temp <- 0

  for(i in 1:n){
    inner <- 0
    for(j in 1: k){
      inner <- inner + A[i,j] * piks[j]
    }
    temp = temp + log(inner)
  }

  return(temp)
}
