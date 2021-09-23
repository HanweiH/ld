test_gl_to_gp<- function(A,pivec = rep(1/ncol(A),ncol(A)), tol = 10^(-5), recur_max = 100){

  if(is.null(A)){
    return(1)
  }

  err <- Inf
  iter <- 0 # initial times of iteration
  new_pivec <- pivec
  new_log_like <- log_like(A) # initial new_log_like with default values
  result <- c()
  result <- append(result,new_log_like)
  while(err>tol && iter< recur_max){
    old_log_like <- new_log_like # store old value
    new_pivec <- update_pi(A,new_pivec) # update pi
    new_log_like <- log_like(A,new_pivec)  # use updated pi to get new new_log_like
    stopifnot(new_log_like > old_log_like)
    err <- abs(new_log_like - old_log_like)
    iter <- iter + 1
    result <- append(result,new_log_like)
  }
  return(result)
}
