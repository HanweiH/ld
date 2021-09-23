log_gl_to_gp <- function(B,pivec = log(rep(1/ncol(B),ncol(B))),tol = 10^(-5), recur_max = 100){

  if(is.null(B)){

    return (1)
  }

  n <- nrow(B)
  err <- Inf
  iter <- 0 # initial times of iteration
  new_pivec <- pivec
  new_log_like <- log_likelihood_forB(B)
  while(err>tol && iter< recur_max){
    old_log_like <- new_log_like # store old value
    new_pivec <- update_log_pi(B,new_pivec) # update pi
    new_log_like <- log_likelihood_forB(B,new_pivec)  # use updated pi to get new new_log_like
    stopifnot(new_log_like > old_log_like)
    err <- abs(new_log_like - old_log_like)
    iter <- iter + 1
  }
  return(new_pivec)
}

