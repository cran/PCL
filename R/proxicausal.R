#' This function is to compute mean squared error
#' @keywords internal
#' @return returns mean squared error
#' @export
MSE_func <- function(bridge_func, para, Y, W, Z){
  g0 <- bridge_func(para = para, Y = Y, W = W, Z = Z)
  g <- apply(g0, 2, mean)
  gmmf <- sum(g^2)
  return(gmmf)
}

#' This function is to compute estimating equation of outcome-inducing confounding bridge function 
#' @keywords internal
#' @return returns the sample level estimating equations for q function
#' @export
hbridge <- function(para, Y, W, Z) {
  hlink <- W %*% para
  g1 <- Z
  g <- c(Y - hlink) * g1
  return(g)
}


#' This function is to compute estimating equation of treatment-inducing confounding bridge function 
#' @keywords internal
#' @return returns the sample level estimating equations for q function
#' @export
qbridge <- function(para, Y, W, Z) { 
  tlink <- 1 + exp(Z %*% para)
  g0 <- W
  g <- c(tlink) * g0 - Y
  return(g)
}

