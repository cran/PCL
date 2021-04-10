#' @title Create a Proximal Causal Learning Object
#' @description Create a proximal causal learning object, usually used as a variable in a model function. Argument matching
#‘ is special for this function, see Details below.
#' @param outcome the outcome variable
#' @param trt the binary treatment variable
#' @param trt_pxy the treatment-inducing proxies 
#' @param out_pxy the outcome-inducing proxies
#' @param covariates the observed confounders
#' @return pcl returns an object of class "pcl", which wraps the treatment, outcome, treatment inducing confounding proxies, outcome inducing confounding proxies and other covariates
#' @export pcl
#' @examples
#' n <- 100
#' outcome <- rnorm(n, 0, 1)
#' trt <- rbinom(n, 1, 0.5)
#' trt_pxy <- rnorm(n, 0, 1)
#' out_pxy <- rnorm(n, 0, 1)
#' covariates <- rnorm(n, 0, 1)
#' pcl_object <- pcl(outcome, trt, trt_pxy, out_pxy, covariates)
pcl <- function (outcome,
                 trt,
                 trt_pxy,
                 out_pxy,
                 covariates) {
  res <- list(outcome = outcome,
              trt = trt,
              trt_pxy = trt_pxy,
              out_pxy = out_pxy,
              covariates = covariates)
  class(res) <- "pcl"
  return(res)
}


#' @title Fit a Proximal Causal Learning Model
#' @description Fit a proximal causal learning model
#' @param pcl_object an pcl object
#' @param method method used to fit
#' @return returns the average causal effect
#' @importFrom stats optim lm
#' @export pclfit
#' @examples
#' n <- 100
#' outcome <- rnorm(n, 0, 1)
#' trt <- rbinom(n, 1, 0.5)
#' trt_pxy <- matrix(rnorm(n, 0, 1), ncol = 1)
#' out_pxy <- matrix(rnorm(n, 0, 1), ncol = 1)
#' covariates <- matrix(rnorm(n, 0, 1), ncol = 1)
#' pcl_object <- pcl(outcome, trt, trt_pxy, out_pxy, covariates)
#' fit <- pclfit(pcl_object)
pclfit <- function (pcl_object, method = "POR") {
  W <- pcl_object$out_pxy
  Z <- pcl_object$trt_pxy
  A <- pcl_object$trt
  X <- pcl_object$covariates
  Y <- pcl_object$outcome
  N <- length(A)
  if (method == "POR") {
    h0_Y <- Y
    h0_W <- cbind(1, A, W, X)
    h0_Z <- cbind(1, A, Z, X)
    inioptim_b0 <- rep(0, 1 + 1 + ncol(W) + ncol(X))
    h <- optim(par = inioptim_b0, fn = MSE_func,
                bridge_func = hbridge, Y = h0_Y, W = h0_W, Z = h0_Z, 
                method = "BFGS", hessian = FALSE)
    b0 <- h$par
    
    POR_work_df <- data.frame(Y = c(cbind(1, 0, W, X) %*% b0, cbind(1, 1, W, X) %*% b0),
                              A = rep(c(0, 1), each = N))
    POR_fit <- lm(Y ~ A, data = POR_work_df)
    ate <- POR_fit$coefficients[2]
    
  } else if (method == "PIPW") {
    warning("Chosen methods not supported yet.")
  } else if (method == "PDR") {
    warning("Chosen methods not supported yet.")
  } else {
    warning("Chosen methods not supported yet.")
  }
  
  return(ate)
}

