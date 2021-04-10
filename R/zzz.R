#' @keywords internal
.onAttach <- function(libname, pkgname){
  msg <- paste("Thank you for using our package. Currently we only implemented proximal two-stage least squares algorithm in 'An Introduction to Proximal Causal Learning'.")
  packageStartupMessage(msg)
  msg <- paste("Other method including PIPW, PDR, longitudinal setting, nonparametric estimation will be updated soon.")
  packageStartupMessage(msg)
  msg <- paste0('We politely ask you to cite our package using: citation("', pkgname, '"). It will be appreciated. ')
  packageStartupMessage(msg)
}