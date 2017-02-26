#' @title Get Z0
#'
#' @param endogenous is the endogenous variables part of the data set
#' @param lags is the number of lags in levels in the model
#'
#' @return Z0 - an xts object

get_Z0 <- function(endogenous, lags) {
  Z0 <- diff(endogenous)
  Z0 <- Z0[-(1:lags),]
  return(Z0)
}
