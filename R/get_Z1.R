#' @title Get Z1
#'
#' @param endogenous an xts object with the endogenous variables
#' @param exogenous an xts object with the exogenous variables
#' @param restricted an xts object with the exogenous variables
#'                   that are restricted to lie in the cointegration space
#' @param lags the number of lags in levels in the model
#'
#' @return Z1 - an xts object

get_Z1 <- function(endogenous, exogenous, restricted, lags) {
  Z1 <- lag(endogenous)

  if(!is.null(restricted)){
    restricted_1 <- stats::lag(restricted,1)
    Z1 <- cbind(Z1,restricted_1)
  }

  if(!is.null(exogenous)) {
    exogenous_1 <- lag(exogenous,1)
    Z1 <- cbind(Z1, exogenous_1)
  }

  Z1 <- Z1[-(1:lags),]
  return(Z1)
}
