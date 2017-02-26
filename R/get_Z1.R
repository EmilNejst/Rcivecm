#' @title Get Z1
#'
#' @importFrom stats lag
#'
#' @return Z1

get_Z1 <- function(endogenous, exogenous, restricted, lags) {
  Z1 <- lag(endogenous)

  if(!is.null(restricted)){
    restricted_1 <- lag(restricted,1)
    Z1 <- cbind(Z1,restricted_1)
  }

  if(!is.null(exogenous)) {
    exogenous_1 <- lag(exogenous,1)
    Z1 <- cbind(Z1, exogenous_1)
  }

  Z1 <- Z1[-(1:lags),]
  return(Z1)
}
