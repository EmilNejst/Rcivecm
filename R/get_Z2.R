#' @title Get Z2
#'
#' @param endogenous is the endogenous variables part of the data
#' @param exogensous is the exogenous variables part of the data
#' @param lags is the number of lags in levels
#'
#' @return Z2 - an xts object

get_Z2 <- function(endogenous, exogenous, lags) {

  if(lags > 1){
    Z2 <- lag(Z0, k = 1, na.pad = TRUE)

    if(!is.null(exogenous)) {
      D_exogenous   <- diff(exogenous)
      D_exogenous_1 <- stats::lag(D_exogenous_1, k = 1, na.pad = TRUE)
      Z2            <- merge(Z2, D_exogenous_1)
    }

    for(i in 3:(lags-1)) {
      D_endogenous_i <- lag(Z0, k = i-1, na.pad = TRUE)

      Z2 <- merge(Z2, D_endogenous_i)
      if(has_exogenous){
        D_exogenous_i <- lag(D_exogenous, k = i -1, na.pad = TRUE)
        Z2            <- merge(Z2, D_endogenous_i)
      }

    }
  } else
  {
    Z2 <- NULL
  }

  return(Z2)
}
