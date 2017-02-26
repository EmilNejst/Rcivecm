#' @title add transitory shock to the data
#'
#'
#' @param data is the data frame that is to be used for modeling
#' @param shock_date is a date object with the date for the transitory shock
#' @param restricted is a boolean indicating whether the
#'
#' @return the input xts object with the added
#' @export

add_transitory_shock <- function(
  data,
  shock_date,
  restricted = FALSE
)

{
  transitory_shock <- xts::xts(rep(0,nrow(data)),index(data))
  transitory_shock[shock_date] <- 1
  transitory_shock[shock_date] <- -1

  colnames(transitory_shock) <- paste(ifelse(restricted,'xr_','x_','trans_'),
                                      format(shock_date,'%Y%m%d'))

  data <- merge(data, transitory_shock)

  return(data)
}
