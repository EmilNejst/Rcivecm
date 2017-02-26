#' @title Get exogenous variables restricted to the cointegration vector
#'
#' @param data is an xts object with the naming conventions of
#'        Rcivecm.
#'
#' @return an xts object with the exogenous variables that
#'         are restricted to the cointegration space

get_restricted <- function(data) {
  variable_names   <- colnames(data)

  if(sum(str_sub(variable_names,1,2) == 'xr_')==0) {

    restricted_names = NULL

  } else {

    restricted_names <- variable_names[(str_sub(variable_names,1,2) == 'xr_')]
    restricted_variables <- data[,restricted_names]

  }
  return(restricted_variables)
}
