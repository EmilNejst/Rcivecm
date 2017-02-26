#' @title Extract exogenous variables from an xts data object
#'
#' @param data is an xts object with variable names following
#'        the scheme in Rcivecm
#'
#' @importFrom stringr str_sub
#'
#' @return an xts object with the exogenous variable parts of the original data.

get_exogenous <- function(data) {
  variable_names      <- colnames(data)

  if(sum((str_sub(variable_names,1,2) == 'x_'))==0) {

    exogenous_variables <- NULL

  } else {

    exogenous_names     <- variable_names[(str_sub(variable_names,1,2) == 'x_')]
    exogenous_variables <- data[,exogenous_names]

  }

  return(exogenous_variables)
}
