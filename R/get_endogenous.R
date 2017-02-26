#' @title Get endogenous variables from data set
#'
#' @param data is a data set with the name codes of
#'        the Rcivecm package
#'
#' @return an xts object with the endogenous variable parts
#'         of the original data

get_endogenous <- function(data){
  variable_names   <- colnames(data)
  endogenous_names <- variable_names[(str_sub(variable_names,1,2) != 'x_' &&
                                      str_sub(variable_names,1,2) != 'xr_')]
  endogenous <- data[,endogenous_names]
  return(endogenous)
}
