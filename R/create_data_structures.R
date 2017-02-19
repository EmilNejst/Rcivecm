#' @title Create a data_structures object for the cointegrated VECM
#'
#' @description This function creates a central object of the package called the model
#'              frame. The model frame holds all the relevant data structures needed
#'              for further analysis.
#'
#'              Using the input information on lags, deterministic trends, rank and
#'              exogenous variables, the model frame creates variables central for
#'              estimation and inference, such as the summarizing variables known
#'              from Johansen(1996) pp. 90 denoted, $Z_0$, $Z_1$ and $Z_2$, as
#'              well as the concentrated variables $R_0$ and $R_1$. Finally the
#'              cornerstone sums of squares $S_ij$ are calculated and stored.
#'
#'              This function is private to the package and will only be invoked through
#'              other functions serving as interfaces.
#'
#' @param data is an xts object with the data of interest. Exogenous variables are included
#'             by adding them to this data object and giving them a header name with the
#'             prefix \code{x_}. If an exogenous variable is restricted to lie within the
#'             cointegration relation, its name should have the prefix \code{xr_}
#'
#'             *Note* that certain exogenous variables, such as a constant
#'             can be introduced through the parameter \code{trend} and need not be specified
#'             as an exogenous variable. If the input data contains _missing values_,
#'             the row containing a missing value is deleted.
#'
#' @param lags are the number of lags (in levels - not differences) used in the model
#' @param rank is the cointegration rank for the system.
#'
#' @return a \code{model_frame} class holding the relevant data objects for further analysis.

create_data_structures <- function(data, lags = 1) {

  # Start by removing poential missing variables
  data <- data[complete.cases(data),]

  # Separate data into endogenous, exogenous and restricted exogenous variables
  variable_names <- colnames(data)
  endogenous_names <- variable_names[(stringr::str_sub(variable_names,1,2) != 'x_' &&
                                                stringr::str_sub(variable_names,1,2) != 'xr_')]
  dimension <- length(endogenous_names)

  has_exogenous <- (sum(stringr::str_sub(variable_names,1,2) == 'x_')==0)
  has_restricted <- (sum(stringr::str_sub(variable_names,1,2) == 'xr_') == 0)

  # Construct Z0, Z1 and Z2
  endogenous_1 <- lag(endogenous_variables)

  if(has_exogenous_variables) {
    exogenous_names <- variables_names[stringr::str_sub(variable_names,1,2)=='x_']
    exogenous <- data[,exogenous_names]
  }

  if(has_restricted_variables) {
    restricted_names <- variable_names[stringr::str_sub(variable_names,1,2) == 'xr_']
    restricted <- data[,restricted_names]
  }

  # Construct Z0
  endogenous_variables <- data[,endogenous_names]
  Z0 <- diff(endogenous)

  # Construct Z1
  Z1 <- lag(endogenous)

  if(has_restricted){
    restricted_1 <- lag(restricted_variables,1)
    Z1 <- cbind(Z1,restricted_1)
  }

  if(has_exogenous) {
    exogenous_1 <- lag(exogenous,1)
    Z1 <- cbind(Z1, exogenous_1)
  }

  # Construct Z2
  if(lags > 1){
    Z2 <- lag(Z0,k=1,na.pad = TRUE)

    if(has_exogenous) {
      D_exogenous <- diff(exogenous)
      D_exogenous_1 <- lag(D_exogenous_1, k = 1, na.pad = TRUE)
      Z2 <- merge(Z2, D_exogenous_1)
    }

    for(i in 3:(lags-1)) {
      D_endogenous_i <- lag(Z0, k = i-1, na.pad = TRUE)

      Z2 <- merge(Z2, D_endogenous_i)
      if(has_exogenous){
        D_exogenous_i <- lag(D_exogenous, k = i -1, na.pad = TRUE)
        Z2 <- merge(Z2, D_endogenous_i)
      }

    }
  }

  # Remove missing values from the beginning of the sample due to
  # model specification
  z0 <- Z0[-(1:lags),]
  Z1 <- Z1[-(1:lags),]
  Z2 <- Z2[-(1:lags),]

  # Effective Sample Size
  T <- nrow(data) - lags

  # Construct Mij for i=0,1,2 and j = 0,1,2
  M00 <- (1/T) * t(Z0) %*% Z0
  M01 <- (1/T) * t(Z0) %*% Z1
  M02 <- (1/T) * t(Z0) %*% Z2
  M11 <- (1/T) * t(Z1) %*% Z1
  M12 <- (1/T) * t(Z1) %*% Z2
  M22 <- (1/T) * t(Z2) %*% Z2

  # Construct R0 and R1
  R0 <- Z0 - M02 %*% solve(M22) %*% Z2
  R1 <- Z1 - M12 %*% solve(M22) %*% Z2

  # Construct Sij for i = 0,1 and j = 0,1
  S00 <- (1/T) * t(R0) %*% R0
  S01 <- (1/T) * t(R0) %*% R1
  S11 <- (1/T) * t(R1) %*% R1

  # Collect all the results into a list
  data_structures <- list(
    Z0 = Z0,
    Z1 = Z1,
    Z2 = Z2,
    R0 = R0,
    R1 = R1,
    S00 = S00,
    S01 = S01,
    S11 = S11
  )

  attr(data_structures, 'class') <- 'data_structures'

  return(data_structures)
}
