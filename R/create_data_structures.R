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
#'
#' @importFrom stats complete.cases
#' @importFrom stats lag
#'
#' @return a \code{model_frame} class holding the relevant data objects for further analysis.

create_data_structures <- function(data, lags = 1) {
  # Start by removing poential missing variables
  data <- data[complete.cases(data),]

  # Separate data into endogenous, exogenous and restricted exogenous variables
  endogenous <- get_endogenous(data)
  exogenous  <- get_exogenous(data)
  restricted <- get_restricted(data)
  dimension  <- ncol(endogenous)

  has_exogenous  <- ifelse(is.null(exogenous),1,0)
  has_restricted <- ifelse(is.null(restricted),1,0)

  # Construct Z0
  Z0 <- get_Z0(endogenous, lags)
  Z1 <- get_Z1(endogenous, exogenous, restricted, lags)
  Z2 <- get_Z2(endogensou, exogenous, lags)

  # Effective Sample Size
  T <- nrow(Z0)

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
