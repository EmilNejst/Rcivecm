#' @title add trend of order trend_order
#'
#' @description A function for adding trends to the dataset. This
#'       function helps the user add deterministics to the original
#'       dataset prior to modeling. The function allow for adding,
#'       trends, broken trends and permanent dummies through the use
#'       of the start_date and end_date inputs. The only type of
#'       dummy that cannot be added using this function is the
#'       transitory dummy (a blip dummy). A specific function
#'       for handling that case is provided, see \code{add_transitory_dummy}.
#'
#'       The function is quite simple and can easily be replicated by
#'       the user. I adds a column to the data set named 'x_...'. If
#'       the user wishes to add the column on his own, this can be
#'       done without any complications for further analysis as long
#'       as the user applies correct naming principles, using the
#'       'x_' prefix.
#'
#' @param data the original dataset to which a trend should be added
#' @param trend_order the order of the trend
#' @param name is the name (excluding the prefix 'x_') of the constructed
#'        trend variable.
#' @param start_date the start date for the trend, default is the
#'        date of the first observation of the data table.
#' @param end_date the end date for the trend is the date at which
#'        the trend ends.
#' @param restricted a boolean variable to indicate whether the included
#'        trend is to be restricted to lie in the cointegration space.
#'        Default is FALSE.
#'
#' @return the input dataset with an added column including a trend
#'
#' @export

add_trend <- function(
  data,
  trend_order,
  name,
  start_date = head(index(data),1),
  end_date = tail(index(data),1),
  restricted = FALSE)

{
  trend <- rep(1,nrow(data))
  trend <- xts::xts(trend,index(data))

  if(trend_order >= 1) {
    for(i in 1:trend_order){
      trend <- cumsum(trend)
    }
  }
  colnames(trend) <- paste(ifelse(restricted,'xr_','x_'), name, sep="")
  data <- merge(data, trend)

  return(data)
}
