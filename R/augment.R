#' Order data by some variable
#' 
#' @param data The dataset you'd like to order
#' @param column_name The column by which you'd like to order
#' @return Dataframe
#' 
#' @export

order_data <- function(crabs, column_name) {
  if (!column_name %in% names(crabs)) {
    stop("These are not the columns you are looking for")
  }
  
  crabs_ordered <- crabs %>%
    arrange(across(({{column_name}})))
  
  return(crabs_ordered)
}