#' Clean Na values from a data set
#' 
#' @param data The dataset you'd like to clean
#' @param column_name The column you would like to clean
#' @return Dataframe
#' 
#' @export

remove_nas <- function(data, column_name){
  clean <- data %>% 
    na.omit() %>% 
    select(({{ column_name }})) 
  if (sum(is.na(clean)) == 0){
    return(clean)
  } else {
    print("NAs still present!")
  }
}