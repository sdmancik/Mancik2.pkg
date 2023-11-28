#' Create linear models from any data set and variables
#' 
#' @param data The dataset you'd like to create a linear model from
#' @param Dependant_var Whatever dependent variable you want to use
#' @param Independant_var whatever independent variable you want to use
#' @return Dataframe
#' 
#' @export

create_lm <- function(data, dependent_var, independent_var) {
  if (!dependent_var %in% names(data) || any(!independent_var %in% names(data))) {
    stop("This is the way...but try another variable")
  }
  
  fmlm <- reformulate(independent_var, response = dependent_var)
  
  lm_model <- lm(fmlm, data = data)
  print(summary(lm_model))
  
  return(lm_model)
}