#' Function for fun
#' 
#' @param data The dataset you want to utilize
#' @param column_name The column you want to clean
#' @return Error
#' 
#' @export

#meme <- function (img) { 
# library(jpeg)
#  img <- readJPEG("data/1n7w77.jpg")
#  plot(1, type = "n", xlim = c(0, 10), ylim = c(0, 10))
#  rasterImage(img, 0, 0, 10, 10)
  
#}

#meme()

Fun_function <- function(crabs, column_name) {
  if (!column_name %in% names(crabs)) {
    stop(meme())
  }
  
  crabs_ordered <- crabs %>%
    arrange(across(({{column_name}})))
  
  return(crabs_ordered)
}