#' Plot QQ's and scatter together using residual and fitted data
#' 
#' @param data The linear model you want to plot
#' @return Dataframe
#' 
#' @export

info_plot <- function(lm_model){
  if(!inherits(lm_model, "lm")) {
    stop("Loud Wookie sounds......Must use a Linear model....RRWWWGG!")
  }  
  augment_crabs <- augment(lm_model)
  
  qq_crabs <- ggplot(augment_crabs, aes(sample = .resid)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = "qq plot of residuals")
  
  scatter_crabs <- ggplot(augment_crabs, aes(x = .fitted, y = .resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")
  
  combined_plot <- grid.arrange(qq_crabs,  scatter_crabs, ncol = 2, widths = c(1,2))
  
  return(combined_plot)
}