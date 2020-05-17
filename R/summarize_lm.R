#' Function to run and summarize a linear regression model
#' 
#' @param formula An object of class formula
#' @param data A data frame with data
#' @param ... other params to be passed to lm()
#' @export

summarize_lm <- function(formula, data, ...){
  
  dep_var <- data %>% 
    dplyr::select((formula)[[2]]) %>% 
    unlist()
  
    m <- lm(formula = formula, data = data, ...)
    
    g <- ggplot2::autoplot(m)+
      ggplot2::theme_minimal()
    
    sum_df <- as.data.frame(summary(m)$coefficients)
    
    explain_lm(m)
    
    invisible(list(plots = g, summary_reg = sum_df, type = "lm"))
  
}
