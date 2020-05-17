#' Function to run and summarize a logistic regression model
#' 
#' @param formula An object of class formula
#' @param data A data frame with data
#' @param ... other params to be passed to glm()
#' @export

summarize_logistic <- function(formula, data, ...){
  
  dep_var <- data %>% 
    dplyr::select((formula)[[2]]) %>% 
    unlist()
  
  if(length(levels(dep_var)) > 2) stop("Dependent variable has more then two categories, try another model for your data")
  
  m <- glm(formula = formula, data = data, family = "binomial", ...)
  
  g <- ggplot2::autoplot(m)+
    ggplot2::theme_minimal()
  
  sum_df <- as.data.frame(summary(m)$coefficients)
  
  explain_logistic(m)
  
  invisible(list(plots = g, summary_reg = sum_df, type = "logistic (glm-binomial)"))

}