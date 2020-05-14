#' Function to run regression models
#' 
#' @param formula An object of class formula
#' @param data A data frame with data
#' @param ... other params to be passed to lm() or glm()
#' @export

run_regression <- function(formula, data, ...){
  
  dep_var <- data %>% 
    dplyr::select((formula)[[2]]) %>% 
    unlist()
  
  if(class(dep_var) %in% c("numeric", "integer")){
    
    m <- lm(formula = formula, data = data, ...)
    
    g <- ggplot2::autoplot(m)+
      ggplot2::theme_minimal()
    
    sum_df <- as.data.frame(summary(m)$coefficients)
    
    explain_lm(m)
    
    invisible(list(plots = g, summary_reg = sum_df, type = "lm"))
    
  } else if(class(dep_var) == "factor"){
    if(length(levels(dep_var)) > 2) stop("Try another model for your data")
    
    m <- glm(formula = formula, data = data, family = "binomial", ...)
    
    g <- ggplot2::autoplot(m)+
      ggplot2::theme_minimal()
    
    sum_df <- as.data.frame(summary(m)$coefficients)
    
    invisible(list(plots = g, summary_reg = sum_df, type = "logistic (glm-binomial)"))
    
  }
  
}
