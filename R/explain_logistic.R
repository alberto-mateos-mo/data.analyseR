#' Function that produces simple explanation for logistic model
#' 
#' @param object An glm-binomial object
#' @return

explain_logistic <- function(object){
  if(object$family$family != "binomial") stop("This function onl works with glm-binomial models")
  
  coefs_df <- as.data.frame(summary(object)$coefficients)
  
  vars <- rownames(coefs_df)
  
  coefs_df <- data.frame("Variable" = vars, coefs_df)
  
  coefs_df <- coefs_df %>% dplyr::mutate(significative = ifelse(`Pr...z..` <= 0.05, 1, 0))
  
  n_sig <- sum(coefs_df$significative)
  
  if(n_sig == 0){
    explain <- "Your model doesn't have any significative variables at 0.05 level."
  }else if(n_sig != 0){
    
    dep <- names(object$model)[1]
    
    outcome <- levels(unlist(object$model[1]))[2]
    
    intercept <- coefs_df$Estimate[1]
    
    coefs_df <- dplyr::filter(coefs_df, Variable != "(Intercept)")
    
    pos_vars <- coefs_df$Variable[which(coefs_df$Estimate >= 0)]
    
    neg_vars <- coefs_df$Variable[which(coefs_df$Estimate < 0)]
    
    explain <- paste0("Base probability to get an outcome of ", outcome, " in ", dep, ": ", round(1/(1+exp(-intercept)), 3))
    
    explain <- c(explain,
                 paste0("The following variables makes more likely to get an outcome of ", outcome, " in ", dep, ": ",
                      paste0(pos_vars, collapse = ", ")))
    
    explain <- c(explain, 
                 paste0("The following variables makes less likely to get an outcome of ", outcome, " in ", dep, ": ",
                        paste0(neg_vars, collapse = ", ")))
    
  }
  
  return(cat(explain, sep = "\n"))

}
