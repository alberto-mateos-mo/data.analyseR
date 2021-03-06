#' Runs a summarised linear regression model
#' 
#' @param formula An object of class formula
#' @param data A data frame with data
#' @param ... other params to be passed to lm()
#' @import ggfortify
#' @export

summarised_lm <- function(formula, data, ...){
  
  dep_var <- data %>% 
    dplyr::select((formula)[[2]]) %>% 
    unlist()
  
  m <- lm(formula = formula, data = data, ...)
  
  g <- ggplot2::autoplot(m)+
    ggplot2::theme_minimal()
  
  sum_df <- as.data.frame(summary(m)$coefficients)
  
  explain_lm(m)
  
  invisible(list(model = m, plots = g, summary_reg = sum_df, type = "lm"))
  
}

#' Provides a simple explanation for a linear model (lm)
#' 
#' @param object An lm object
#' @noRd

explain_lm <- function(object){
  
  df <- as.data.frame(summary(object)$coefficients)
  
  vars <- rownames(df)
  
  df <- data.frame("Variable" = vars, df)
  
  df <- df %>% dplyr::mutate(significative = ifelse(`Pr...t..` <= 0.05, 1, 0))
  
  betas <- df[,2]
  
  names(betas) <- vars
  
  dep <- names(object$model)[1]
  
  adj <- paste("Your model has an adjusted r-squared of:", round(summary(object)$adj.r.squared, 2))
  
  n_sig <- sum(df$significative)
  
  if(n_sig == 0){
    explain <- "Your model doesn't have any significative variables at 0.05 level."
  }else if(n_sig != 0){
    explain <- paste("Your model has", n_sig, "significative variables at 0.05 level.")
    
    details <- lapply(seq_along(betas), 
                      function(x, i){
                        if(i == 1) paste("Base level for", dep, "is", round(x[1], 2))
                        else if(i != 1){
                          paste("Every increment in", names(betas)[i], "produces an increment of", round(betas[i], 2), "units in", dep)
                        }
                      },
                      x = betas)
    explain <- c(explain, "-", unlist(details), "*caeteris paribus*")
  }
  
  explain <- c(adj, explain)
  
  return(cat(explain, sep = "\n"))
  
}

#' Runs a summarised logistic regression model
#' 
#' @param formula An object of class formula
#' @param data A data frame with data
#' @param ... other params to be passed to glm()
#' @import ggfortify
#' @export
#' 

summarised_logistic <- function(formula, data, ...){
  
  dep_var <- data %>% 
    dplyr::select((formula)[[2]]) %>% 
    unlist()
  
  if(length(levels(dep_var)) > 2) stop("Dependent variable has more then two categories, try another model for your data")
  
  m <- glm(formula = formula, data = data, family = "binomial", ...)
  
  g <- ggplot2::autoplot(m)+
    ggplot2::theme_minimal()
  
  sum_df <- as.data.frame(summary(m)$coefficients)
  
  explain_logistic(m)
  
  invisible(list(model = m, plots = g, summary_reg = sum_df, type = "logistic (glm-binomial)"))
  
}

#' Provides a simple explanation for a logistic model
#' 
#' @param object An glm-binomial object
#' @noRd

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
