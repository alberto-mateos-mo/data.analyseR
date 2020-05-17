#' Function that produces simple explanation for lm model
#' 
#' @param object An lm object
#' @return

explain_lm <- function(object){
  # browser()
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
    explain <- c(explain, "-", unlist(details))
  }
  
  explain <- c(adj, explain)
  
  return(cat(explain, sep = "\n"))
  
}
