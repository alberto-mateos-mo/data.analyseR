#' Runs a summarised PCA analysis
#' 
#' @param x A dataframe containing variables to reduce
#' @param ... arguments to be passed to prcomp
#' @export

summarised_pca <- function(x, ...){
  res <- prcomp(x, ...)
  scrplot <- factoextra::fviz_eig(res)
  indivplot <- factoextra::fviz_pca_ind(res, col.ind = "cos2")
  varsplot <- factoextra::fviz_pca_var(res, col.var = "contrib")
  
  return(list(pca = res, scrplot = scrplot, indivplot = indivplot, varsplot = varsplot))
}

#' Provides insights about PCA results
#' 
#' @param x An object of class prcomp
#' @noRd

explain_pca <- function(x){
  
  if(class(x) != "prcomp") stop("You can only use this function with an object of class prcomp")
  
  var_exp <- diag(cov(x$x))/sum(diag(cov(x$x)))
  
  iv <- which(var_exp >= 0.1)[length(which(var_exp >= 0.1))]
  
  cv <- which(cumsum(var_exp)>=0.75)[1]
  
  if(iv == cv){
    explain <- c(paste("The number of principal components that explain at least 10% of variance each is:", iv),
                 paste("The number of components that explain at least 70% of cumulative variance is:", cv),
                 paste("Your PCA results show that you need", iv, "components."))
  }else if(iv != cv){
    explain <- c(paste("The number of principal components that explain at least 10% of variance each is:", iv),
                 paste("The number of components that explain at least 70% of cumulative variance is:", cv),
                 paste("Your PCA results show that you need between",  min(c(iv, cv)), "and", max(c(iv, cv)), "components."))
  }
  
  return(cat(explain, sep = "\n"))
  
}