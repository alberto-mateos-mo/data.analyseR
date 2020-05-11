#' Function to run PCA analysis
#' 
#' @param x A dataframe containing variables to reduce
#' @param ... arguments to be passed to prcomp
#' @export

reduce_pca <- function(x, ...){
  res <- prcomp(x, ...)
  scrplot <- factoextra::fviz_eig(res)
  indivplot <- factoextra::fviz_pca_ind(res, col.ind = "cos2")
  varsplot <- factoextra::fviz_pca_var(res, col.var = "contrib")
  
  return(list(pca = res, scrplot = scrplot, indivplot = indivplot, varsplot = varsplot))
}