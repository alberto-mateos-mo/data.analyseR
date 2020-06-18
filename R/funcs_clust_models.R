#' Runs a summarised k-means algorithm
#' 
#' @param x Dataframe with data to be clustered
#' @param scale. Whether to scale data or not
#' 

summarised_km <- function(x, scale. = TRUE, centers = 2, nstart = 25){
  if(scale. == TRUE) x <- as.data.frame(scale(x))
  
  km <- kmeans(x, centers = centers, nstart = nstart)
  
  x <- cbind(x, cluster = km$cluster)
  
  g <- x %>% 
    dplyr::group_by(cluster) %>% 
    dplyr::summarise_all(mean) %>% 
    tidyr::pivot_longer(-cluster) %>% 
    ggplot2::ggplot(., ggplot2::aes(name, value, colour = cluster, group = cluster)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Variable", y = "Average value")+
    ggplot2::theme_minimal()
  
  return(list(model = km, plot = g))
    
}