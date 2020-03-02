#' Functions to create plots
#' 
#' @param xval x input for the plot
#' @param yval y input for the plot
#' @param fillvall variable for filling barplots
#' @param colourval variable for colouring plots
#' @param tipo Character the plot to be created
#'

crea_plot <- function(xval, yval, fillval, colourval, tipo){
  if(tipo == "none"){
    return(NULL)
  }
  if(tipo == "density"){
    # g <- ggplot2::geom_density(ggplot2::aes((eval(xval))), fill = "#002B7A", colour = "#002B7A")
    g <- ggplot2::geom_density(ggplot2::aes_string(xval), fill = "#002B7A", colour = "#002B7A")
    return(g)
  }
  if(tipo == "barplot"){
    # g <- ggplot2::geom_bar(ggplot2::aes(eval(xval), fill = eval(ifelse(is.null(fillval), "1", fillval))))
    g <- ggplot2::geom_bar(ggplot2::aes_string(xval, fill = ifelse(is.null(fillval), "1", fillval)))
    return(g)
  }
  if(tipo == "scatter"){
    # g <- ggplot2::geom_point(ggplot2::aes(x = eval(xval), y = eval(yval), 
    #                                       colour = eval(ifelse(is.null(colourval), "1", colourval))))
    g <- ggplot2::geom_point(ggplot2::aes_string(x = xval, y = yval, 
                                          colour = ifelse(is.null(colourval), "1", colourval)))
  }
}

#' Function that selects "best" possible plot
#' 
#' @param data A dataframe
#' @param xval x input for the plot
#' @param yval y input for the plot
which_plot <- function(data, xval, yval){
  x <- which(names(data) == xval)
  y <- which(names(data) == yval)
  tmp <- list(x = data[,x], y = data[,y])
  
  if(is.null(xval) & is.null(yval)){
    return("none")
  }
  # if y is null we'll plot 1 variable
  if(is.null(yval)){
    if(is.numeric(tmp$x)){
      return("density")
    }
    else{
      return("barplot")
    }
  }
  if(is.numeric(tmp$x) & is.numeric(tmp$y)){
    return("scatter")
  }
}