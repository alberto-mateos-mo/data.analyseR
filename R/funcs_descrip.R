#' Function to create geoms
#' 
#' @param xval x input for the plot
#' @param yval y input for the plot
#' @param fillvall variable for filling barplots
#' @param colourval variable for colouring plots
#' @param tipo Character the plot to be created
#'

crea_plot <- function(xval, yval, fillval, colourval, tipo){
  if(tipo == "none" | is.null(tipo)){
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
                                          colour = ifelse(is.null(colourval), "1", colourval)), size = 3)
    return(g)
  }
  if(tipo == "g_density1"){
    g <- ggridges::geom_density_ridges(ggplot2::aes_string(x = xval, y = yval))
    
    return(g)
  }
  if(tipo == "g_density2"){
    g <- ggridges::geom_density_ridges(ggplot2::aes_string(x = yval, y = xval))
    
    return(g)
  }
  if(tipo == "ballon"){
    g <- ggplot2::geom_count(ggplot2::aes_string(x = xval, y = yval))
    
    return(g)
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
  if(is.numeric(tmp$x) & is.factor(tmp$y)){
    return("g_density1")
  }
  if(is.numeric(tmp$y) & is.factor(tmp$x)){
    return("g_density2")
  }
  if(is.factor(tmp$x) & is.factor(tmp$y)){
    return("ballon")
  }else{
    return(NULL)
  }
}

#' Custom ggplot2 theme
#' 
#' @export

d_theme <- function(){
  ggplot2::theme_minimal()+
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "#717171", arrow = grid::arrow(length = ggplot2::unit(1, "mm"))),
          axis.title.y = ggplot2::element_text(angle = 90, hjust = 1, size = 8, vjust = 3),
          axis.title.x = ggplot2::element_text(angle = 0, hjust = 1, size = 8, vjust = -1),
          axis.ticks = ggplot2::element_line(size = 0.25),
          legend.background = ggplot2::element_rect(colour = "grey"),
          legend.text.align = 0,
          legend.position = "right",
          legend.justification = c(1, 1),
          panel.grid.major = ggplot2::element_line(colour = "#CECBC3"),
          panel.grid.minor = ggplot2::element_line(colour = "#CECBC3", linetype = 2),
          plot.title = ggplot2::element_text(vjust = 2.5, hjust = 0, face = "bold", size = 14)
    )
}