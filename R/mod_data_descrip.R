# Module UI
  
#' @title   mod_data_descrip_ui and mod_data_descrip_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_descrip
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_data_descrip_ui <- function(id){
  ns <- NS(id)
  tagList(
    esquisse::dragulaInput(ns("test"), sourceLabel = "Variables", targetsLabels = c("x", "y", "fill", "colour"), 
                           choices = c("")),
    plotOutput(ns("res"))
  )
}
    
# Module Server
    
#' @rdname mod_data_descrip
#' @export
#' @keywords internal
    
mod_data_descrip_server <- function(input, output, session, react){
  ns <- session$ns
  
  observe({
    esquisse::updateDragulaInput(session, "test", choices = names(react()))
  })
  
  output$res <- renderPlot({
    x <- ifelse(is.null(input$test$target$x), 0, rlang::parse_expr(input$test$target$x))
    y <- ifelse(is.null(input$test$target$y), 0, rlang::parse_expr(input$test$target$y))
    fill <- ifelse(is.null(input$test$target$fill), 0, rlang::parse_expr(input$test$target$fill))
    colour <- ifelse(is.null(input$test$target$colour), 0, rlang::parse_expr(input$test$target$colour))
    
    tipo <- which_plot(data = react(), xval = input$test$target$x, yval = input$test$target$y)
    g <- crea_plot(xval = x, yval = y, 
                   fillval = fill, colourval = colour, 
                   tipo = tipo)
    ggplot2::ggplot(react())+
      g
  })
}
    
## To be copied in the UI
# mod_data_descrip_ui("data_descrip_ui_1")
    
## To be copied in the server
# callModule(mod_data_descrip_server, "data_descrip_ui_1")
 
