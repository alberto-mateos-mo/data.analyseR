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
    col_12(
      h4("Explore your data.")
    ),
    esquisse::dragulaInput(ns("test"), sourceLabel = "Variables", targetsLabels = c("x", "y", "fill", "colour"), 
                           choices = c(""), replace = TRUE),
    col_12(align = "center",
      downloadButton(ns("plot_down"), label = "Download Plot"),
      plotOutput(ns("res"), width = "800px")
    )
  )
}
    
# Module Server
    
#' @rdname mod_data_descrip
#' @export
#' @keywords internal
    
mod_data_descrip_server <- function(input, output, session, react){
  ns <- session$ns
  
  observe({
    esquisse::updateDragulaInput(session, "test", choiceValues = names(react()), choiceNames = badgeType(col_name = names(react())),
                                 badge = FALSE)
  })
  
   plotplot <- reactive({
    # x <- ifelse(is.null(input$test$target$x), 0, rlang::parse_expr(input$test$target$x))
    x <- input$test$target$x
    # y <- ifelse(is.null(input$test$target$y), 0, rlang::parse_expr(input$test$target$y))
    y <- input$test$target$y
    # fill <- ifelse(is.null(input$test$target$fill), 0, rlang::parse_expr(input$test$target$fill))
    fill <- input$test$target$fill
    # colour <- ifelse(is.null(input$test$target$colour), 0, rlang::parse_expr(input$test$target$colour))
    colour <- input$test$target$colour
    
    tipo <- which_plot(data = react(), xval = input$test$target$x, yval = input$test$target$y)
   
    tryCatch( g <- crea_plot(xval = x, yval = y, 
                             fillval = fill, colourval = colour, 
                             tipo = tipo), 
             error = function(e) stop("An error ocurred, we probably mis guessed variable type, may be you want to configure formats in DATA FORMAT tab."))
    
    g <- ggplot2::ggplot(react())+
      g+
      d_theme()
    
    # plotly::ggplotly(g)
    g
  })
  
  output$res <- renderPlot({
    plotplot()
  })
  
  output$plot_down <- downloadHandler(
    filename = function() "plot.png",
    content = function(file){
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 600, units = "in")
      ggplot2::ggsave(file, plot = plotplot(), device = device)
    }
  )
  
}
    
## To be copied in the UI
# mod_data_descrip_ui("data_descrip_ui_1")
    
## To be copied in the server
# callModule(mod_data_descrip_server, "data_descrip_ui_1")
 
