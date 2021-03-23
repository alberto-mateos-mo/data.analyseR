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
    actionButton(ns("other_plot"), "Next possible plot."),
    col_12(align = "center",
      plotly::plotlyOutput(ns("res"), width = "800px"),
      downloadButton(ns("plot_down"), label = "Download Plot")
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
    x <- input$test$target$x
    y <- input$test$target$y
    fill <- input$test$target$fill
    colour <- input$test$target$colour
    
    tipo <- which_plot(data = react(), xval = input$test$target$x, yval = input$test$target$y)
   
    tryCatch( g <- crea_plot(xval = x, yval = y, 
                             fillval = fill, colourval = colour, 
                             tipo = tipo, subtipo = input$other_plot), 
             error = function(e) stop("An error ocurred, we probably mis guessed variable type, may be you want to configure formats in DATA FORMAT tab."))
    
    g <- ggplot2::ggplot(react())+
      g+
      d_theme()
    
    return(list(gg = g, tipo = tipo))
  })
  
  output$res <- plotly::renderPlotly({
    plotly::ggplotly(plotplot()$gg)
  })
  
  # choices <- observe({
  #   tryCatch(
  #     if(plotplot()$tipo == "scatter-line"){
  #       return(c("points", "line"))
  #     }
  #     else{
  #       return(NULL)
  #     },
  #     error = function(e) NULL
  #   )
  # })

  observe({
    choices <- NULL
    tryCatch(
        if(plotplot()$tipo == "scatter-line"){
          choices <- c("points", "line")
        },
        error = function(e) NULL
      )
    # if(!is.null(choices())){
      updateSelectInput(session, "other_plot", choices = choices)
    # }
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
 
