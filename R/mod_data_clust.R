# Module UI
  
#' @title   mod_data_clust_ui and mod_data_clust_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_clust
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_data_clust_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Explore clustering patterns in your data."),
    fluidRow(
      col_4(
        h6("Note: we are automatically selecting numeric variables only."),
        # shinyWidgets::radioGroupButtons(inputId = ns("type"), label = "Model:", choices = c("k-means", "hierarchical"), status = "primary"),
        selectizeInput(ns("vars"), "Variables to use:", choices = NULL, multiple = TRUE),
        shiny::checkboxInput(ns("scale"), "Use scaled data", value = TRUE),
        numericInput(ns("centers"), "Number of clusters:", value = NULL),
        actionButton(ns("run"), "Run Clustering."),
        h4("")
      ),
      col_6(
        plotly::plotlyOutput(ns("plot")),
        verbatimTextOutput(ns("info"))
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_data_clust
#' @export
#' @keywords internal
    
mod_data_clust_server <- function(input, output, session, react){
  ns <- session$ns
  
  df <- reactive({
    react() %>% 
      dplyr::select_if(is.numeric) %>% 
      na.omit()
  })
  
  observe({
    updateSelectizeInput(session, "vars", choices = names(df()))
    updateNumericInput(session, "centers", value = round(sqrt(nrow(df()))))
  })
  
  results <- eventReactive(input$run, {
    if(is.null(input$vars)){
      return(summarised_km(df(), scale. = input$scale, centers = input$centers))
    }else if(!is.null(input$vars)){
      i <- which(names(df()) %in% input$vars)
      df <- df()[,i]
      return(summarised_km(df, scale. = input$scale, centers = input$centers))
    }
    
  })
  
  output$plot <- plotly::renderPlotly({
    plotly::ggplotly(results()$plot)
  })
  
  clust_message <- eventReactive(input$run, {
    cat("An optimal clustering will show a differentiated behaviour among the variables used.")
  })
  
  output$info <- renderPrint({
    clust_message()
  })
  
}
    
## To be copied in the UI
# mod_data_clust_ui("data_clust_ui_1")
    
## To be copied in the server
# callModule(mod_data_clust_server, "data_clust_ui_1")
 
