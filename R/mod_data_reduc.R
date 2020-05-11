# Module UI
  
#' @title   mod_data_reduc_ui and mod_data_reduc_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_reduc
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_data_reduc_ui <- function(id){
  ns <- NS(id)
  tagList(
    # inputPanel(
    #   selectInput(ns("t_red"), "Select reduction method", choices = c("PCA", "KPCA", "tSNE", "Factor Analysis"))
    # ),
    fluidRow(
      col_4(
        h4("Here you can perform PCA analysis to your data."),
        h6("Note: we are automatically selecting numeric variables only."),
        actionButton(ns("run"), "Run PCA."),
        h4(""),
        selectizeInput(ns("vars"), "Variables to use:", choices = NULL, multiple = TRUE),
        selectInput(ns("plot"), "Select the plot you want to see:", choices = c("Screeplot", "Individuals Plot", "Variables Plot"))
      ),
      col_6(
        plotOutput(ns("pca")),
        textOutput(ns("explain"))
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_data_reduc
#' @export
#' @keywords internal
    
mod_data_reduc_server <- function(input, output, session, react){
  ns <- session$ns
  
  df <- reactive({
    react() %>% 
      dplyr::select_if(is.numeric) %>% 
      na.omit()
  })
  
  observe({
    updateSelectizeInput(session, "vars", choices = names(df()))
  })
  
  results <- eventReactive(input$run, {
    # if(input$t_red == "PCA"){
    #   return()
    # }
    if(is.null(input$vars)){
      return(reduce_pca(df(), retx = TRUE))
    }else if(!is.null(input$vars)){
      i <- which(names(df()) %in% input$vars)
      df <- df()[,i]
      return(reduce_pca(df, retx = TRUE))
    }
    
  })
  
  output$pca <- renderPlot({
    if(input$plot == "Screeplot"){
      plot(results()$scrplot)
    }
    if(input$plot == "Individuals Plot"){
      plot(results()$indivplot)
    }
    if(input$plot == "Variables Plot"){
      plot(results()$varsplot)
    }
  })
  
  output$explain <- renderPrint({
    explain_pca(results()$pca)
  })
  
}
    
## To be copied in the UI
# mod_data_reduc_ui("data_reduc_ui_1")
    
## To be copied in the server
# callModule(mod_data_reduc_server, "data_reduc_ui_1")
 
