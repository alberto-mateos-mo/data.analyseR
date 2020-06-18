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
      col_12(
        h4("Can your data be explained by a fewer number of variables?")
      ),
      col_12(h4("")),
      col_4(
        # h5("Here you can analyse how a PCA reduction performs in your data."),
        h6("Note: we are automatically selecting numeric variables only."),
        h4(""),
        selectizeInput(ns("vars"), "Variables to use:", choices = NULL, multiple = TRUE),
        shiny::checkboxInput(ns("scale"), "Use scaled data", value = TRUE),
        actionButton(ns("run"), "Run PCA."),
        h4(""),
        selectInput(ns("plot"), "Select the plot you want to see:", choices = c("Screeplot", "Individuals Plot", "Variables Plot")),
        numericInput(ns("pca_num"), label = "Number of components to download:", value = NULL),
        downloadButton(ns("pca_down"), label = "Download PCA")
      ),
      col_6(
        plotOutput(ns("pca")),
        verbatimTextOutput(ns("explain"))
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
    updateNumericInput(session, "pca_num", value = 1, min = 1, max = ncol(df()), step = 1)
  })
  
  results <- eventReactive(input$run, {
    # if(input$t_red == "PCA"){
    #   return()
    # }
    if(is.null(input$vars)){
      return(summarised_pca(df(), retx = TRUE, scale. = input$scale))
    }else if(!is.null(input$vars)){
      i <- which(names(df()) %in% input$vars)
      df <- df()[,i]
      return(summarised_pca(df, retx = TRUE, scale. = input$scale))
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
  
  pca_data <- reactive({
    results()$pca$x[,c(1:input$pca_num)]
  })
  
  output$explain <- renderPrint({
    explain_pca(results()$pca)
  })
  
  output$pca_down <- downloadHandler(
    filename = function(){
      "pca_data.csv"
    },
    
    content = function(file){
      write.csv(pca_data(), file, row.names = FALSE)
    },
    
    contentType = "text/csv"
  )
  
}
    
## To be copied in the UI
# mod_data_reduc_ui("data_reduc_ui_1")
    
## To be copied in the server
# callModule(mod_data_reduc_server, "data_reduc_ui_1")
 
