# Module UI
  
#' @title   mod_data_reading_ui and mod_data_reading_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_reading
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_data_reading_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    sidebarPanel(
      fileInput(ns("data"), label = "Upload your data file"),
      
      h6("We support almost all data formats"),
      
      shinyWidgets::useSweetAlert() 
    ),
    
    col_4(
      h3("Showing some of the columns"),
      DT::DTOutput(ns("tabla"), width = 800),
      h5("Your data info:"),
      tableOutput(ns("meta"))
    )
  )
}
    
# Module Server
    
#' @rdname mod_data_reading
#' @export
#' @keywords internal
    
mod_data_reading_server <- function(input, output, session){
  ns <- session$ns
  
  userFile <- reactive({
    validate(need(input$data, message = FALSE))
    input$data
  })
  
  userData <- reactive({
    
    validate(need(input$data, message = FALSE))
    
    ufile <- userFile()$datapath
    
    try(rio::import(ufile))
    
  })

  observe({
    if(class(userData()) == "data.frame"){
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Success !!",
        text = "All in order",
        type = "success"
      )
    }else{
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error !!",
        text = "We do not support your data :(",
        type = "error"
      )
    }
  })
  
  output$tabla <- DT::renderDT({
    if(class(userData()) != "data.frame") NULL
    m <- min(ncol(userData()), 8)
    DT::datatable(userData()[,c(1:m)], rownames = F) %>%
      DT::formatStyle(backgroundColor = "#00274d",
                      columns = names(userData()[,c(1:m)]))
  })
  
  output$meta <- renderTable({
    if(class(userData()) != "data.frame") NULL
    DataExplorer::introduce(userData())
  })
  
}
    
## To be copied in the UI
# mod_data_reading_ui("data_reading_ui_1")
    
## To be copied in the server
# callModule(mod_data_reading_server, "data_reading_ui_1")
 
