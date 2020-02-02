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
      
      h6("We support csv, sav, xlsx, txt, rda, rds"),
      
      textOutput(ns("dValidation")) 
    ),
    
    col_4(
      DT::DTOutput(ns("tabla"), width = 800)
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
    
    l <- length(unlist(strsplit(rlang::as_string(userFile()$datapath), ".", fixed = TRUE)))
    ext <- tolower(unlist(strsplit(rlang::as_string(userFile()$datapath), ".", fixed = TRUE))[l])
    ufile <- userFile()$datapath
    
    if(ext == "sav"){
      df <- haven::read_sav(file = ufile, encoding = "UTF-8")
      return(df)
    }
    if(ext == "csv"){
      df <- read.csv(file = ufile, encoding = "UTF-8")
      return(df)
    }
    if(ext == "xlsx"){
      df <- readxl::read_xlsx(path = ufile)
      return(df)
    }
    if(ext == "txt"){
      df <- read.table(file = ufile, sep = " ", fileEncoding = "UTF-8")
      return(df)
    }
    if(ext == "rds"){
      df <- readRDS(file = ufile)
      return(df)
    }
    else{
      return("n")
    }
  })
  
  output$dValidation <- renderText({
    if(class(userData()) == "character"){
      "Data format not supported :("
    }else{
      "Data uploaded successfully :)"
    }
  })
  
  output$tabla <- DT::renderDT({
    if(class(userData()) == "character") NULL
    m <- min(ncol(userData()), 8)
    DT::datatable(userData()[,c(1:m)], rownames = F) %>%
      DT::formatStyle(backgroundColor = "#00274d",
                      columns = names(userData()[,c(1:m)]))
  })
  
}
    
## To be copied in the UI
# mod_data_reading_ui("data_reading_ui_1")
    
## To be copied in the server
# callModule(mod_data_reading_server, "data_reading_ui_1")
 
