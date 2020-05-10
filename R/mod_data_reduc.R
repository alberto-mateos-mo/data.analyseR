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
    inputPanel(
      selectInput(ns("t_red"), "Select reduction method", choices = c("PCA", "KPCA", "tSNE"))
    )
  )
}
    
# Module Server
    
#' @rdname mod_data_reduc
#' @export
#' @keywords internal
    
mod_data_reduc_server <- function(input, output, session){
  ns <- session$ns
  red_method <- reactive({
    if(input$t_red == "PCA"){
      return()
    }
  })
}
    
## To be copied in the UI
# mod_data_reduc_ui("data_reduc_ui_1")
    
## To be copied in the server
# callModule(mod_data_reduc_server, "data_reduc_ui_1")
 
