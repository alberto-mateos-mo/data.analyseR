# Module UI
  
#' @title   mod_data_reg_models_ui and mod_data_reg_models_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_reg_models
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_data_reg_models_ui <- function(id){
  ns <- NS(id)
  tagList(
    esquisse::dragulaInput(ns("vars"), sourceLabel = "Variables", targetsLabels = c("dependent", "independents"), 
                           choices = c(""))
  )
}
    
# Module Server
    
#' @rdname mod_data_reg_models
#' @export
#' @keywords internal
    
mod_data_reg_models_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_data_reg_models_ui("data_reg_models_ui_1")
    
## To be copied in the server
# callModule(mod_data_reg_models_server, "data_reg_models_ui_1")
 
