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
  
  )
}
    
# Module Server
    
#' @rdname mod_data_descrip
#' @export
#' @keywords internal
    
mod_data_descrip_server <- function(input, output, session, react){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_data_descrip_ui("data_descrip_ui_1")
    
## To be copied in the server
# callModule(mod_data_descrip_server, "data_descrip_ui_1")
 
