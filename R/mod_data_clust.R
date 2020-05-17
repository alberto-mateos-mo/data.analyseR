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
    h4("Here you can explore clustering patterns in your data.")
  )
}
    
# Module Server
    
#' @rdname mod_data_clust
#' @export
#' @keywords internal
    
mod_data_clust_server <- function(input, output, session, react){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_data_clust_ui("data_clust_ui_1")
    
## To be copied in the server
# callModule(mod_data_clust_server, "data_clust_ui_1")
 
