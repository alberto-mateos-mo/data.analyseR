# Module UI
  
#' @title   mod_data_format_ui and mod_data_format_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_format
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_data_format_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      h4("Here you can choose the format of each column"),
      actionButton(ns("apply"), "Apply formats")
      ),
    mainPanel(
      rhandsontable::rHandsontableOutput(ns("format"))
    )
  )
}
    
# Module Server
    
#' @rdname mod_data_format
#' @export
#' @keywords internal
    
mod_data_format_server <- function(input, output, session, react){
  ns <- session$ns
  
  df_type <- reactive({
    coltype <- unlist(lapply(react(), class))
    coltype <- ifelse(coltype %in% c("numeric", "integer"), "numeric",
                      ifelse(coltype %in% c("factor", "character"), "categorical", "other"))
    data.frame(variable = names(react()), 
                     format = factor(coltype),
                     stringsAsFactors = FALSE)
  })
  
  output$format <- rhandsontable::renderRHandsontable({
    rhandsontable::rhandsontable(df_type(), rowHeaders = NULL)
  })
  
  # datos_f <- eventReactive(input$apply, {
  # 
  # })
}
    
## To be copied in the UI
# mod_data_format_ui("data_format_ui_1")
    
## To be copied in the server
# callModule(mod_data_format_server, "data_format_ui_1")
 
