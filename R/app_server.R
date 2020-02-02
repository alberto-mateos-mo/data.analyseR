#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  observeEvent(input$browser,{
    browser()
  })
  
  callModule(mod_data_reading_server, "data_reading_ui_1")
}
