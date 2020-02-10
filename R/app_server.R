#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  observeEvent(input$browser,{
    browser()
  })
  
  datos <- callModule(mod_data_reading_server, "data_reading_ui_1")
  callModule(mod_data_descrip_server, "data_descrip_ui_1", react = datos)
}
