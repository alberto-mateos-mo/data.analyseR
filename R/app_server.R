#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  datos <- callModule(mod_data_reading_server, "data_reading_ui_1")
  datos_f <- callModule(mod_data_format_server, "data_format_ui_1", react = datos)
  callModule(mod_data_descrip_server, "data_descrip_ui_1", react = datos_f)
  callModule(mod_data_reduc_server, "data_reduc_ui_1", react = datos_f)
  callModule(mod_data_reg_models_server, "data_reg_models_ui_1", react = datos_f)
  callModule(mod_data_clust_server, "data_clust_ui_1", react = datos_f)
}
