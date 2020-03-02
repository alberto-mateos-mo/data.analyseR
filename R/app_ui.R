#' @import shiny rlang
app_ui <- function() {
  
  # bootstraplib::bs_theme_new(bootswatch = NULL)
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # bootstraplib::bootstrap(),
    # List the first level UI elements here 
    navbarPage(title = "Data Analyser Toolkit",
               tabPanel("Data Upload",
                        mod_data_reading_ui("data_reading_ui_1")
                        ),
               tabPanel("Data Format",
                        mod_data_format_ui("data_format_ui_1")
               ),
               tabPanel("Data Explorer",
                        mod_data_descrip_ui("data_descrip_ui_1")
                        )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'data.analyseR')
  )
 
  # bootstraplib::bs_theme_new()
  # 
  # bootstraplib::bs_theme_add_variables(`body-bg` = "#607AAC", `body-color` = "#FFFFFF", 
  #                                      `input-border-color` = "#D59F0F", primary = "#D59F0F", 
  #                                      default = "#002B7A", secondary = "#D59F0F", success = "#2B7A00", 
  #                                      `enable-shadows` = TRUE, `enable-gradients` = TRUE)
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
