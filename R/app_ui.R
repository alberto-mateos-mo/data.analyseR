#' @import shiny rlang
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    theme = shinythemes::themeSelector(),
    # List the first level UI elements here 
    navbarPage(title = "Data Analyser Toolkit",
               tabPanel("Data Upload",
                        mod_data_reading_ui("data_reading_ui_1")
                        ),
               tabPanel("Data Format",
                        mod_data_format_ui("data_format_ui_1")
               ),
               tabPanel("Data Descriptives",
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
 
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
