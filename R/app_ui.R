#' @import shiny rlang
app_ui <- function() {
  
  # bootstraplib::bs_theme_new(bootswatch = NULL)
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    tags$head(
      tags$style(HTML(
        "
    .custom {
      background-color: #D59F0F;
      color: #FFFFFF;
      cursor: move;
      font-size: 120%;
      overflow: hidden;
      border-radius: 1px;
    }
    "
      ))
    ),
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
                        ),
               navbarMenu("Stats Models",
                          tabPanel("Dimensionality Reduction",
                                   mod_data_reduc_ui("data_reduc_ui_1")
                                   ),
                          tabPanel("Regression models",
                                   mod_data_reg_models_ui("data_reg_models_ui_1"))
                          )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'data.analyseR')
  )
 
  bootstraplib::bs_theme_new(version = "4+3", bootswatch = "lux")
  bootstraplib::bs_theme_add_variables(`font-size-base` = "1rem")
  bootstraplib::bs_theme_add_variables(`body-color` = "#002B7A", 
                         `input-border-color` = "#002B7A", primary = "#D59F0F", 
                         default = "#D59F0F", secondary = "#D59F0F", `gray-900` = "#002B7A")
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    bootstraplib::bootstrap(minified = FALSE)
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    # tags$link(rel="stylesheet", type="text/css", href="www/bootstrap.min.css")
  )
}
