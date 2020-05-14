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
    h4("Here you can explore regression models for your data."),
    fluidRow(
      col_12(
        esquisse::dragulaInput(ns("vars"), sourceLabel = "Variables", targetsLabels = c("Dependent", "Independents"), 
                               choices = c(""), replace = FALSE) 
      )
    ),
    fluidRow(
      col_4(
        actionButton(ns("go"), "Run Model!")
      )
    ),
    fluidRow(align = "center",
      col_6(tableOutput(ns("summ"))),
      col_6(plotOutput(ns("reg_plot")))
    ),
    fluidRow(
      col_12(verbatimTextOutput(ns("reg_explain")))
    )
  )
}
    
# Module Server
    
#' @rdname mod_data_reg_models
#' @export
#' @keywords internal
    
mod_data_reg_models_server <- function(input, output, session, react){
  ns <- session$ns
  
  observe({
    esquisse::updateDragulaInput(session, "vars", choiceValues = names(react()), choiceNames = badgeType(col_name = names(react())),
                                 badge = FALSE)
  })
  
  results <- eventReactive(input$go, {
    
    if(length(input$vars$target$Dependent)>1) stop("You can only use one variable as dependent.")
    
    form <- as.formula(paste(paste0(input$vars$target$Dependent, collapse = ""), "~", paste0(input$vars$target$Independents, collapse = "+")))
    
    run_regression(formula = form, data = react())
    
  })
  
  model_type <- reactive({
    paste("We fiited a", results()$type, "model.", collapse = " ")
  })
  
  output$reg_explain <- renderPrint(
    results()
  )
  
  output$reg_plot <- renderPlot(
    results()$plots
  )
  
  output$summ <- renderTable(
    results()$summary_reg,
    rownames = TRUE
  )
  
}
    
## To be copied in the UI
# mod_data_reg_models_ui("data_reg_models_ui_1")
    
## To be copied in the server
# callModule(mod_data_reg_models_server, "data_reg_models_ui_1")
 
