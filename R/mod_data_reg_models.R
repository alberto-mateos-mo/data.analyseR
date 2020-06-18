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
    h4("Explore regression models for your data."),
    fluidRow(
      col_12(
        esquisse::dragulaInput(ns("vars"), sourceLabel = "Variables", targetsLabels = c("Dependent", "Independents"), 
                               choices = c(""), replace = FALSE) 
      )
    ),
    shinyWidgets::radioGroupButtons(inputId = ns("type"), label = "Model:", choices = c("Linear", "Logistic"), status = "primary"),
    fluidRow(
      col_4(
        actionButton(ns("go"), "Run Model!")
      )
    ),
    fluidRow(align = "center",
      col_6(
          tableOutput(ns("summ"))
      ),
      col_6(
        col_12(verbatimTextOutput(ns("reg_explain")))
      )
    ),
    fluidRow(
      col_12(),
      col_12(align = "center",
        shinyWidgets::radioGroupButtons(ns("plot"), "Summary Plot to View:", 
                                        choiceNames = c("Residuals vs. Fitted", "QQ-Plot", "Scale Location", "Residuals vs. Leverage"), 
                                        choiceValues = c(1, 2, 3, 4),
                                        individual = TRUE),
        plotOutput(ns("reg_plot"), width = "800px"),
        downloadButton(ns("plot_down"))
      ),
      col_12()
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
    
    if(input$type == "Linear"){
      summarised_lm(formula = form, data = react(), na.action = na.omit)
    }else if(input$type == "Logistic"){
      summarised_logistic(formula = form, data = react())
    }
    
  })
  
  output$reg_explain <- renderPrint(
    results()
  )
  
  reg_plot <- reactive({
    req(input$go)
    results()$plots[[as.numeric(input$plot)]]
  })
  
  output$reg_plot <- renderPlot(
    reg_plot()
  )
  
  output$plot_down <- downloadHandler(
    filename = function() "reg_plot.png",
    content = function(file){
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 600, units = "in")
      ggplot2::ggsave(file, plot = reg_plot(), device = device)
    }
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
 
