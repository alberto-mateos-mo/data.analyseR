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
    fluidRow(
    col_12(h4("Here you can choose the format of each column if needed.")),
    col_4(
      h6("You can skip this step by clicking the Apply formats button and we'll guess column type."),
      h6("You can always come back if something went tricky in another tab ;)"),
      actionButton(ns("apply"), "Apply formats")
      ),
    col_6(align = "center",
      rhandsontable::rHandsontableOutput(ns("format"))
    )
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
    data.frame(variable = names(react()), 
                     format = factor("", levels = c("numeric", "categorical", "")),
                     stringsAsFactors = FALSE)
  })
  
  output$format <- rhandsontable::renderRHandsontable({
    rhandsontable::rhandsontable(df_type(), rowHeaders = NULL)
  })
  
  datos_f <- eventReactive(input$apply, {
    df <- react()
    tmp <- rhandsontable::hot_to_r(input$format)
    
    for(i in 1:nrow(tmp)){
      v <- which(names(react()) == tmp[i,1])
      
      if(tmp[i,2] == "numeric"){
        df[,v] <- as.numeric(df[,v])
      }
      if(tmp[i,2] == "categorical"){
        df[,v] <- as.factor(df[,v])
      }
      if(tmp[i,2] == ""){
        df[,v] <- df[,v]
      }
    }
    
    df
  })
  
  datos <- reactive({
    if(input$apply == 0){
      return(react())
    }else if(input$apply != 0){
      return(datos_f())
    }
  })
  
  observe({
    
    if(input$apply != 0){
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Done !!",
        text = "We have applied formats correctly",
        type = "info"
      )
    }
    
  })
  
  return({
    datos
  })
  
}
    
## To be copied in the UI
# mod_data_format_ui("data_format_ui_1")
    
## To be copied in the server
# callModule(mod_data_format_server, "data_format_ui_1")
 
