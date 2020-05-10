#' Create badge according to data type
#'
#' It uses conventions defined in the package, variable type are retrieve with \code{\link{col_type}}.
#'
#' @param col_name Variable's name
#' @param col_type Variable's type : 'discrete', 'time', 'continuous', 'id'
#'
#' @noRd

badgeType <- function(col_name) {
  
  res <- lapply(
    X = seq_along(col_name),
    FUN = function(i) {
      col_name_i <- col_name[i]
      # tags$span(class='label label-discrete badge-dad', col_name_i)
      tags$span(class = "label custom", col_name_i)
    }
  )
  res
}