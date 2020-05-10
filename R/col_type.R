#' Try to guess type of a vector
#'
#' @param x a vector
#'
#' @noRd
col_type <- function(x, no_id = FALSE) {
  if (is.null(x))
    return(NULL)
  
  if (is.data.frame(x) && inherits(x, what = "sf")) {
    x <- x[, setdiff(names(x), attr(x, "sf_column")), drop = FALSE]
  } 
  
  if (is.data.frame(x)) {
    return(unlist(lapply(x, col_type), use.names = FALSE))
  } else {
    if (inherits(x, c("logical", "character", "factor", "AsIs"))) {
      n <- length(x)
      u <- length(unique(x))
      if (u/n < 0.99 | u <= 30 | no_id) {
        return("discrete")
      } else {
        return("id")
      }
    }
    
    if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
      return("time")
    }
    
    if (inherits(x, c("numeric", "integer", "double"))) {
      return("continuous")
    }
  }
  
  NULL
}