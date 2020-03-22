#' Pretty delta cases format for plots
#'
#' @param x The value to make pretty
#'
#' @return The imput value in a + or - minus format
#' @export
#'
#' @examples
#' prettyDeltaCases(x = 10)
#' prettyDeltaCases(x = 0)
#' prettyDeltaCases(x = -10)
prettyDeltaCases <- function(x) {

 pretty_delta_cases <- ifelse(x > 0, paste0("+", x), x)

 return(pretty_delta_cases)

}
