#' Plots the daily positive tested cases.
#'
#' @param data Data frame container the case data. Should be
#' in the expected package standard. Which can be checked by
#' the `checkDataFrame()` function.
#' @param town Municipality for whoch the plot should be
#' created. If no town is specified the sum of all
#' municipalities is used.
#' @param type The type op plot. Inputs can be `columns`,
#' `line` or `both`.
#' @param trend_line If `trend_line == TRUE` a trendline will
#' be added to the plot.
#'
#' @return A ggplot and a dataframe based on the municipality
#' in the input. The plot can be called by `$plot` and the
#' data via `$data`.
#' @export
#'
#' @examples
#' \dontrun{
#' plotCases(md, town = "Amsterdam", type = "column", trend_line = TRUE)
#' }
plotCases <- function(data, town, type, trend_line) {

 # Filter data
 df <- filterTown(data = data, town = town)

 # Create labels
 base_title <- "Covid 19 cases in"

 if (methods::hasArg(town) == FALSE) {
  l_title <- paste(base_title, "The Netherlands")
 } else {
  l_title <- paste(base_title, town)
 }

 # Create the plot
 plot <- ggplot2::ggplot(df, ggplot2::aes(x = report_date, y = cases)) +
  ggplot2::scale_x_date(date_breaks = "days" , date_labels = "%d-%m") +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  ggplot2::labs(title = l_title,
       x = "Case report date",
       y = "Reported cases") +
  ggplot2::theme_linedraw()

 # Type of plot
 if (type == "column") {
  plot <- plot +
   ggplot2::geom_col() +
   ggplot2::geom_text(ggplot2::aes(y = cases + 25, label = cases), position = ggplot2::position_dodge(1), size = 2.8)
 } else if (type == "line") {
  plot <- plot +
   ggplot2::geom_line() +
   ggplot2::geom_point()
 } else if (type == "both") {
  plot <- plot +
   ggplot2::geom_line() +
   ggplot2::geom_col(alpha = .7)
 } else {
  stop("Only types column, line or both are valid inputs")
 }

 # Trend line
 if (trend_line == TRUE) {
  plot <- plot +
   ggplot2::geom_smooth(se = FALSE)
 }

 return(list(plot = plot, data = df))
}
