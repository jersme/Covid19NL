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
 plot <- ggplot(df, aes(x = report_date, y = cases)) +
  scale_x_date(date_breaks = "days" , date_labels = "%d-%m") +
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  labs(title = l_title,
       x = "Case report date",
       y = "Reported cases") +
  theme_linedraw()

 # Type of plot
 if (type == "column") {
  plot <- plot +
   geom_col() +
   geom_text(aes(y = cases + 25, label = cases), position = position_dodge(1), size = 2.8)
 } else if (type == "line") {
  plot <- plot +
   geom_line() +
   geom_point()
 } else if (type == "both") {
  plot <- plot +
   geom_line() +
   geom_col(alpha = .7)
 } else {
  stop("Only types column, line or both are valid inputs")
 }

 # Trend line
 if (trend_line == TRUE) {
  plot <- plot +
   geom_smooth(se = FALSE)
 }

 return(list(plot = plot, data = df))
}
