#' Import data from COVID-19-NL Github Repository
#'
#' @param path The path the file `NL_COVID19_info_city.csv`.
#'
#' @return A data frame grouped with reported cases per municipality and day.
#' @export
#'
#' @examples
#' \dontrun{
#' loadDataCOVID19NL()
#' }
loadDataCOVID19NL <- function(path) {

 # Load the data
 df <- readr::read_csv(path) %>%
  janitor::clean_names()

 # Go from wide to long format
 df <- reshape2::melt(df, id.vars = c("city")) %>%
  dplyr::filter(city != "SUM") %>%
  dplyr::rename(report_date = variable,
                cases = value) %>%
  dplyr::mutate(report_date = substr(x = report_date, start = 2, stop = 9),
                report_date = as.Date(report_date, "%Y%m%d"),
                cases = as.numeric(cases)) %>%
  dplyr::group_by(city, report_date) %>%
  dplyr::summarise(cases = sum(cases, na.rm = TRUE)) %>%
  dplyr::ungroup()

 return(df)
}

#' Filter on municipality
#'
#' @param data A data frame accorindg the package standards
#' @param town The name of the municipality on which the filter
#' should take place.
#'
#' @return A data frame based on the selected municipality in
#' the town argument.
#' @export
#'
#' @examples
#' \dontrun{
#' filterTown()
#' }
filterTown <- function(data, town) {

 # Filter based on selected town
 if (methods::hasArg(town) == FALSE) {
  df <- data
  message("No town selection made")
 } else {
  df <- data %>%
   dplyr::filter(city == town)
 }

 # Aggregate data
 if (methods::hasArg(town) == FALSE) {
  df <- df %>%
   dplyr::group_by(report_date) %>%
   dplyr::summarise(cases =  sum(cases, na.rm = TRUE)) %>%
   dplyr::ungroup()
 } else {
  df <- df %>%
   dplyr::group_by(report_date, city) %>%
   dplyr::summarise(cases =  sum(cases, na.rm = TRUE)) %>%
   dplyr::ungroup()
 }

 # Return filtered dataset
 return(df)

}
