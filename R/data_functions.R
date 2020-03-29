#' Import data from COVID-19-NL Github Repository
#'
#' @param fromGitHub If this parameter is TRUE the data will be loaded
#' from Github. If FALSE the a local csv file is expected.
#' @param path Mainly to be used with the fromGitHub == FALSE
#' option. Point to the file `NL_COVID19_info_city.csv`. Standard path is
#' `https://raw.githubusercontent.com/ZequnZ/COVID-19-NL/master/data/NL_COVID19_info_city.csv`
#'
#' @return A data frame grouped with reported cases per municipality and day.
#' @export
#'
#' @examples
#' \dontrun{
#' loadDataCOVID19NL()
#' }
loadDataCOVID19NL <- function(fromGitHub = TRUE, path = "https://raw.githubusercontent.com/ZequnZ/COVID-19-NL/master/data/NL_COVID19_info_city.csv") {

 # Load the data
 if (fromGitHub == TRUE) {
   df <- readr::read_csv(url(path))
 } else if (fromGitHub == FALSE) {

   if(path == "https://raw.githubusercontent.com/ZequnZ/COVID-19-NL/master/data/NL_COVID19_info_city.csv") {
     stop("Please select a valid path or use fromGitHub == TRUE")
   }
   df <- readr::read_csv(path)
 }

  df <- df %>%
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

#' Check if the data frame is according the expected package
#' specification.
#'
#' @param data The data frame to be checked
#'
#' @return TRUE for a correct data frame and FALSE for a
#' wrong data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' checkDataFrame()
#' }
checkDataFrame <- function(data) {

 # Check if columns names are in place
 names_check <- names(data) == c("city", "report_date", "cases" )
 names_check <- all(names_check)

 # Check of cases variables is numeric
 cases_check <- is.numeric(data$cases)

 # Combine all test
 test_result <- all(c(names_check, cases_check))

 return(test_result)

}

getDailyDelta <- function(data, town_selected) {

 # Summarise data to daily if no town is selected
 if (town_selected == FALSE) {
  df <- data %>%
   dplyr::group_by(report_date) %>%
   dplyr::summarise(cases = sum(cases, na.rm = TRUE)) %>%
   dplyr::ungroup()
 } else if (town_selected == TRUE) {
  df <- data
 }

 # Calculate the daily delta case base
 df <- df %>%
  dplyr::arrange(report_date) %>%
  dplyr::mutate(prev_cases = dplyr::lag(cases, 1),
                delta_cases = cases - prev_cases,
                delta_cases = ifelse(is.na(delta_cases), 0, delta_cases))

 # Select only required columns
 if (town_selected == FALSE) {
  df <- df %>%
   dplyr::select(report_date, cases, delta_cases)
 } else if (town_selected == TRUE) {
  df <- df %>%
   dplyr::select(city, report_date, cases, delta_cases)
 }

 return(df)
}

#' Calculate days since first case
#'
#' @param data Data frame with city, report_date and cases
#' @param cases_threshold TThe number of cases threshold to compare
#'
#' @return Data frame to compare days since case.
#' @export
#'
#' @examples
#' \dontrun{
#' daysSinceCase()
#' }
daysSinceCase <- function(data, cases_threshold) {

  # Create data with only active cases
  df <- data %>%
    dplyr::arrange(report_date) %>%
    dplyr::filter(cases >= cases_threshold)

  # Add the days
  df <- df %>%
    dplyr::mutate(days = c(1:nrow(df)),
                  case_threshold = cases_threshold)

  # Return the data.frame
  return(df)
}

#' Get data to compare two towns by days since case n
#'
#' @param data A data frame obtained via the daysSinceCase() function
#' @param town1 The first municipality
#' @param town2 The second municipality
#' @param cases_threshold The number of cases threshold to compare
#'
#' @return A data frame with two municipalities
#' @export
#'
#' @examples
#' \dontrun{
#' compareTrend()
#' }
compareTrend <- function(data, town1, town2, cases_threshold) {

  # Get data
  town1_data <- filterTown(data = data, town = town1)
  town2_data <- filterTown(data = data, town = town2)

  # Add days from case data
  town1_data <- daysSinceCase(data = town1_data, cases = cases_threshold)
  town2_data <- daysSinceCase(data = town2_data, cases = cases_threshold)

  # Combine data
  df <- dplyr::bind_rows(town1_data, town2_data)

  # Return data frame
  return(df)
}
