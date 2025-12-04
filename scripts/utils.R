library(tidyverse)

#' Completes data for hours 0-23
#' 
#' @description for every start, end station pair in the data, guarantees that
#' a row exists for every hour 0-23. added rows have mu_hat = 0.0. removes 
#' any route that has only mu_hat = 0 (i.e. never predicted to occur)
#' 
#' @param arrival_rates data frame/tibble - contains raw estimated arrival rates
#' 
#' @return a completed tibble, with entries for every route across all hours,
#' and with 0-probability routes removed
complete_hours <- function(arrival_rates) {
  result <- arrival_rates %>%
    group_by(start_station, end_station) %>%
    complete(hour = 0:23, fill = list(mu_hat = 0.0, avg_trips = 0.0,
                                      avg_avail = 1.0)) %>%
    filter(sum(mu_hat) != 0) %>%
    ungroup() 
}

#' Finds the max hourly arrival rate between stations
#'
#' @description for every start, end station pair in the data, determines the 
#' maximum estimated hourly arrival rate
#' 
#' @param arrival_rates data frame/tibble - contains estimated arrival rates
#' 
#' @return a tibble with the max hourly estimated arrival rate for every 
#' start, end station pair present in the data
find_lambda_max <- function(arrival_rates) {
  lambda_maxes <- arrival_rates %>%
    group_by(start_station, end_station) %>%
    summarize(lambda_max = max(mu_hat), .groups = "drop")
  return(lambda_maxes)
}
