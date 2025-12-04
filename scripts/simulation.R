library(tidyverse)

#' Simulates a day of attempted bike rides
#' 
#' @description simulates a day of attempted bike rides assuming a
#' non-homogeneous poisson process per route. uses completed estimated
#' arrival rates and max hourly rates to do so.
#' 
#' @param complete_arrival_rates data frame/tibble - contains completed 
#' estimated arrival rates
#' @param lambda_max data frame/tibble - stores maximum hourly arrival rate
#' for each route present in the data
#' 
#' @return a tibble with all attempted rides (start, end, and time in hours)
simulate_one_day <- function(complete_arrival_rates, lambda_max) {
  # create a data frame with all distinct routes
  all_routes <- complete_arrival_rates %>%
    distinct(start_station, end_station)
  
  # creates pre-allocated output vector of lists (tibbles)
  output_list <- vector("list", length = nrow(all_routes))
  
  # simulate a day of rides for each route
  for (route_num in 1:nrow(all_routes)) {
    # extract route as named vector
    route <- unlist(all_routes[route_num, ])
    
    # extract lambda max for this route
    lambda_max_route <- lambda_max %>%
      filter(start_station == route["start_station"] &
             end_station == route["end_station"]) %>%
      slice(1) %>%
      pull(lambda_max)
    
    # extract hourly_arrival_rates for this route
    hourly_arrival_rates <- complete_arrival_rates %>%
      filter(start_station == route["start_station"] &
             end_station == route["end_station"]) %>%
      arrange(hour) %>%
      pull(mu_hat)
    
    # simulate this one-route for the day and add to our output log
    output_list[[route_num]] <- simulate_route_one_day(route,
                                                       hourly_arrival_rates,
                                                       lambda_max_route)
  }
  
  # bind all simulated route outputs together and return
  output_df <- bind_rows(output_list)
  
  return(output_df)
}

#' Simulates a single day of attempted bike rides for one route
#' 
#' @description simulates a day of attempted bike rides for a single, specified
#' route. uses a thinning procedure to model a poisson process. 
#' 
#' @param route named character vector - gives the route we are modeling
#' @param hourly_arrival_rates numeric vector - the hourly arrival rates in order
#' @param lambda_max single-element numeric vector - max hourly arrival rate
#'  for this route
#'  
#'  @return a tibble with one day of simulated bike rides for this route
simulate_route_one_day <- function(route, hourly_arrival_rates, lambda_max) {
  # initialize an empty output tibble
  output <- tibble(
    start_station = character(),
    end_station = character(),
    time = numeric()
  )
  
  # initialize time at 0
  time = 0
  
  # while we are still within one day
  while (time < 24) {
    # record lambda_i
    lambda_i = hourly_arrival_rates[floor(time) + 1]
    
    # sample time from exponential distribution
    sampled_time <- rexp(1, rate = lambda_max)
    
    # add sampled_time to time
    time <- time + sampled_time
    
    # choose whether to keep the observation (thinning)
    if (time < 24 && rbinom(1, size = 1, p = (lambda_i / lambda_max)) == 1) {
      output <- output %>%
        add_row(start_station = route["start_station"],
                end_station = route["end_station"],
                time = time)
    }
  }
  
  return(output)
}