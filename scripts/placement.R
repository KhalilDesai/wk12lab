library(tidyverse)

#' Rudimentarily optimizes initial placement for satisfaction rate
#'
#' @description given a fleet_size and the outputs of our data processing, 
#' rudimentarily optimizes the initial placement for satisfaction rate. first
#' simulates a day of rides, then sets an initial, uniform placement. that 
#' placement is evaluated on the simulated day, and a bike is moved to the station
#' with the most failed requests from the station with the least failed requests
#' and available bikes. this loop is executed num_iters times.
#' 
#' @param fleet_size single-element numeric vector - the fleet size. must be a
#' multiple of the number of unique stations
#' @param complete_arrival_rates data frame/tibble - the complete arrival rates
#' estimated and cleaned from our raw data
#' @param lambda_max data frame/tibble - the tibble with lambda_max info
#' @param seed single-element numeric vector - optional random seed for reproducibility
#' @param num_iters single-element numeric vector - number of iterations of 
#' optimization loop
#' 
#' @return a named list with the following components:
#' - satisfaction_rate : the proportion of satisfied requests for our optimized placement
#' - optimized_placement : our optimized initial placement of bikes
#' - final_arrangement : the final arrangement of bikes after the day of rides
#'  for our optimized placement
#' - rate_history : satisfaction rates recorded over the iterations of the 
#' optimization loop
get_optimal_placement <- function(fleet_size, complete_arrival_rates, lambda_max,
                                  seed = NULL, num_iters = 20) {
  # get all unique stations
  unique_stations <- unique(c(complete_arrival_rates$start_station,
                              complete_arrival_rates$end_station))
  unique_stations <- factor(as.character(sort(as.numeric(unique_stations[unique_stations != "R"]))))
  
  # make sure fleet_size is a multiple of the number of unique stations
  if (fleet_size %% length(unique_stations) != 0) {
    stop("fleet_size must be multiple of the number of unique stations")
  }
  
  # initial placement is uniform, *REQUIRES FLEET_SIZE TO BE MULTIPLE OF NUMBER OF STATIONS*
  placement <- setNames(rep(fleet_size / length(unique_stations),
                                    length(unique_stations)), unique_stations)
  
  # simulate day of ride requests
  simulated_day <- simulate_one_day(complete_arrival_rates, lambda_max, seed)
  
  # initialize vector for satisfaction_rate history
  satisfaction_rate_history <- c()
  
  # rudimentarily optimize placement for simulated requests
  # essentially, move one bike from least strained station with bikes to 
  # station with most failed requests
   for (i in 1:num_iters) {
    # evaluate our current placement
    result <- evaluate_placement(placement, simulated_day)
    
    # add satisfaction_rate to satisfaction_rate_history
    satisfaction_rate_history <- c(satisfaction_rate_history, result$satisfaction_rate)
    
    # if we achieve a perfect placement (unlikely), break
    if (length(result$failed_requests) == 0) break
    
    # otherwise find the station with the most failed requests
    fail_counts_partial <- table(result$failed_requests)
    all_stations <- names(placement)
    fail_counts <- setNames(rep(0, length(all_stations)), all_stations)
    fail_counts[names(fail_counts_partial)] <- as.numeric(fail_counts_partial)
    
    worst_station <- names(sort(fail_counts, decreasing = TRUE))[1]
    
    # find the station with the fewest failed requests and bikes in the initial placement
    stations_with_bikes <- placement > 0
    fail_counts_avail <- fail_counts[names(fail_counts) %in% names(placement[stations_with_bikes])]
    best_station <- names(sort(fail_counts_avail, decreasing = FALSE))[1]
    
    # move one bike from best to worst station
    placement[worst_station] <- placement[worst_station] + 1
    placement[best_station] <- placement[best_station] - 1
  }
  
  return(list(satisfaction_rate = result$satisfaction_rate,
              optimized_placement = placement,
              final_arrangement = result$final_arrangement,
              rate_history = satisfaction_rate_history))
}


#' Evaluates a given placement on a simulated day of rides
#' 
#' @description evaluates a given placement on a simulated day. returns the 
#' satisfaction rate, the final arrangement of bikes, and a vector with the 
#' stations that failed requests due to lack of bikes.
#' 
#' @param placement a named numeric vector - names are stations and values are
#' number of bikes
#' @param simulated_day data frame/tibble - full simulated day of ridership
#' 
#' @return a named list with the following components:
#' - satisfaction_rate : the proportion of satisfied requests
#' - final_arrangement : the final arrangement of bikes after the day of rides
#' - failed_requests : a vector that records the stations that failed requests
evaluate_placement <- function(placement, simulated_day) {
  # sort simulated day by time
  sorted_day <- simulated_day %>%
    arrange(time)
  
  # initialize number of satisfied requests at 0, empty vector of failed requests
  satisfied <- 0
  failed_requests <- c()
  
  for (request_num in 1:nrow(sorted_day)) {
    # get request
    request <- sorted_day %>% 
      slice(request_num)
    
    # if there is a bike available at the start station, make the transfer
    if (placement[request$start_station] > 0) {
      satisfied <- satisfied + 1
      placement[request$start_station] <- placement[request$start_station] - 1
      placement[request$end_station] <- placement[request$end_station] + 1
    } else {
      # otherwise, mark it as a failed request
      failed_requests <- c(failed_requests, request$start_station)
    }
    
  }
  
  # return evaluation info as named list
  return(list(satisfaction_rate = satisfied / nrow(simulated_day),
              final_arrangement = placement,
              failed_requests = failed_requests))
}