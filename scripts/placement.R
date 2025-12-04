library(tidyverse)

#' Rudimentarily optimizes initial placement for satisfaction rate
#'
get_optimal_placement <- function(fleet_size, complete_arrival_rates, lambda_max, seed = NULL, num_iters = 20) {
  
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
  
  # rudimentarily optimize placement for simulated requests
  # essentially, move one bike from least strained station with bikes to 
  # station with most failed requests
   for (i in 1:num_iters) {
    # evaluate our current placement
    result <- evaluate_placement(placement, simulated_day)
    
    # TODO: FIX THIS print (for debugging)
    print(result$satisfaction_rate)
    
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
  
  print(result$final_placement)
  return(result$satisfaction_rate)
}


# TODO: FUNCTION DOCUMENTATION
# basically evaluates proportion of satisfied requests given an initial placement
# placement is a named vector
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
      failed_requests <- c(failed_requests, request$start_station)
    }
    
  }
  
  # return evaluation info
  return(list(satisfaction_rate = satisfied / nrow(simulated_day),
              final_placement = placement,
              failed_requests = failed_requests))
}