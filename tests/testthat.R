library(testthat)
library(tidyverse)

# load functions
source("scripts/estimation.R")
source("scripts/simulation.R")
source("scripts/utils.R")
source("scripts/placement.R")

# load some toy ridership data for testing
toydata <- read_csv("data/toy_data.csv")
toydata$start_station <- as.character(toydata$start_station)
toydata$end_station <- as.character(toydata$end_station)

# estimate_arrival_rates unit test (makes sure it is correct value)
arrival_rates <- estimate_arrival_rates(toydata)
expect_equal(arrival_rates$avg_trips[1], 1.5)

# find_lambda_max unit test (makes sure it is correct value)
lambda_max <- find_lambda_max(arrival_rates)
expect_equal(lambda_max$lambda_max[1], 1.5)

# complete_hours unit test (ought to now be multiple of 24)
complete_arrival_rates <- complete_hours(arrival_rates)
expect_equal(nrow(complete_arrival_rates) %% 24, 0)

# simulate_day + simulate_route_arrivals unit test (times ought to be sorted)
simulation <- simulate_one_day(complete_arrival_rates, lambda_max)
expect_equal(simulation$time, sort(simulation$time))

# get_optimal_placement + evaluate_placement unit tests
optimal_placement <- get_optimal_placement(4, complete_arrival_rates, lambda_max, num_iters = 5)
expect_equal(sum(optimal_placement$optimized_placement), 4)
expect_equal(sum(optimal_placement$final_arrangement), 4)
