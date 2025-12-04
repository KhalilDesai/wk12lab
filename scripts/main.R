library(tidyverse)

## 0) LOAD FUNCTIONS AND SET CONSTANTS

# loads functions held in separate scripts
source("scripts/utils.R")
source("scripts/estimation.R")
source("scripts/simulation.R")
source("scripts/placement.R")

# constants for use across program
SEED <- 2025
FLEET_SIZES <- c(22, 44, 66, 88, 110)

## 1) DATA PREP

# load bike riding data
bike_data <- read_csv("data/sample_bike.csv")

# drop all rows with NA stations (just to be safe)
bike_data <- bike_data %>%
  drop_na(start_station, end_station)

## 2) ESTIMATION

# estimate arrival rates for all end/start/hour trios present in the data
arrival_rates <- estimate_arrival_rates(bike_data)

# complete all routes for all hours (0-23) for all station pairs
complete_arrival_rates <- complete_hours(arrival_rates)

# compute the largest hourly arrival rate for every station pair
lambda_max <- find_lambda_max(complete_arrival_rates)

## 3) SIMULATION + OPTIMIZATION

optimal_placement <- get_optimal_placement(22, complete_arrival_rates, lambda_max, seed = SEED, num_iters = 40)
print

# simulates a single day
#simulated_day <- simulate_one_day(complete_arrival_rates, lambda_max, SEED)

# placement IDFKKKKKKK
#placement <- setNames(rep(1, 23), 2:24)

# evaluate a bike placement
#result <- evaluate_placement(placement, simulated_day)
#print(result$satisfaction_rate)
# print(result$final_placement)


## 4) VISUALIZATION AND ANALYSIS

# placements for 5 fleet sizes (22, 44, 66, 88, 110)
# net demand over time
# before vs after bike allocation
# satisfaction_rate vs fleet_size given optimal allocation