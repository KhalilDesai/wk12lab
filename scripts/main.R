library(tidyverse)

## 0) LOAD FUNCTIONS AND SET CONSTANTS

# loads functions held in separate scripts
source("scripts/utils.R")
source("scripts/estimation.R")
source("scripts/simulation.R")
source("scripts/placement.R")
source("scripts/analysis.R")

# constants for use across program
SEED <- 2025
FLEET_SIZES <- c(22, 44, 66, 88)
RUN_OPTIMIZATION <- TRUE

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

# finds optimal placements for 4 different fleet sizes
optimal_placement_1 <- get_optimal_placement(FLEET_SIZES[1], complete_arrival_rates,
                                           lambda_max, seed = SEED, num_iters = 20)
optimal_placement_2 <- get_optimal_placement(FLEET_SIZES[2], complete_arrival_rates,
                                             lambda_max, seed = SEED, num_iters = 40)
optimal_placement_3 <- get_optimal_placement(FLEET_SIZES[3], complete_arrival_rates,
                                             lambda_max, seed = SEED, num_iters = 60)
optimal_placement_4 <- get_optimal_placement(FLEET_SIZES[4], complete_arrival_rates,
                                             lambda_max, seed = SEED, num_iters = 80)
## 4) VISUALIZATION AND ANALYSIS

# save placements
save_placement(FLEET_SIZES[1], optimal_placement_1$optimized_placement)
save_placement(FLEET_SIZES[2], optimal_placement_2$optimized_placement)
save_placement(FLEET_SIZES[3], optimal_placement_3$optimized_placement)
save_placement(FLEET_SIZES[4], optimal_placement_4$optimized_placement)

# net demand over time
# before vs after bike allocation
# satisfaction_rate vs fleet_size given optimal allocation