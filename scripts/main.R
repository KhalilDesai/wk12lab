library(tidyverse)

## 0) LOAD FUNCTIONS

source("scripts/utils.R")
source("scripts/estimation.R")
source("scripts/simulation.R")

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

## 3) SIMULATION

# TODO: REPLACE LATER
# simulates a single day
simulated_day <- simulate_one_day(complete_arrival_rates, lambda_max)

##





## 4) OPTIMIZATION
  
## 5) VISUALIZATION AND ANALYSIS