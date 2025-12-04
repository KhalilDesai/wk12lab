library(tidyverse)
library(ggplot2)

#' Saves csv file with placement
#'
#' @description saves a placement as a csv file in results. file should have
#' name "{fleet_size}_placement.csv
#' 
#' @param fleet_size fleet size for file name
#' @param placement a named vector - names are stations, values are number 
#' of bikes
#' 
#' @return NULL
save_placement <- function(fleet_size, placement) {
  data_frame <- data.frame(station = names(placement),
                           bikes = as.numeric(placement))
  # construct file name
  file_name <- paste0("results/fleet_size_", fleet_size, "_placement.csv")
  
  # save csv file
  write_csv(data_frame, file_name)
  
  return()
}