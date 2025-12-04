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
  # creates data frame
  data_frame <- data.frame(station = names(placement),
                           bikes = as.numeric(placement))
  # construct file name
  file_name <- paste0("results/fleet_size_", fleet_size, "_placement.csv")
  
  # save csv file
  write_csv(data_frame, file_name)
  
  return()
}

#' Creates plot of fleet_size vs satisfaction rate
#'
#' @description saves a plot of fleet_size vs satisfaction rate in the results
#' folder
#' 
#' @param fleet_sizes a numeric vector - fleet sizes
#' @param satisfaction_rates a numeric vector - corresponding satisfaction rates
#' 
#' @return NULL
plot_fleet_size_graph <- function(fleet_sizes, satisfaction_rates) {
  # combine data into one data frame
  data_frame <- data.frame(fleet_size = fleet_sizes,
                           satisfaction_rate = satisfaction_rates)
  
  # create the plot using ggplot
  plot <- ggplot(data_frame, aes(x = fleet_size, y = satisfaction_rate)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "darkred", size = 3) +
    scale_x_continuous(breaks = fleet_sizes) +
    ylim(0, 1) +
    labs(
      title = "Fleet Size vs Satisfaction Rate",
      x = "Fleet Size",
      y = "Satisfaction Rate") +
    theme_minimal()
  
  # save the plot
  file_name <- "results/fleet_size_vs_satisfaction.png"
  ggsave(filename = file_name, plot = plot)
  
  return()
}