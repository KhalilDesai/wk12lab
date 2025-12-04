library(tidyverse)
library(ggplot2)
library(patchwork)

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
      title = "Fleet Size vs Optimized Satisfaction Rate",
      x = "Fleet Size",
      y = "Optimized Satisfaction Rate") +
    theme_minimal()
  
  # save the plot
  file_name <- "results/fleet_size_vs_satisfaction.png"
  ggsave(filename = file_name, plot = plot)
  
  return()
}

#' Creates bar graph for initial placement and final arrangement after simulated day
#'
#' @description creates a plot comparing the distribution of bikes over the stations
#' before and after the simulated day
#' 
#' @param initial_placement a named vector - names are stations, values bike amounts
#' @param final_arrangement a named vector - names are stations, values bike amounts
#' 
#' @return NULL
before_vs_after_plot <- function(initial_placement, final_arrangement) {
  # turn placement/arrangement into data frames
  df_initial <- data.frame(station = names(initial_placement),
                           bikes = as.numeric(initial_placement),
                           time = "Initial")
  
  df_final <- data.frame(station = names(final_arrangement),
                         bikes = as.numeric(final_arrangement),
                         time = "Final")
  
  # make station a factor so we get the labels in the correct order
  stations_ordered <- sort(as.numeric(names(initial_placement)))
  df_initial$station <- factor(df_initial$station, levels = as.character(stations_ordered))
  df_final$station <- factor(df_final$station, levels = as.character(stations_ordered))
  
  # plot initial placement
  plot1 <- ggplot(df_initial, aes(x = station, y = bikes, fill = station)) +
    geom_bar(stat = "identity", fill = "steelblue", show.legend = FALSE) +
    labs(title = "Initial Placement for Fleet Size of 88", x = "Station",
         y = "Number of Bikes") +
    theme_minimal() 
  
  # plot final arrangement
  plot2 <- ggplot(df_final, aes(x = station, y = bikes, fill = station)) +
    geom_bar(stat = "identity", fill = "darkred", show.legend = FALSE) +
    labs(title = "Final Arrangement for Fleet Size of 88", x = "Station",
         y = "Number of Bikes") +
    theme_minimal()
  
  # stack plots vertically
  combined_plot <- plot1 / plot2
  
  # save the plot
  file_name <- "results/before_vs_after.png"
  ggsave(file_name, combined_plot, width = 8, height = 6)
  
  return()
}
