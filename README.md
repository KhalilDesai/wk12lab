# PHP 1560 Week 12 Lab

### Author: Khalil Desai (worked with Zoey Dangleman in class)

Welcome to my implementation of our week 12 lab! My submission is structured as followed:
- `utils.R` contains helpful, minor functions used across the pipeline
- `estimation.R` contains functions for estimating arrival rates from provided data
- `simulation.R` contains functions for simulating ridership using estimated rates
- `placement.R` contains functions for evaluating and optimizing initial bike placements
- `analysis.R` contains functions for plotting, saving, and interpreting our results
- `main.R` runs the core pipeline

To run the program, simply source the `main.R` file. A few constants can be set at 
the top to adjust your results. The main file loads functions from all of the individual scripts,
then executes a complete estimation, simulation, and optimization pipeline. Comments
and documentation within the project provide more details. 

In `tests`, `testthat.R` contains unit tests for the main functions in my logical
sequence. To run the tests, simply source that file. 

I hope you have a great day!
