# Gas Consumption Time Series Analysis
# This script analyzes the time series data from a NetLogo gas consumption simulation

# Load required libraries
library(forecast)
library(ggplot2)
library(dplyr)
library(tseries)
library(zoo)

# Function to simulate data similar to the NetLogo model
simulate_gas_consumption <- function(years = 10) {
  # Initialize parameters from NetLogo model
  initial_population <- 1000
  base_consumption <- 450
  growth_rate <- 0.0003
  high_season_factor <- 1.2
  low_season_factor <- 0.8
  incident_probability <- 0.1
  random_variation <- 20
  minimum_consumption <- 400
  
  # Initialize time and data structures
  months <- years * 12
  population <- numeric(months)
  population[1] <- initial_population
  avg_consumption <- numeric(months)
  total_consumption <- numeric(months)
  time_data <- data.frame(
    year = numeric(months),
    month = numeric(months),
    month_name = character(months),
    season_factor = numeric(months),
    incident = logical(months),
    population = numeric(months),
    avg_consumption = numeric(months),
    total_consumption = numeric(months)
  )
  
  month_names <- c("January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December")
  
  # Simulate each month
  for (i in 1:months) {
    current_month <- ((i - 1) %% 12) + 1
    current_year <- floor((i - 1) / 12)
    
    # Set seasonal factors
    if (current_month %in% c(12, 1, 2)) {
      season_factor <- high_season_factor  # Winter
    } else if (current_month %in% c(3, 4, 5)) {
      # Spring - transitioning down
      season_factor <- 1 + (high_season_factor - 1) * (1 - (current_month - 2) / 3)
    } else if (current_month %in% c(6, 7, 8)) {
      season_factor <- low_season_factor  # Summer
    } else {
      # Fall - transitioning up
      season_factor <- low_season_factor + (high_season_factor - low_season_factor) * ((current_month - 8) / 3)
    }
    
    # Check for incidents
    incident <- runif(1) < incident_probability
    
    # Update population (monthly growth)
    if (i > 1) {
      monthly_growth_rate <- growth_rate / 12
      expected_new_residents <- population[i-1] * monthly_growth_rate
      whole_new_residents <- floor(expected_new_residents)
      fractional_part <- expected_new_residents - whole_new_residents
      
      population[i] <- population[i-1] + whole_new_residents
      if (runif(1) < fractional_part) {
        population[i] <- population[i] + 1
      }
    } else {
      population[i] <- initial_population
    }
    
    # Calculate average consumption per resident
    avg_cons <- base_consumption * season_factor
    
    # Add random variation
    avg_cons <- avg_cons + runif(1, -random_variation/2, random_variation/2)
    
    # Apply incident effect
    if (incident) {
      avg_cons <- avg_cons + runif(1, -5, 5)
    }
    
    # Ensure minimum consumption
    avg_cons <- max(avg_cons, minimum_consumption)
    
    avg_consumption[i] <- avg_cons
    total_consumption[i] <- avg_cons * population[i]
    
    # Store data
    time_data$year[i] <- current_year
    time_data$month[i] <- current_month
    time_data$month_name[i] <- month_names[current_month]
    time_data$season_factor[i] <- season_factor
    time_data$incident[i] <- incident
    time_data$population[i] <- population[i]
    time_data$avg_consumption[i] <- avg_consumption[i]
    time_data$total_consumption[i] <- total_consumption[i]
  }
  
  return(time_data)
}

# Simulate data (alternatively, you could import real data from NetLogo)
set.seed(123)  # For reproducibility
sim_data <- simulate_gas_consumption(10)

# Add date column for better time series handling
sim_data$date <- as.Date(paste(sim_data$year + 2020, sim_data$month, "1", sep="-"))

# Create time series objects
total_ts <- ts(sim_data$total_consumption, frequency = 12, start = c(0, 1))
avg_ts <- ts(sim_data$avg_consumption, frequency = 12, start = c(0, 1))
pop_ts <- ts(sim_data$population, frequency = 12, start = c(0, 1))

# -------------------------------
# Exploratory Data Analysis
# -------------------------------

# Plot the raw time series data
par(mfrow = c(3, 1), mar = c(2, 4, 2, 1))
plot(total_ts, main = "Total Gas Consumption Over Time", ylab = "Total Consumption")
plot(avg_ts, main = "Average Consumption Per Resident", ylab = "Average Consumption")
plot(pop_ts, main = "Population Growth", ylab = "Population")
par(mfrow = c(1, 1))

# Create seasonal plots
ggplot_seasonal <- function() {
  ggplot(sim_data, aes(x = factor(month), y = total_consumption, group = year, color = factor(year))) +
    geom_line() +
    labs(title = "Seasonal Pattern of Gas Consumption",
         x = "Month",
         y = "Total Consumption",
         color = "Year") +
    theme_minimal() +
    scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
}
ggplot_seasonal()

# Boxplot of consumption by month
ggplot_boxplot <- function() {
  ggplot(sim_data, aes(x = factor(month), y = total_consumption)) +
    geom_boxplot() +
    labs(title = "Monthly Distribution of Total Gas Consumption",
         x = "Month",
         y = "Total Consumption") +
    theme_minimal() +
    scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
}
ggplot_boxplot()

# -------------------------------
# Time Series Decomposition
# -------------------------------

# Decompose the time series into trend, seasonal, and random components
total_decomp <- decompose(total_ts)
plot(total_decomp)

# STL decomposition (Seasonal and Trend decomposition using Loess)
total_stl <- stl(total_ts, s.window = "periodic")
plot(total_stl)

# -------------------------------
# Export Results
# -------------------------------

# Function to export results to PDF
export_results <- function() {
  # This would be implemented with packages like rmarkdown or knitr
  # Note: Implementation depends on specific requirements
  cat("To export these results to PDF or other formats, you can use:\n")
  cat("1. The R Markdown package (rmarkdown)\n")
  cat("2. The knitr package\n")
  cat("3. Export plots individually with ggsave() or pdf()\n")
}

export_results()