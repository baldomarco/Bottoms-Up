# Load required package
install.packages("markovchain")
library(markovchain)

# Define the data root
dataroot <- "C:/iLand/climate/wind/random_wind_event_new_script/"

# Read the existing data
a <- read.table(paste0(dataroot, "w6_severe_Tomas.txt"), header = TRUE)

# Initialize columns if not already present
a$modules.wind.dayOfYear <- 1
a$modules.wind.speed <- 0
a$modules.wind.direction <- 0
a$modules.wind.duration <- 0
a$model.climate.co2concentration <- 332

# Define Markov Chain states and transition matrix
states <- as.character(1:600)  # States represent years in the range 1 to 270
n <- length(states)
transition_matrix <- matrix(1/n, nrow = n, ncol = n, dimnames = list(states, states))

# Create Markov Chain object
mc <- new("markovchain", states = states, transitionMatrix = transition_matrix)

# Generate a random year series using the Markov Chain
set.seed(21)  # Set seed for reproducibility
series_length <- 90  # Generate 70 events to match the wind properties assignment
start_state <- sample(states, 1)
random_years <- markovchainSequence(n = series_length, markovchain = mc, t0 = start_state)

# Convert random_years to numeric
random_years <- as.numeric(random_years)

# Insert the generated years into the `a$year` column
a$year <- c(a$year, random_years)  # Append new random years
a$year <- sort(unique(a$year))     # Sort and remove duplicates

# Match the `events` for disturbance simulation
events <- which(a$year %in% random_years)

# Assign random wind properties for the events
set.seed(42)  # Set seed for reproducibility
a$modules.wind.speed[events] <- round(runif(length(events), 5.0, 7.5), 1)
a$modules.wind.direction[events] <- round(runif(length(events), 1, 360), 0)
a$modules.wind.duration[events] <- round(runif(length(events), 30, 90), 0)
a$modules.wind.dayOfYear[events] <- round(runif(length(events), 1, 365), 0)

# Write the updated dataframe to a new file
write.table(a, paste0(dataroot, "wind.txt"), col.names = TRUE, quote = FALSE, row.names = FALSE, sep = " ")

# Additional example for generating random samples and saving
x2 <- round(runif(100, 5.0, 7.5), 1)
write.table(x2, paste0(dataroot, "run1.txt"), col.names = TRUE, quote = FALSE, row.names = FALSE, sep = " ")
