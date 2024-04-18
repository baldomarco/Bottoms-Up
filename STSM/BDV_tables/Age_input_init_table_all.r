# Marco Baldo 
# 17/12/2023
# baldo@fld.czu.cz 

library(dplyr)
library(readxl)
library(writexl)


# Import the tree age table derived from the age allometric function Katka created
Katka_age_table <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Katka/CZ_JH1_EstimatedTreeAgeFromYielTables05042024.xlsx", sheet = 2)

# Transform the tree age decimal values in integers
Katka_age_table$age <- round(Katka_age_table$age)

str(Katka_age_table)
unique(Katka_age_table$plotID)

______________________________________________________________

# Function to process each text file
process_text_file <- function(file_path) {
  # Extract plot ID from file name
  plotID <- strsplit(basename(file_path), "_init\\.txt$")
  plotID <- unlist(plotID)[1]  # Extract the first element of the list
  print("Extracted plot ID from file name:")
  print(plotID)
  
  # Read the init file
  init_data <- read.table(file_path, header = TRUE, sep = ";")
  
  # Print the entire Katka_age_table to check plot IDs
  print("Plot IDs in Katka_age_table:")
  print(unique(Katka_age_table$plotID))
  
  # Extract age data corresponding to plot_ID from Katka_age_table
  age_data <- Katka_age_table[Katka_age_table$plotID == plotID, "age"]
  print("Original age_data:")
  print(age_data)
  
  # Check if age_data is empty
  if (length(age_data) == 0) {
    print("No age data found for plot ID:", plotID)
    return(NULL)
  }
  
  # Extract numeric values from age_data
  age_values <- as.numeric(age_data$age)
  
  # Determine the number of times to repeat the vector (in your case, 4)
  num_repeats <- 4
  
  # Replicate the entire vector `num_repeats` times
  repeated_series <- rep(age_values, times = num_repeats)
  
  print("Repeated series:")
  print(repeated_series)
  
  round_rep_age <- as.integer(round(repeated_series))
  
  # Create a new dataframe with the replicated values in a single column
  new_age_data <- data.frame(age = round_rep_age)
  
  # Combine columns between the init and age dataframes
  init_data <- cbind(init_data, new_age_data)
  
  # Write the updated data to a new file
  write.table(init_data, 
              file = paste0(directory, plotID, "_init_age_CORR.txt"),
              append = FALSE, 
              quote = FALSE, 
              sep = " ", 
              eol = "\n", 
              na = "NA",
              dec = ".", 
              row.names = FALSE, 
              col.names = TRUE)
  
  return(NULL)  # Return NULL since we're writing to file directly
}

# Process each text file in the directory
lapply(text_files, process_text_file)


____ __________ _____________ ___________ ____________ _______
# Old part only for one single plot

# Import the required tables
age_L1_10 <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/Plot L1_10 TreeAgesDerivedfromYieldTables.xlsx")
init_L1_10 <- read.table("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/init/L1_10_init.txt", header = TRUE, sep = ";")

# Look at them
str(age_L1_10)
head(age_L1_10)

str(init_L1_10)
head(init_L1_10)

#-------------------------------------------------------------------------------
# Manipulate the age dataframe to have only what we are interest to know = age and multiply by 4 to have the same number of lines that we have in init (remind we are synthesise information from forest field samples at 50mx50m are in iLand model at 100mx100m area, that's why we repeat the trees x4)

filtered_age_L1_10 <- age_L1_10[, 12]

# Replicate each value four times
repeated_series <- c(filtered_age_L1_10$`mean age`, filtered_age_L1_10$`mean age`, filtered_age_L1_10$`mean age`, filtered_age_L1_10$`mean age`)

# Repeat the concatenated series in groups of four, repeating the same value four times consecutive. This does not work in our case because with this code it groups the same tree one in succession of the other. In my case I have a series of trees with precise geographic coordinates for each, grouped in a 50x50m area. I then translate the same ones by 50m east, south and southeast to cover a quadrant of 1 ha. So I need the series to be in succession not the individual values.
# rep_age <- rep(repeated_series, each = 4)

# Round just for the integers
round_rep_age <- as.integer(round(as.numeric(repeated_series)))

# Create a new dataframe with the replicated values in a single column
new_age_L1_10 <- data.frame(round_rep_age)

# Print the new dataframe
print(new_age_L1_10)

mean(new_age_L1_10$round_rep_age)
#-------------------------------------------------------------------------------
# Combind columns between the init_L1_10 dataframe and age
init_L1_10 <- bind_cols(init_L1_10, age = new_age_L1_10$round_rep_age)

# Write the updated data to the same path

write.table(init_L1_10, 
            file = "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/init/L1_10_init_age_CORR.txt",
            append = FALSE, 
            quote = FALSE, 
            sep = " ", 
            eol = "\n", 
            na = "NA",
            dec = ".", 
            row.names = FALSE, 
            col.names = TRUE)

