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

# Create a directory where there are the text file to edit and write in:
directory <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/gis/init/init/"

# Get a list of all text files in the directory
text_files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)

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
