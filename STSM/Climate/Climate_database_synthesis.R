                              # DATE: 02/10/2023 #
                           # AUTHOR: Dr. Marco Baldo #
                    # Title: Climate_database_synthesis

# This code is describing and processing the R script needed to process the 
# Czech Rep. forest sampling plots climate databases from the Cz meteorological
# agency. It is realized based on the source of the data (excel sheets per plot)
# and based on the fact that this data needs to be consistent with the iLand
# climete variables needed as input in the simulation model


# Important to downoload the required files and example download the folder in https://zenodo.org/records/10193821

library(readxl)
library(dplyr)

# Use a dataroot where store the files
dataroot <- "your path"
#-------------------------------------------------------------------------------
# DATAFRAME needed for the Climate request

# CLIMATE REQUEST DATAFRAME
datasoil_CZ <- readRDS("C:/Users/baldo/Desktop/Bottoms_Up/stsm_roma/official/datasoil_CZ.rds")

CLIM_DATA_REQUEST <- datasoil_CZ[,1:6]

# Save the new data frame as a CSV file with a similar name to the original
path <- paste0(dataroot, "CLIM_DATA_REQUEST.csv")
write.csv(CLIM_DATA_REQUEST, path)


# Specify the file path
file_path <- "C:/Users/baldo/Desktop/Climate/OUTPUT_ALL_ELEM_2.xlsx"  # Replace with the actual file path

# Read all sheets into a list of data frames
sheet_names <- excel_sheets(file_path)  # Get the names of all sheets

# Create an empty list to store the data frames
dfs <- list()

________________________________________________________________________________
# Loop through each sheet and import it into a data frame
for (sheet in sheet_names) {
  df <- read_excel(file_path, sheet = sheet)
  dfs[[sheet]] <- df
}

________________________________________________________________________________
# Access the imported data frames by sheet name
# For example, to access the first sheet:
sheet1 <- dfs[["Sheet1"]]

# Perform operations on the imported data frames as needed -------------- IMPORTANT
# For example, print the contents of each sheet
for (sheet in sheet_names) {
  print(dfs[[sheet]])
}

################################################################################
THE CODE NEEDED FOR CREATE MY DATABASE
################################################################################

library(DBI)
library(RSQLite)
library(readxl)
library(dplyr)

# Specify the file path of the Excel file
excel_file <- "C:/Users/baldo/Desktop/Climate/OUTPUT_ALL_ELEM_2.xlsx" # Replace with the actual file path if different

# Get the directory path of the Excel file
excel_dir <- dirname(excel_file)

#________________________________________________________________________________
# Specify the name of the SQLite database file
db_file <- file.path(excel_dir, "db_clim_plot_prova.sqlite")

# Read all sheets into a list of data frames
sheet_names <- excel_sheets(excel_file)

# Create an empty list to store the data frames
dfs <- list()


# Loop through each sheet
for (sheet in sheet_names) {
  # Read the sheet into a data frame
  df <- read_excel(excel_file, sheet = sheet)
  
  # Get the value in the first row and KEYID column as the sheet name
  sheet_name <- as.character(df[1, "KEYID"])
  
  # Assign the sheet name to the data frame in the list
  dfs[[sheet_name]] <- df
}

# Iterate through the list of data frames
for (sheet_name in names(dfs)) {
  data <- dfs[[sheet_name]]  # Get the data frame
  
  # Filter out data for the year 2021
  data <- data %>% filter(YEAR != 2021)
  
  # Check precipitation values
  data$SRA[data$SRA < 0] <- 0   # checks if the value of SRA = Precipitation is less than 0, and if it is, it sets the value of SRA to 0.
  
  
  # Create a new column for vapor pressure deficit
  data$VPD <- NA

  #             RADIANCE             #                   -----------------------
  # Convert "rad" from W/m²/day to MJ/m²/day 
  data <- data %>%
    mutate(RAD_Mj = coalesce(RAD * 3.6 / 1000))   # The new column is created by multiplying the "rad" column by 3.6 (to convert from W/m²/day to kJ/m²/day) and then dividing by 1,000 to convert from kJ to MJ.
  
  #               VPD                 #                  -----------------------                    
  # Go through every row and create the new variable for solving the equation for the VPD
  for (i in 1:nrow(data)) {
    rh <- data$H[i]
    temp <- data$T[i]
    
    # Create the VPD avg daily variables from the available data using Arden Buck equation
    
    es <- 0.611 * exp((17.27 * temp) / (temp + 237.3))  # Saturation vapor pressure - Arden Buck equation
    vpd <- es * (1 - (rh / 100))                        # Vapour pressure deficit
    
    # Check that the VPD is not equal to zero
    if (vpd == 0) {
      vpd <- 4.735757e-06  # Prevent division by zero or log(0) theoretically needs to be done with the temperature, but wasn't working. It is the temp = 0 that give at the VPD value 0
      }
    
    # Fulfill the column created previously VPD with NA, with the new values from the equation [i] is for all the values in succession
    data$VPD[i] <- vpd
    
  }
  
  # Select specific columns and change their order
  data <- data[, c("YEAR", "MONTH", "DAY", "TMI", "TMA", "SRA", "RAD_Mj", "VPD")]  # Select columns "year absolute year e.g.2009", "month 1:12", day = 1:31 if less days in the month put less, min_temp T Celsius", max_temp T Celsius, precipitation of the day (mm) , radiation = daily sum of global radiation per m² (MJ/m2/day), "VPD" average of the vapour pressure deficit of that day (kPa)
  
  # Edit column names
  colnames(data) <- c("year", "month", "day", "min_temp", "max_temp", "prec", "rad", "vpd")  # Rename columns
  
  # Update the data frame in the list with the computed VPD
  dfs[[sheet_name]] <- data
}


# Create a SQLite database connection
con <- dbConnect(RSQLite::SQLite(), db_file)

# Loop through each data frame and create a table in the database
for (sheet_name in names(dfs)) {
  df <- dfs[[sheet_name]]
  
  # Create a table with the sheet name in the database # Overwrite the Existing Table: If you want to replace the existing table with the new data from your data frame, you can set the overwrite argument to TRUE when using dbWriteTable. This will replace the existing table with the new data.
  dbWriteTable(con, name = sheet_name, value = df, overwrite = TRUE)
}

# Close the database connection
dbDisconnect(con)

