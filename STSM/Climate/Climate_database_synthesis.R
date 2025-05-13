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
datasoil_CZ <- readRDS("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/R/stsm_roma/official/datasoil_CZ.rds")

CLIM_DATA_REQUEST <- datasoil_CZ[,1:6]

# Save the new data frame as a CSV file with a similar name to the original
path <- paste0(dataroot, "CLIM_DATA_REQUEST.csv")
write.csv(CLIM_DATA_REQUEST, path)


# Specify the file path
file_path <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/climate/OUTPUT_ALL_ELEM_2.xlsx"  # Replace with the actual file path

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
excel_file <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/climate/OUTPUT_ALL_ELEM_2.xlsx" # Replace with the actual file path if different

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


# THE END-----------------------------------------------------------------------

#-------------------------------------------------------------------------------

#                  SECTION ON TABLE CREATION

#-------------------------------------------------------------------------------
# Create a table of statistics for plots and sites

library(DBI)
library(RSQLite)

# 1st STEP----------------------------------------------------------------------

# Import database
db_path <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/climate/db_clim_plot_prova.sqlite"

# Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), db_path)

# Get a list of all table names
table_names <- dbListTables(con)

# Create a named list to store all tables
tables_list <- list()

# Loop through each table and read it into R
for (name in table_names) {
  tables_list[[name]] <- dbReadTable(con, name)
}

# Disconnect from the database
dbDisconnect(con)

# Now you can access each table using tables_list[["table_name"]]
# Example: View the first table
head(tables_list[[1]])

# 2nd STEP----------------------------------------------------------------------

# Crea una lista vuota per le tabelle modificate
tables_with_id <- list()

# Aggiungi la colonna plotID a ciascuna tabella
for (plotID in names(tables_list)) {
  df <- tables_list[[plotID]]
  df$plotID <- plotID  # Aggiungi una colonna plotID
  tables_with_id[[plotID]] <- df
}

# Combina tutte le tabelle in un unico dataframe
climate_data_all <- do.call(rbind, tables_with_id)

# Visualizza il risultato
head(climate_data_all)

# 3rd STEP----------------------------------------------------------------------

library(dplyr)
library(stringr)

# Crea consistenza nei nomi
climate_data_all <- climate_data_all %>%
  mutate(
    
    # Rimuove il prefisso iniziale
    plotID = str_remove(plotID, "^CZ_JH1_"),
    
    # Estrae il site (es. L1 da L1XL1_02)
    site = str_extract(plotID, "^L\\d"),
    
    # Trasforma L1XL1_02 in L1_02
    plotID = str_replace(plotID, "^L(\\d)XL\\d_", "L\\1_")
  )


# Filtra solo i plotID che sono presenti in BDV_predictors
climate_data_all <- climate_data_all %>%
  filter(plotID %in% BDV_predictors$plotID)

head(climate_data_all)

# 4th STEP----------------------------------------------------------------------

# Create the stats about annual temperature and precipitations
# PERIOD 1961-1990 & 1991-2020

# Add daily mean temperature first
climate_data_all <- climate_data_all %>%
  mutate(daily_mean_temp = (min_temp + max_temp) / 2)

# Table for 1961–1990
climate_summary_61_90 <- climate_data_all %>%
  filter(year >= 1961, year <= 1990) %>%
  group_by(plotID, site, year) %>%
  summarise(
    annual_mean_temp = mean(daily_mean_temp, na.rm = TRUE),
    annual_min_temp = min(min_temp, na.rm = TRUE),
    annual_max_temp = max(max_temp, na.rm = TRUE),
    annual_precip = sum(prec, na.rm = TRUE),
    annual_mean_vpd = mean(vpd, na.rm = TRUE),
    .groups = "drop"
  )

# Table for 1991–2020
climate_summary_91_20 <- climate_data_all %>%
  filter(year >= 1991, year <= 2020) %>%
  group_by(plotID, site, year) %>%
  summarise(
    annual_mean_temp = mean(daily_mean_temp, na.rm = TRUE),
    annual_min_temp = min(min_temp, na.rm = TRUE),
    annual_max_temp = max(max_temp, na.rm = TRUE),
    annual_precip = sum(prec, na.rm = TRUE),
    annual_mean_vpd = mean(vpd, na.rm = TRUE),
    .groups = "drop"
  )

# AGGREGATE FOR THE 30 YEAR PERIOD

# Aggregate for 1961–1990
climate_avg_61_90 <- climate_summary_61_90 %>%
  group_by(plotID, site) %>%
  summarise(
    mean_annual_temp = mean(annual_mean_temp, na.rm = TRUE),
    mean_annual_min_temp = mean(annual_min_temp, na.rm = TRUE),
    mean_annual_max_temp = mean(annual_max_temp, na.rm = TRUE),
    mean_annual_precip = mean(annual_precip, na.rm = TRUE),
    mean_annual_vpd = mean(annual_mean_vpd, na.rm = TRUE),
    .groups = "drop"
  )

# Aggregate for 1991–2020
climate_avg_91_20 <- climate_summary_91_20 %>%
  group_by(plotID, site) %>%
  summarise(
    mean_annual_temp = mean(annual_mean_temp, na.rm = TRUE),
    mean_annual_min_temp = mean(annual_min_temp, na.rm = TRUE),
    mean_annual_max_temp = mean(annual_max_temp, na.rm = TRUE),
    mean_annual_precip = mean(annual_precip, na.rm = TRUE),
    mean_annual_vpd = mean(annual_mean_vpd, na.rm = TRUE),
    .groups = "drop"
  )

# CAPTION
# Summary of long-term climatic conditions for each forest plot (plotID) and corresponding site, computed for the periods 1961–1990 and 1991–2020. For each 30-year period, the following variables are shown: (1) mean annual air temperature (°C), calculated as the average of daily mean temperatures (i.e., the mean of daily minimum and maximum temperatures); (2) mean annual minimum temperature (°C), representing the average of the lowest daily temperatures recorded in each year; (3) mean annual maximum temperature (°C), representing the average of the highest daily temperatures recorded in each year; (4) mean annual precipitation (mm), computed as the average of annual total precipitation sums; and (5) mean annual vapor pressure deficit (kPa), averaged from daily values over each year. Values represent multi-year means over the respective 30-year periods for each plot, providing insight into site-specific climate baselines and shifts over time.


# PER SITE AGGREGATION AND RANGE

# Site-level summaries for 1961–1990
climate_summary_1961_1990 <- climate_avg_61_90 %>%
  group_by(site) %>%
  summarise(
    mean_annual_temp_range = paste0(round(min(mean_annual_temp), 3), "–", round(max(mean_annual_temp), 3)),
    mean_annual_min_temp_range = paste0(round(min(mean_annual_min_temp), 3), "–", round(max(mean_annual_min_temp), 3)),
    mean_annual_max_temp_range = paste0(round(min(mean_annual_max_temp), 3), "–", round(max(mean_annual_max_temp), 3)),
    mean_annual_precip_range = paste0(round(min(mean_annual_precip), 3), "–", round(max(mean_annual_precip), 3)),
    mean_annual_vpd_range = paste0(round(min(mean_annual_vpd), 3), "–", round(max(mean_annual_vpd), 3))
  )

# Site-level summaries for 1991–2020
climate_summary_1991_2020 <- climate_avg_91_20 %>%
  group_by(site) %>%
  summarise(
    mean_annual_temp_range = paste0(round(min(mean_annual_temp), 3), "–", round(max(mean_annual_temp), 3)),
    mean_annual_min_temp_range = paste0(round(min(mean_annual_min_temp), 3), "–", round(max(mean_annual_min_temp), 3)),
    mean_annual_max_temp_range = paste0(round(min(mean_annual_max_temp), 3), "–", round(max(mean_annual_max_temp), 3)),
    mean_annual_precip_range = paste0(round(min(mean_annual_precip), 3), "–", round(max(mean_annual_precip), 3)),
    mean_annual_vpd_range = paste0(round(min(mean_annual_vpd), 3), "–", round(max(mean_annual_vpd), 3))
  )

# Table X. Summary of site-level climate variability for the periods 1961–1990 and 1991–2020. For each forest site, the table reports the range (minimum to maximum) of five climate variables calculated across all plots belonging to that site. The variables include annual mean daily temperature (°C), computed as the average of daily means across each year and then averaged over 30 years; annual average minimum temperature (°C); annual average maximum temperature (°C); total annual precipitation (mm); and annual mean vapor pressure deficit (kPa). These values reflect the spatial heterogeneity of climate conditions within each site, offering a basis for understanding climatic influences on ecological and biodiversity dynamics over time.




# write excel ------------------------------------------------------------------

# 1961-1990
writexl::write_xlsx(climate_summary_1961_1990, "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/climate/climate_summary_1961_1990.xlsx")

# 1991-2020
writexl::write_xlsx(climate_summary_1991_2020, "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/climate/climate_summary_1991_2020.xlsx")



#-------------------------------------------------------------------------------
# ==============================================================================

# Let's make some graphs
library(ggplot2)
library(tidyr)

# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/climate/"
pdf(paste0(dataroot, "BDV_Climate_V1.pdf"), height=9, width=16)

# Combine datasets and label period
climate_all <- bind_rows(
  climate_avg_61_90 %>% mutate(period = "1961-1990"),
  climate_avg_91_20 %>% mutate(period = "1991-2020")
)

#-------------------------------------------------------------------------------
# Scatter plot matrix (pairs) for selected variables
library(GGally)

ggpairs(climate_all,
        columns = c("mean_annual_temp", "mean_annual_min_temp", "mean_annual_max_temp", "mean_annual_precip", "mean_annual_vpd"),
        mapping = aes(color = site),
        upper = list(continuous = wrap("cor", size = 3)),
        lower = list(continuous = wrap("points", alpha = 0.5, size = 1))) +
  theme_bw()

#-------------------------------------------------------------------------------
# Example scatter plots
scatter_vars <- list(
  c("mean_annual_temp", "mean_annual_vpd"),
  c("mean_annual_temp", "mean_annual_precip"),
  c("mean_annual_precip", "mean_annual_vpd")
)

plot_list <- lapply(scatter_vars, function(vars) {
  ggplot(climate_all, aes_string(x = vars[1], y = vars[2], color = "period")) +
    geom_point(alpha = 0.6) +
    theme_bw() +
    labs(title = paste(vars[1], "vs", vars[2]), x = vars[1], y = vars[2])
})

# Display the plots (if using RMarkdown inline)
library(patchwork)
wrap_plots(plot_list)


# Separate per sites
scatter_vars <- list(
  c("mean_annual_temp", "mean_annual_vpd"),
  c("mean_annual_temp", "mean_annual_precip"),
  c("mean_annual_precip", "mean_annual_vpd")
)

plot_list <- lapply(scatter_vars, function(vars) {
  ggplot(climate_all, aes_string(x = vars[1], y = vars[2], color = "site", shape = "period")) +
    geom_point(alpha = 0.7, size = 2) +
    theme_bw() +
    labs(title = paste(vars[1], "vs", vars[2]), x = vars[1], y = vars[2])
})

wrap_plots(plot_list)

#-------------------------------------------------------------------------------
# 3D RAPPRESENATATION

library(plotly)

# Ensure 'period' is a factor and levels are in a known order
climate_all$period <- factor(climate_all$period, levels = c("1961-1990", "1991-2020"))

# Plotly symbol names: https://plotly.com/r/reference/#scatter-marker-symbol
plot_ly(
  data = climate_all,
  x = ~mean_annual_temp,
  y = ~mean_annual_precip,
  z = ~mean_annual_vpd,
  color = ~site,
  symbol = ~period,
  symbols = c("circle", "diamond"),  # https://plotly.com/r/reference/#scatter-marker-symbol
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 4, opacity = 0.7)
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Mean Annual Temperature"),
      yaxis = list(title = "Mean Annual Precipitation"),
      zaxis = list(title = "Mean Annual VPD")),
      legend = list(
        font = list(size = 16)  # 🔍 Increase legend text size here
    )
  )

#-------------------------------------------------------------------------------
# Second

# Ensure period is a factor
climate_all$period <- factor(climate_all$period)

# Create grid for prediction
temp_seq <- seq(min(climate_all$mean_annual_temp), max(climate_all$mean_annual_temp), length.out = 30)
precip_seq <- seq(min(climate_all$mean_annual_precip), max(climate_all$mean_annual_precip), length.out = 30)
grid <- expand.grid(mean_annual_temp = temp_seq, mean_annual_precip = precip_seq)

# Colors for surfaces per period
surface_colors <- c("gray", "orange")

# Begin plot
p <- plot_ly()

# Loop over periods to add points and fitted surface per group
for (i in seq_along(levels(climate_all$period))) {
  per <- levels(climate_all$period)[i]
  dat <- filter(climate_all, period == per)
  
  # Nonlinear polynomial model (2nd degree)
  fit <- lm(mean_annual_vpd ~ poly(mean_annual_temp, 2, raw=TRUE) +
              poly(mean_annual_precip, 2, raw=TRUE), data = dat)
  
  # Predict over the grid
  grid$mean_annual_vpd <- predict(fit, newdata = grid)
  
  # Turn z into a matrix
  z_matrix <- matrix(grid$mean_annual_vpd, nrow = length(temp_seq), ncol = length(precip_seq))
  
  # Add scatter points
  p <- add_trace(p,
                 data = dat,
                 x = ~mean_annual_temp,
                 y = ~mean_annual_precip,
                 z = ~mean_annual_vpd,
                 type = "scatter3d",
                 mode = "markers",
                 color = ~site,
                 symbol = ~period,
                 symbols = c("circle", "diamond"),
                 marker = list(size = 4, opacity = 0.7),
                 name = paste("Points", per),
                 showlegend = TRUE
  )
  
  # Add surface
  p <- add_surface(p,
                   x = temp_seq,
                   y = precip_seq,
                   z = z_matrix,
                   opacity = 0.4,
                   showscale = FALSE,
                   name = paste("Fit", per),
                   surfacecolor = matrix(rep(i, length(temp_seq) * length(precip_seq)),
                                         nrow = length(temp_seq)),
                   colorscale = list(c(0,1), c(surface_colors[i], surface_colors[i]))
  )
}

# Add layout
p <- layout(p,
            scene = list(
              xaxis = list(title = "Mean Annual Temperature"),
              yaxis = list(title = "Mean Annual Precipitation"),
              zaxis = list(title = "Mean Annual VPD")
            ),
            legend = list(font = list(size = 16))
)

p

#-------------------------------------------------------------------------------
# Reshape data to long format
climate_long <- climate_all %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "variable", values_to = "value")

# Density plots by variable, shaded by period
ggplot(climate_long, aes(x = value, fill = period)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  theme_bw() +
  labs(title = "Distributions of Climate Variables by Period",
       x = "Value", y = "Density")

# Per site
library(scales)  # for pretty_breaks()

# Filter plot-level data by period
climate_long_61_90 <- filter(climate_long, period == "1961-1990")
climate_long_91_20 <- filter(climate_long, period == "1991-2020")

# Histogram for 1961–1990
p1 <- ggplot(climate_long_61_90, aes(x = value, fill = site)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  scale_x_continuous(breaks = pretty_breaks(n = 8)) +
  theme_bw() +
  labs(title = "Histogram of Climate Variables by Site (1961–1990)",
       x = "Value", y = "Number of Plots")

# Histogram for 1991–2020
p2 <- ggplot(climate_long_91_20, aes(x = value, fill = site)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  scale_x_continuous(breaks = pretty_breaks(n = 8)) +
  theme_bw() +
  labs(title = "Histogram of Climate Variables by Site (1991–2020)",
       x = "Value", y = "Number of Plots")

# Show
p1
p2

# Caption: Figure: Distribution of climate variables across sites for the periods 1961–1990 and 1991–2020. Histograms show the relative frequency (density) of plot-level values for each variable, grouped by site. The Y-axis represents the estimated density, allowing comparison of distribution shapes regardless of how many plots each site contains. Overlapping colors indicate where sites share similar ranges of values. Facets separate the variables for clear interpretation across climatic dimensions.

#-------------------------------------------------------------------------------
# Boxplot for each variable by site and period
climate_all_long <- climate_all %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "variable", values_to = "value")

ggplot(climate_all_long, aes(x = site, y = value, fill = period)) +
  geom_boxplot(outlier.shape = NA, coef = 1.5) +  # classical Tukey boxplots
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  theme_bw() +
  labs(title = "Climate Variable Distributions by Site and Period")

#-------------------------------------------------------------------------------
# Prepare for ribbon plot
ribbon_df <- bind_rows(
  climate_avg_61_90 %>%
    select(plotID, site, min = mean_annual_min_temp, max = mean_annual_max_temp, mean = mean_annual_temp) %>%
    mutate(period = "1961-1990"),
  climate_avg_91_20 %>%
    select(plotID, site, min = mean_annual_min_temp, max = mean_annual_max_temp, mean = mean_annual_temp) %>%
    mutate(period = "1991-2020")
) %>%
  group_by(site, period) %>%
  summarise(min = mean(min), max = mean(max), mean = mean(mean), .groups = "drop") %>%
  mutate(year = ifelse(period == "1961-1990", 1975, 2005))  # approximate midpoints

# Plot with ribbon per site
ggplot(ribbon_df, aes(x = year, group = site)) +
  geom_ribbon(aes(ymin = min, ymax = max, fill = site), alpha = 0.3) +
  geom_line(aes(y = mean, color = site), size = 1) +
  facet_wrap(~site) +
  theme_bw() +
  labs(title = "Temperature Change per Site", y = "Temperature (°C)", x = "Year")


##########    END     ###########
#--------------------------------

