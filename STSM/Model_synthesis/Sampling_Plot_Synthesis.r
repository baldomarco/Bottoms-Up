
#                                       Dr. Marco Baldo, MSc
# 
#                                22/03/2023  Sapienza - University of Rome


#            This script read and manipulate the Bottoms Up database of multi-taxonomic samples devided in plots
#            for the study region of the Czech Republic and prepare the input tables required to synthesise the 
#            forest and sites structures into the iLand model of forest dynamics and disturbances simulations


# To convert a list of coordinates from angular to metric units in R
# To set a smaller value for cellsize, you can modify the cellsize argument in the rasterize() function. For example, to set cellsize to 0.1, you can use the following 

load("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/R/stsm_roma/alldata (1).RData") # Bottoms-Up data

#-------------------------------------------------------------------------------
# Create the framework for make the plot list from the central DB

# FIRST METHOD USED TO BUILD A SINGLE PLOT EXCEL FILE OF SITE L1-L6 FROM BOTTOMS UP IN ILAND

library(rgdal)
library(tidyr)
library(dplyr)
library(geosphere)
library(sf)
library(ggpubr)
library(ggplot2)
library(writexl)
library(MASS)


#-------------------------------------------------------------------------------
# Create the framework for make the plot list from the central DB

# First select only CZ site (CZ_JH1)

# Subset the db for the CZ_JH1 group of sites
CZ_JH1 <- subset(treedata, file == "Raw_data_structure20200515_CZ_JH1.xlsx")

# Check it
print(CZ_JH1)
str(CZ_JH1)

# See how many sites we have and their names
unique_sites <- unique(CZ_JH1$siteID)  # alternative unique_sites <- unique(CZ_JH1[,"siteID"])
print(unique_sites) # 6 sites

# See how many plots we have and their names
unique_plots <- unique(CZ_JH1$plotID)  # alternative unique_plots <- unique(CZ_JH1[,"plotID"])
print(unique_plots) # 99 plots
length(unique_plots)

#_______________________________________________________________________________
# Subset per sites

# Get the unique values in the siteID column
sites <- unique(CZ_JH1$siteID)

# Create a list to store the subsets
site_subsets <- list()

# Loop through the unique values in the siteID column
for (site in sites){
  subset_df <- subset(CZ_JH1, siteID == site) # subset by sites
  # add the subset to the list
  site_subsets[[site]] <- subset_df
}

print(site_subsets) # Here I have a database subseted into 6 different dataframes

#-------------------------------------------------------------------------------
#_______________________________________________________________________________
# Subset per plots

# Get the unique values in the siteID column
plots <- unique(CZ_JH1$plotID)

# Create a list to store the subsets
plot_subsets <- list()

# Loop through the unique values in the siteID column
for (plot in plots){
  subset_df <- subset(CZ_JH1, plotID == plot) # subset by plots
  # add the subset to the list
  plot_subsets[[plot]] <- subset_df
}

print(plot_subsets) # Here I have a database subseted into 6 different dataframes

#-------------------------------------------------------------------------------
#_______________________________________________________________________________
# Subset site 1
# Subset the db for the CZ_JH1 group of sites
CZ_JH1_L1 <- subset(treedata, siteID == "CZ_JH1_L1")

# Check it
print(CZ_JH1_L1)

# See how many plots we have and their names
L1_plots <- unique(CZ_JH1_L1$plotID)  # alternative unique_plots <- unique(CZ_JH1[,"plotID"])
print(L1_plots) # 18 plots

# Create a list to store the subsets
L1_plot_subsets <- list()

# Loop through the unique values in the siteID column
for (plot in L1_plots){
  subset_df <- subset(CZ_JH1_L1, plotID == plot) # subset by plots
  # add the subset to the list
  L1_plot_subsets[[plot]] <- subset_df
}

# USED TO BUILD THE SINGLE PLOT OF SITE 1 EXCEL IN FOLDER PLOT(BOTTOMS UP)

L1_plots <- unique(CZ_JH1_L1$plotID)
print(L1_plots) # 18 plots

# Create a list to store the subsets
L1_plot_subsets <- list()

for (plot in L1_plots){
  
  subset_df <- subset(CZ_JH1_L1, plotID == plot)
  
  # remove rows with missing values in 'coordx'
  subset_clean <- subset_df %>% drop_na(coordx)
  
  # Convert the data to a spatial object with WGS84 CRS
  data_sf <- st_as_sf(subset_clean, coords = c("coordx", "coordy"), crs = 4326)
  
  # Project the data to a metric CRS with 1m resolution for WGS84                                                    
  # proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=m +no_defs"
  
  # Project the data to a metric CRS with 1cm resolution for WGS84
  proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=cm +no_defs"
  
  # Transform the data coordinates in the new projection you set in proj
  data_proj <- st_transform(data_sf, proj)
  
  # Extract the projected x and y coordinates from the data
  subset_clean$x <- st_coordinates(data_proj)[, 1]
  subset_clean$y <- st_coordinates(data_proj)[, 2]
  
  # Create a directory to store the plots
  plot_dir <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/" 
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir)
  }
  
  # Loop over the plotIDs and create a plot for each one
  for (i in seq_along(L1_plots)) {
    plot_data <- subset_clean %>% filter(plotID == L1_plots[i])
    
    # Create the plot
    p1 <- ggplot(plot_data, aes(x = x, y = y)) +
      geom_point() +
      ggtitle(paste0("Plot ", L1_plots[i], " - Metric Coordinates"))+
      ylab("Latitude [cm]")+
      xlab("Longitude [cm]")
    
    p2 <- ggplot(plot_data, aes(x = coordx, y = coordy)) +
      geom_point() +
      ggtitle(paste0("Plot ", L1_plots[i], " - Angular Coordinate"))+
      ylab("Latitude [ϕ - degree]")+
      xlab("Longitude [λ - degree]")
    
    combined_plot <- ggpubr::ggarrange(p1, p2, nrow = 1, ncol = 2, common.legend = TRUE)
    
    # Open a new graphics device
    dev.new()
    
    # Display the combined plot
    print(combined_plot)
    
    # Save the plot as PDF and PNG
    file_name <- paste0("plot_", L1_plots[i])
    ggsave(file.path(plot_dir, paste0(file_name, ".pdf")))
    ggsave(file.path(plot_dir, paste0(file_name, ".png")))
    
  }
  
  # add the subset to the list
  L1_plot_subsets[[plot]] <- subset_clean
  
  # Create a unique file name for the XLSX
  file_name <- paste0("plot_", plot)
  
  dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/"
  write_xlsx(L1_plot_subsets[[plot]],paste0(dataroot,file_name,".xlsx"))
  
}


#-------------------------------------------------------------------------------
#_______________________________________________________________________________
# Subset site 2
# Subset the db for the CZ_JH1 group of sites
CZ_JH1_L2 <- subset(treedata, siteID == "CZ_JH1_L2")

# Check it
print(CZ_JH1_L2)

# See how many plots we have and their names
L2_plots <- unique(CZ_JH1_L2$plotID)  # alternative unique_plots <- unique(CZ_JH1[,"plotID"])
print(L2_plots) # 18 plots

# Create a list to store the subsets
L2_plot_subsets <- list()

# Loop through the unique values in the siteID column
for (plot in L2_plots){
  subset_df <- subset(CZ_JH1_L2, plotID == plot) # subset by plots
  # add the subset to the list
  L2_plot_subsets[[plot]] <- subset_df
}

# USED TO BUILD THE SINGLE PLOT OF SITE 1 EXCEL IN FOLDER PLOT(BOTTOMS UP)

L2_plots <- unique(CZ_JH1_L2$plotID)
print(L2_plots) # 18 plots

# Create a list to store the subsets
L2_plot_subsets <- list()

for (plot in L2_plots){
  
  subset_df <- subset(CZ_JH1_L2, plotID == plot)
  
  # remove rows with missing values in 'coordx'
  subset_clean <- subset_df %>% drop_na(coordx)
  
  # Convert the data to a spatial object with WGS84 CRS
  data_sf <- st_as_sf(subset_clean, coords = c("coordx", "coordy"), crs = 4326)
  
  # Project the data to a metric CRS with 1m resolution for WGS84                                                    
  # proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=m +no_defs"
  
  # Project the data to a metric CRS with 1cm resolution for WGS84
  proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=cm +no_defs"
  
  # Transform the data coordinates in the new projection you set in proj
  data_proj <- st_transform(data_sf, proj)
  
  # Extract the projected x and y coordinates from the data
  subset_clean$x <- st_coordinates(data_proj)[, 1]
  subset_clean$y <- st_coordinates(data_proj)[, 2]
  
  # add the subset to the list
  L2_plot_subsets[[plot]] <- subset_clean
  
  # Create a unique file name for the XLSX
  file_name <- paste0("plot_", plot)
  
  dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/"
  write_xlsx(L2_plot_subsets[[plot]],paste0(dataroot,file_name,".xlsx"))
  
}


#-------------------------------------------------------------------------------
#_______________________________________________________________________________
# Subset site 3
# Subset the db for the CZ_JH1 group of sites
CZ_JH1_L3 <- subset(treedata, siteID == "CZ_JH1_L3")

# Check it
print(CZ_JH1_L3)

# See how many plots we have and their names
L3_plots <- unique(CZ_JH1_L3$plotID)  # alternative unique_plots <- unique(CZ_JH1[,"plotID"])
print(L3_plots) # 18 plots

# Create a list to store the subsets
L3_plot_subsets <- list()

# Loop through the unique values in the siteID column
for (plot in L3_plots){
  subset_df <- subset(CZ_JH1_L3, plotID == plot) # subset by plots
  # add the subset to the list
  L3_plot_subsets[[plot]] <- subset_df
}

# USED TO BUILD THE SINGLE PLOT OF SITE 1 EXCEL IN FOLDER PLOT(BOTTOMS UP)

L3_plots <- unique(CZ_JH1_L3$plotID)
print(L3_plots) # 18 plots

# Create a list to store the subsets
L3_plot_subsets <- list()

for (plot in L3_plots){
  
  subset_df <- subset(CZ_JH1_L3, plotID == plot)
  
  # remove rows with missing values in 'coordx'
  subset_clean <- subset_df %>% drop_na(coordx)
  
  # Convert the data to a spatial object with WGS84 CRS
  data_sf <- st_as_sf(subset_clean, coords = c("coordx", "coordy"), crs = 4326)
  
  # Project the data to a metric CRS with 1m resolution for WGS84                                                    
  # proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=m +no_defs"
  
  # Project the data to a metric CRS with 1cm resolution for WGS84
  proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=cm +no_defs"
  
  # Transform the data coordinates in the new projection you set in proj
  data_proj <- st_transform(data_sf, proj)
  
  # Extract the projected x and y coordinates from the data
  subset_clean$x <- st_coordinates(data_proj)[, 1]
  subset_clean$y <- st_coordinates(data_proj)[, 2]
  
  # add the subset to the list
  L3_plot_subsets[[plot]] <- subset_clean
  
  # Create a unique file name for the XLSX
  file_name <- paste0("plot_", plot)
  
  dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/"
  write_xlsx(L3_plot_subsets[[plot]],paste0(dataroot,file_name,".xlsx"))
  
}

#-------------------------------------------------------------------------------
#_______________________________________________________________________________
# Subset site 4
# Subset the db for the CZ_JH1 group of sites
CZ_JH1_L4 <- subset(treedata, siteID == "CZ_JH1_L4")

# Check it
print(CZ_JH1_L4)

# See how many plots we have and their names
L4_plots <- unique(CZ_JH1_L4$plotID)  # alternative unique_plots <- unique(CZ_JH1[,"plotID"])
print(L4_plots) # 18 plots

# Create a list to store the subsets
L4_plot_subsets <- list()

# Loop through the unique values in the siteID column
for (plot in L4_plots){
  subset_df <- subset(CZ_JH1_L4, plotID == plot) # subset by plots
  # add the subset to the list
  L4_plot_subsets[[plot]] <- subset_df
}

# USED TO BUILD THE SINGLE PLOT OF SITE 1 EXCEL IN FOLDER PLOT(BOTTOMS UP)

L4_plots <- unique(CZ_JH1_L4$plotID)
print(L4_plots) # 18 plots

# Create a list to store the subsets
L4_plot_subsets <- list()

for (plot in L4_plots){
  
  subset_df <- subset(CZ_JH1_L4, plotID == plot)
  
  # remove rows with missing values in 'coordx'
  subset_clean <- subset_df %>% drop_na(coordx)
  
  # Convert the data to a spatial object with WGS84 CRS
  data_sf <- st_as_sf(subset_clean, coords = c("coordx", "coordy"), crs = 4326)
  
  # Project the data to a metric CRS with 1m resolution for WGS84                                                    
  # proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=m +no_defs"
  
  # Project the data to a metric CRS with 1cm resolution for WGS84
  proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=cm +no_defs"
  
  # Transform the data coordinates in the new projection you set in proj
  data_proj <- st_transform(data_sf, proj)
  
  # Extract the projected x and y coordinates from the data
  subset_clean$x <- st_coordinates(data_proj)[, 1]
  subset_clean$y <- st_coordinates(data_proj)[, 2]
  
  # add the subset to the list
  L4_plot_subsets[[plot]] <- subset_clean
  
  # Create a unique file name for the XLSX
  file_name <- paste0("plot_", plot)
  
  dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/"
  write_xlsx(L4_plot_subsets[[plot]],paste0(dataroot,file_name,".xlsx"))
  
}

#-------------------------------------------------------------------------------
#_______________________________________________________________________________
# Subset site 5
# Subset the db for the CZ_JH1 group of sites
CZ_JH1_L5 <- subset(treedata, siteID == "CZ_JH1_L5")

# Check it
print(CZ_JH1_L5)

# See how many plots we have and their names
L5_plots <- unique(CZ_JH1_L5$plotID)  # alternative unique_plots <- unique(CZ_JH1[,"plotID"])
print(L5_plots) # 18 plots

# Create a list to store the subsets
L5_plot_subsets <- list()

# Loop through the unique values in the siteID column
for (plot in L5_plots){
  subset_df <- subset(CZ_JH1_L5, plotID == plot) # subset by plots
  # add the subset to the list
  L5_plot_subsets[[plot]] <- subset_df
}

# USED TO BUILD THE SINGLE PLOT OF SITE 1 EXCEL IN FOLDER PLOT(BOTTOMS UP)

L5_plots <- unique(CZ_JH1_L5$plotID)
print(L5_plots) # 18 plots

# Create a list to store the subsets
L5_plot_subsets <- list()

for (plot in L5_plots){
  
  subset_df <- subset(CZ_JH1_L5, plotID == plot)
  
  # remove rows with missing values in 'coordx'
  subset_clean <- subset_df %>% drop_na(coordx)
  
  # Convert the data to a spatial object with WGS84 CRS
  data_sf <- st_as_sf(subset_clean, coords = c("coordx", "coordy"), crs = 4326)
  
  # Project the data to a metric CRS with 1m resolution for WGS84                                                    
  # proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=m +no_defs"
  
  # Project the data to a metric CRS with 1cm resolution for WGS84
  proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=cm +no_defs"
  
  # Transform the data coordinates in the new projection you set in proj
  data_proj <- st_transform(data_sf, proj)
  
  # Extract the projected x and y coordinates from the data
  subset_clean$x <- st_coordinates(data_proj)[, 1]
  subset_clean$y <- st_coordinates(data_proj)[, 2]
  
  # add the subset to the list
  L5_plot_subsets[[plot]] <- subset_clean
  
  # Create a unique file name for the XLSX
  file_name <- paste0("plot_", plot)
  
  dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/"
  write_xlsx(L5_plot_subsets[[plot]],paste0(dataroot,file_name,".xlsx"))
  
}

#-------------------------------------------------------------------------------
#_______________________________________________________________________________
# Subset site 6
# Subset the db for the CZ_JH1 group of sites
CZ_JH1_L6 <- subset(treedata, siteID == "CZ_JH1_L6")

# Check it
print(CZ_JH1_L6)

# See how many plots we have and their names
L6_plots <- unique(CZ_JH1_L6$plotID)  # alternative unique_plots <- unique(CZ_JH1[,"plotID"])
print(L6_plots) # 18 plots

# Create a list to store the subsets
L6_plot_subsets <- list()

# Loop through the unique values in the siteID column
for (plot in L6_plots){
  subset_df <- subset(CZ_JH1_L6, plotID == plot) # subset by plots
  # add the subset to the list
  L6_plot_subsets[[plot]] <- subset_df
}

# USED TO BUILD THE SINGLE PLOT OF SITE 1 EXCEL IN FOLDER PLOT(BOTTOMS UP)

L6_plots <- unique(CZ_JH1_L6$plotID)
print(L6_plots) # 18 plots

# Create a list to store the subsets
L6_plot_subsets <- list()

for (plot in L6_plots){
  
  subset_df <- subset(CZ_JH1_L6, plotID == plot)
  
  # remove rows with missing values in 'coordx'
  subset_clean <- subset_df %>% drop_na(coordx)
  
  # Convert the data to a spatial object with WGS84 CRS
  data_sf <- st_as_sf(subset_clean, coords = c("coordx", "coordy"), crs = 4326)
  
  # Project the data to a metric CRS with 1m resolution for WGS84                                                    
  # proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=m +no_defs"
  
  # Project the data to a metric CRS with 1cm resolution for WGS84
  proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=cm +no_defs"
  
  # Transform the data coordinates in the new projection you set in proj
  data_proj <- st_transform(data_sf, proj)
  
  # Extract the projected x and y coordinates from the data
  subset_clean$x <- st_coordinates(data_proj)[, 1]
  subset_clean$y <- st_coordinates(data_proj)[, 2]
  
  # add the subset to the list
  L6_plot_subsets[[plot]] <- subset_clean
  
  # Create a unique file name for the XLSX
  file_name <- paste0("plot_", plot)
  
  dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/"
  write_xlsx(L6_plot_subsets[[plot]],paste0(dataroot,file_name,".xlsx"))
  
}

#-------------------------------------------------------------------------------
# SECOND METHOD ALL IN A LOOP
# First select only CZ site (CZ_JH1)

library(fields)
library(dplyr)

# Subset the db for the CZ_JH1 group of sites
CZ_JH1 <- subset(treedata, file == "Raw_data_structure20200515_CZ_JH1.xlsx")

# Check it
print(CZ_JH1)
str(CZ_JH1)

# See how many sites we have and their names
unique_sites <- unique(CZ_JH1$siteID)  # alternative unique_sites <- unique(CZ_JH1[,"siteID"])
print(unique_sites) # 6 sites

# See how many plots we have and their names
unique_plots <- unique(CZ_JH1$plotID)  # alternative unique_plots <- unique(CZ_JH1[,"plotID"])
print(unique_plots) # 99 plots
length(unique_plots)

#_______________________________________________________________________________
# Subset per sites

# Get the unique values in the siteID column
sites <- unique(CZ_JH1$siteID)

# Create a list to store the subsets
site_subsets <- list()

# Loop through the unique values in the siteID column
for (site in sites){
  subset_df <- subset(CZ_JH1, siteID == site) # subset by sites
  # add the subset to the list
  site_subsets[[site]] <- subset_df
}

print(site_subsets) # Here I have a database subseted into 6 different dataframes

#-------------------------------------------------------------------------------
#_______________________________________________________________________________
# Subset per plots

# Get the unique values in the siteID column
plots <- unique(CZ_JH1$plotID)

# Create a list to store the subsets
plot_subsets <- list()

# Loop through the unique values in the siteID column
for (plot in plots){
  subset_df <- subset(CZ_JH1, plotID == plot) # subset by plots
  # add the subset to the list
  plot_subsets[[plot]] <- subset_df
}

print(plot_subsets) # Here I have a database subseted into 6 different dataframes

#_______________________________________________________________________________
# Subset site 1
# Subset the db for the CZ_JH1 group of sites
CZ_JH1_L1 <- subset(treedata, siteID == "CZ_JH1_L1")

# Check it
print(CZ_JH1_L1)

#-------------------------------------------------------------------------------

# See how many plots we have and their names
L1_plots <- unique(CZ_JH1_L1$plotID)  # alternative unique_plots <- unique(CZ_JH1[,"plotID"])
print(L1_plots) # 18 plots

# Create a list to store the subsets
L1_plot_subsets <- list()

# Loop through the unique values in the siteID column
for (plot in L1_plots){
  subset_df <- subset(CZ_JH1_L1, plotID == plot) # subset by plots
  # add the subset to the list
  L1_plot_subsets[[plot]] <- subset_df
}

#-------------------------------------------------------------------------------
# Preallocate the results_list with NA values
results_list <- vector("list", length(L1_plot_subsets))
names(results_list) <- names(L1_plot_subsets)  # If you want to keep the same names as the original subset list

# Loop through each dataframe in the subset list
for (i in 1:length(L1_plot_subsets)) {
  
  # Perform your analysis on the current dataframe and store the results
  current_df <- L1_plot_subsets[[i]]
  
  # Check for missing values in coordx and coordy columns
  if (sum(is.na(current_df$coordx)) > 0 || sum(is.na(current_df$coordy)) > 0) {
    # Remove rows with missing values
    current_df <- na.omit(current_df[, c("coordx", "coordy")])
  }
  
  # Convert the data to a spatial object with WGS 84 CRS
  data_sf <- st_as_sf(current_df, coords = c("coordx", "coordy"), crs = 4326)
  
  # Project the data to a metric CRS with 1m resolution for WGS84                                                    
  # proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=m +no_defs"
  
  # Project the data to a metric CRS with 1cm resolution for WGS84
  proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=cm +no_defs"
  
  # Transform the data coordinates in the new projection you set in proj
  data_proj <- st_transform(data_sf, proj)
  
  # Extract the projected x and y coordinates from the data
  current_df$x <- st_coordinates(data_proj)[, 1]
  current_df$y <- st_coordinates(data_proj)[, 2]
  
  my_plot <- { 
    par(mfrow = c(1,2))
    plot(current_df$x, current_df$y, main = "Metric", xlab = "Long")
    plot(data_sf$coordx, data_sf$coordy, main = "Angular", xlab = "Long", ylab = "Lat")
  }
  
  # Create a unique file name for the PDF
  file_name <- paste0("plot_", i, ".pdf")
  
  dataroot <- ("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/Plot_coordinates_confront/")
  
  # Open a new PDF device with the unique file name
  pdf(my_plot, paste(dataroot, file_name), height=8, width=12)
  
  results_list[[i]] <- results
  
}


#-------------------------------------------------------------------------------
# Subset per sites and per plots my database CZ_JH1

# Get the unique values in the plotID column
plots <- unique(CZ_JH1$plotID)

# Create a list to store the subsets
plot_subsets <- list()

# Loop through the unique values in the siteID column
for (plot in plots){
  
  # Subset the df by plotID
  subset_df <- subset(CZ_JH1, plotID == plot)
  
  # Create a list to store the subset by site
  site_subsets <- list()
  
  # Get the unique values in the site column for this plot
  plot_sites <- unique(subset_df$siteID)
  
  # Loop through the unique value in the site column for this plot
  for (site in plot_sites){
    
    # Subset the dataframe by plot and site
    site_subsets_df <- subset(subset_df, siteID == site)
    
    # Add the subset to the site_subset list
    site_subsets[[site]] <- site_subsets_df
  }
  
  # Add the site_subsets list to the subset list, using the plot name as the index
  subsets[[plot]] <- site_subsets
}

print(subsets)

#-------------------------------------------------------------------------------
# Produce several excel per plot

# Assuming you have already created a list of dataframes named 'subset'

# Preallocate the results_list with NA values
results_list <- vector("list", length(plot_subsets))
names(results_list) <- names(plot_subsets)  # If you want to keep the same names as the original subset list

# Loop through each dataframe in the subset list
for (i in 1:length(plot_subsets)) {
  
  # Perform your analysis on the current dataframe and store the results
  current_df <- plot_subsets[[i]]
  
  # Convert the data to a spatial object with WGS84 CRS
  data_sf <- st_as_sf(current_df, coords = c("coordx", "coordy"), crs = 4326)
  
  # Project the data to a metric CRS with 1m resolution for WGS84                                                    
  # proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=m +no_defs"
  
  # Project the data to a metric CRS with 1cm resolution for WGS84
  proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=cm +no_defs"
  
  # Transform the data coordinates in the new projection you set in proj
  data_proj <- st_transform(data_sf, proj)
  
  # Extract the projected x and y coordinates from the data
  current_df$x <- st_coordinates(data_proj)[, 1]
  current_df$y <- st_coordinates(data_proj)[, 2]
  
  plot(data$x,data$y,main="cents")
  plot(data$coordx, data$coordy,main="degrees")
  
  # Create a unique file name for the PDF
  file_name_metric <- paste0("plot_metric", i, ".pdf")
  file_name_angular <- paste0("plot_angular", i, ".pdf")
  
  # Open a new PDF device with the unique file name
  pdf(file_name_metric)
  pdf(file_name_angular)
  
  # Plot the data and save it to the PDF
  plot(results)
  
  # Close the PDF device
  dev.off()
  
  # Compute the distance between points in meters
  data$distance <- distGeo(as.matrix(data[, c("coordx", "coordy")])) # * 1000
  
  
  results_list[[i]] <- results
  
}



#-------------------------------------------------------------------------------
#           SECOND FASE OF THE ANALYSIS AND DATA PREPARATION
#-------------------------------------------------------------------------------
# CREATE THE TABLE FOR ILAND TREE AND ENVIRONMENT INITIALIZATION
# read the data from the Excel file
library(readxl)
# Now you have all the data frames in data_list
#---------------------------------------------
# You can access each data frame using its corresponding file path

#---------------------------------------------
# data <- read_excel("C:/iLand/2023/plot_bottoms_up/plots/Raw_data_structure_CZ_JH1_final.xlsx")

#-------------------------------SITE CZ_JH1_L1---------------------------------------
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_03.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_07.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_10.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_13.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_17.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_18.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_22.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_24.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_26.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_27.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_31.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_33.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_34.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_36.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_38.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_43.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_44.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L1_48.xlsx")

#-------------------------------SITE CZ_JH1_L2---------------------------------------
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_01.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_05.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_06.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_09.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_13.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_14.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_16.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_17.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_18.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_20.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_26.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_27.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_30.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_32.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_33.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_34.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_38.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_44.xlsx")

#-------------------------------SITE CZ_JH1_L3---------------------------------------
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_10.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_12.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_16.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_18.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_23.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_24.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_26.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_27.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_02.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_31.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_32.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_34.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_37.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_38.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_41.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_42.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_06.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L3_09.xlsx")

#-------------------------------SITE CZ_JH1_L4---------------------------------------
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_10.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_11.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_16.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_17.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_18.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_20.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_23.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_25.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_26.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_27.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_02.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_30.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_31.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_32.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_33.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_39.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_42.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_04.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_06.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L4_09.xlsx")

#-------------------------------SITE CZ_JH1_L5---------------------------------------
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L5_01.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L5_05.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L5_12.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L5_18.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L5_19.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L5_21.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L5_25.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L5_28.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L5_32.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L5_33.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L5_37.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L5_38.xlsx")

#-------------------------------SITE CZ_JH1_L6---------------------------------------
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L6_01.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L6_02.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L6_03.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L6_04.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L6_05.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L6_08.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L6_10.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L6_11.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L6_14.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L6_15.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L6_17.xlsx")
# data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L6_19.xlsx")
data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L6_21.xlsx")

#data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/plot_L2_44.xlsx")

#------------------------------------------------------------------------------------
# Species present in the plot 
sp <- unique(data$treesp)
sp

# Types of forest stand
data$standID

# Forest cataegory
# Load
plotdescdata_CZ <- readRDS("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/R/stsm_roma/plotdescdata_CZ.rds")

# Create a vector with useful information!
forest_cat <- plotdescdata_CZ %>% dplyr::select(plotID, catego, fortyp, habtyp)
forest_cat

# View(forest_cat)

#---------------------------------------------------------------------
# IMPORTANT

# Convert the data to a spatial object with WGS84 CRS
library(sf)
data_sf <- st_as_sf(data, coords = c("coordx", "coordy"), crs = 4326)

# Project the data to a metric CRS with 1m resolution for WGS84                                                    
# proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=m +no_defs"

# Project the data to a metric CRS with 1cm resolution for WGS84
proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=cm +no_defs"

#------------------------------------------------------------------
# THIS FOR CZECHIA
# data_sf <- st_as_sf(data, coords = c("coordx", "coordy"), crs = 5514)
# proj <- "+proj=utm +zone=33 +ellps=bessel +towgs84=570.8,85.7,462.8,4.998,1.587,-9.050,4.072 +units=m +no_defs"

# THIS FOR ITALY
# data_sf <- st_as_sf(data, coords = c("coordx", "coordy"), crs = 32632) if in northen part of EPGS 3003 at the national level
# proj <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"

# THIS FOR NL
#data_sf <- st_as_sf(data, coords = c("coordx", "coordy"), crs = 28992)
# proj <- "+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# THIS FOR SWITZERLAND # Project the data to Swiss CH1903+ CRS
# data_sf <- st_as_sf(data, coords = c("coordx", "coordy"), crs = 21781)
# proj <- "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0.842,-0.236 +units=m +no_defs"

#----------------------------------------------------------------- 
# Transform the data coordinates in the new projection you set in proj
data_proj <- st_transform(data_sf, proj)

# Extract the projected x and y coordinates from the data
data$x <- st_coordinates(data_proj)[, 1]
data$y <- st_coordinates(data_proj)[, 2]

# Create the plot
par(mfrow = c(1,2))
plot(data$x, data$y, xlab = "Longitude [cm]", ylab = "Latitude [cm]", main = ("Plot L3_27 - Metric Coordinates"))
plot(data$coordx, data$coordy, xlab = "Longitude [degrees]", 
     ylab = "Latitude [degrees]", main = ("Plot L3_27 - Angular Coordinates"))
dev.off()

# Compute the distance between points in meters
data$distance <- distGeo(as.matrix(data[, c("coordx", "coordy")])) # * 1000


#------------------------------------------------------------------
# Try to extrapolate the corner point of the plot from the trees coordinates         # Yes, it is possible to calculate the corner points of a plot, given the plot size and the coordinates of trees within the plot. One way to do this is to find the maximum and minimum coordinates of the trees in both the x and y directions. These values can then be used to calculate the coordinates of the four corner points of the plot. Here is some sample code that you could use to calculate the corner points of a plot:

# Read in the data for the trees within the plot
# in case you start from the tree excel table.. 
# tree_data <- read.csv("tree_data.csv")

{# Calculate the minimum and maximum x and y coordinates of the trees
  min_x <- min(data$x)
  max_x <- max(data$x)
  min_y <- min(data$y)
  max_y <- max(data$y)
  
  # Calculate the coordinates of the four corners of the plot
  corner1 <- c(min_x - 25, min_y - 25)           # This is the corner for iland bottom left
  corner2 <- c(max_x + 25, min_y - 25)
  corner3 <- c(max_x + 25, max_y + 25)
  corner4 <- c(min_x - 25, max_y + 25)
  
  # Print the coordinates of the four corners of the plot
  cat("Corner 1: ", corner1[1], ",", corner1[2], "\n")
  cat("Corner 2: ", corner2[1], ",", corner2[2], "\n")
  cat("Corner 3: ", corner3[1], ",", corner3[2], "\n")
  cat("Corner 4: ", corner4[1], ",", corner4[2], "\n")}

# The formula I provided in the previous answer is a basic trigonometry formula for calculating the coordinates of a point based on its distance and direction from another point.
 
 # Formula behind
 # x2 = x1 + d * cos(theta)
 # y2 = y1 + d * sin(theta)
 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#                             Write out the RU and Stand grids
#------------------------------------------------------------------------------- 
 # This script is making RUgrid 100x100 and Standgrid for artifical landscape
 #library(raster)
 #library(fields)
 
 out.dataroot<-"C:/iLand/2023/plot_bottoms_up/gis/env_grid/"  # where I am working and put the data
 
 # WHERE ARE WE?
 x.coord.corner<- as.integer(corner1[1])
 y.coord.corner<- as.integer(corner1[2])
 













# SELECT THE COLUMNS
desired_columns <- dplyr::select(data, x, y, species, treedb, treeht)      # Select the dataset
colnames(desired_columns)<-c("x","y","species","dbh","height")      # CHANGE THE NAME OF THE COLUMNS

{desired_columns <- mutate(desired_columns, species = if_else(species == "abies", "piab", species))
  # desired_columns <- mutate(desired_columns, species = if_else(species == "alba", "abal", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "decidua", "lade", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "sylvestris", "pisy", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "sylvatica", "fasy", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "robur", "quro", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "pseudoplatanus", "acps", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "excelsior", "frex", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "betulus", "cabe", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "pendula", "bepe", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "incana", "alin", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "petraea", "qupe", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "glutinosa", "algl", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "sativa", "casa", species))
  # desired_columns <- mutate(desired_columns, species = if_else(species == "nigra", "pini", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "campestre", "acca", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "platanoides", "acpl", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "pubescence", "qupu", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "cembra", "pice", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "aucuparia", "soau", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "aria", "soar", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "avellana", "coav", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "viridis", "alvi", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "tremula", "potr", species))
  #desired_columns <- mutate(desired_columns, species = if_else(species == "nigra", "poni", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "cordata", "tico", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "platyphyllos", "tipl", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "glabra", "ulgl", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "caprea", "saca", species))
  desired_columns <- mutate(desired_columns, species = if_else(species == "pseudoacacia", "rops", species))}


# Convert from meters to cm the tree height columns
# desired_columns$treeheight <- desired_columns$treeheight * 100 # PICUS

# Correct for not having the integers
desired_columns$x <- as.integer(desired_columns$x)
desired_columns$y <- as.integer(desired_columns$y)    
desired_columns$height <- as.numeric(desired_columns$height)    

#---------------------------------------------------------------------
# Shift the numbers from metrics coordinates in iland quadrant coordinates

# Create the element for shift the coordinates to 0 - 50 meters

#----------------------------------------------------------------------
# 1st way use the min x and y of the tree coordinates and reduce it of 50cm to have the corner 0,0
min_x <- min(data$x)
min_y <- min(data$y)

min_x <- min_x - 20 # becarful 20 is cm!!!
min_y <- min_y - 20

#--------------------------------------------------------------------

# 2nd way Let's apply the corner bottom left coordinates of the plot (also more robust as method)
min_x <- x.coord.corner
min_y <- y.coord.corner


# Make coordinates in the way to be zero (you should use the corner maybe)
new_coordinate_x <- min_x
desired_columns$x <- desired_columns$x - new_coordinate_x

new_coordinate_y <- min_y
desired_columns$y <- desired_columns$y - new_coordinate_y 

# Convert from cm in meters the coordinates columns
desired_columns$x <- as.numeric(desired_columns$x) / 100
desired_columns$y <- as.numeric(desired_columns$y) / 100

desired_columns$x <- round(desired_columns$x, 2)
desired_columns$y <- round(desired_columns$y, 2)
desired_columns$height <- round(desired_columns$height, 2)

# Add a new column 'tree_id' with sequential IDs
desired_columns <- desired_columns %>% 
  mutate(tree_id = row_number())

# Set threshold
threshold <- 50

# Filter out rows where x or y exceed the threshold
trees_above_threshold <- desired_columns[desired_columns$x > threshold | desired_columns$y > threshold, ]
trees_below_threshold <- desired_columns[desired_columns$x <= threshold & desired_columns$y <= threshold, ]

# Perform kernel density estimation for trees within the threshold
kde_result <- kde(trees_below_threshold[, c("x", "y")])

# Determine number of kernels based on the number of trees outside the threshold
num_kernels <- nrow(trees_above_threshold)

# Sample new coordinates from the kernel density estimate
sampled_points <- rkde(num_kernels, kde_result)

# Reduce the numbers at 2 decimals
sampled_points <- round(sampled_points, 2)

# Check the class of sampled_points
class(sampled_points)

# If sampled_points is not a dataframe, convert it to a dataframe
sampled_points <- as.data.frame(sampled_points)

# Replace x and y coordinates in sampled_points exceeding the threshold with the threshold value
sampled_points$x <- pmin(sampled_points$x, threshold)
sampled_points$y <- pmin(sampled_points$y, threshold)

# Replace x and y coordinates in sampled_points falling below zero with 0
sampled_points$x <- pmax(sampled_points$x, 0)
sampled_points$y <- pmax(sampled_points$y, 0)

# Now, try the mutate() function again
trees_above_threshold <- trees_above_threshold %>%
  mutate(x = sampled_points$x,
         y = sampled_points$y)

# Now, try the mutate() function again
trees_above_threshold <- trees_above_threshold %>%
  mutate(x = sampled_points$V1,
         y = sampled_points$V2)

desired_columns <- bind_rows(trees_above_threshold, trees_below_threshold) %>%
  arrange(tree_id)

desired_columns <- dplyr::select(desired_columns, -tree_id)

# Print the modified data frame
print(desired_columns)

#--------------------------------------------------------------------------------
# Replicate the same forest structure to cover the 100x100m from the source data at 50x50

{# switch the x,y coordinates of x=+50, y=+0
  desired_columns2 <- desired_columns
  desired_columns2$x <- desired_columns$x + 50
  desired_columns2$y <- desired_columns$y
  
  # switch the x,y coordinates of x=+0, y=+50
  desired_columns3 <- desired_columns
  desired_columns3$x <- desired_columns$x 
  desired_columns3$y <- desired_columns$y + 50
  
  # switch the x,y coordinates of x= +50, y= +50
  desired_columns4 <- desired_columns
  desired_columns4$x <- desired_columns$x + 50
  desired_columns4$y <- desired_columns$y + 50
  
  # Combine the four data frames into a single data frame
  final_data <- rbind(desired_columns, desired_columns2, desired_columns3, desired_columns4)}


#---------------------INIT INPUT TABLE -----------------------------------------
# write 
out.dataroot<-"C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/gis/init/init/"    # use the same place

# write.table(desired_columns, file=paste(out.dataroot,"_init.txt",sep=";"), append = FALSE, quote = FALSE, sep = "\t" means sep by space,
#             eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=TRUE)

write.table(final_data, file = paste(out.dataroot, "L6_21_init.txt", sep = ""),
            append = FALSE, quote = FALSE, sep = ";", eol = "\n", na = "NA",
            dec = ".", row.names = FALSE, col.names = TRUE)


#-------------------------------------------------------------------------------
#                                       END
#
#-------------------------------------------------------------------------------


# RU GRID----------------------------------------------------------------------
# SIZE OF THE AREA THAT I WANT TO COVER
# 1 x 1   100m -> 1x1

xn<-1
yn<-1

# HERE THE STANDGRID HAS THE SAME RESOLUTION AS THE RESOURCE UNITS

# Define which values will be the IDs (environment) of resource units and stands. 
# I define here only one resource unit:

RU.values<-rep(110, xn*yn)           # but can be a sequence with this e.g.: seq(1000,length.out = xn*yn, by=1)
RUindex.values<-c(0:(xn*yn-1))  # this is RU index, always starts from 0, iLand is generating this during the simulation runs.

# Order them into a matrix:
RU.grid<-matrix(RU.values,ncol=xn)
RUindex.grid<-matrix(RUindex.values,ncol=xn)

#------------------------------------------------------------------------------------------------------------
# PLOT GRID
# SIZE OF THE AREA THAT I WANT TO COVER
# 1 x 1   100m -> 1x1

xn_<-10
yn_<-10

# For stands I usually put here IDs referring to the Site index (init) for which we will populate the trees. To a unified structure I just put here one SI, SI=26
Stand.values<-rep(110, xn_*yn_) # Here I should change this number based on the climate and plot number related

# Order them into a matrix:
Stand.grid<-matrix(Stand.values,ncol=xn_)

#-------------------------------------------------------------------------------
# plot them:
set.panel(2,2)
par(mar=c(2,4,2,4))
image.plot(RU.grid,main=paste0("RU grid: ",xn,"x",yn),legend.width = 1.2)
image.plot(RU.grid,main=paste0("Plot grid: ",xn_,"x",yn_),legend.width = 1.2)
image.plot(Stand.grid, main="Stand grid")
image.plot(RUindex.grid, main="RU index grid")
dev.off()

#---------------------------------------ENVIRONMENT-------------------------------------
RU.grid.file<-paste0(out.dataroot,"environment_grid_plot_L2_09.asc")

write.table(paste("NCOLS",	xn, sep="\t"), file = RU.grid.file, append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)

write.table(paste("NROWS",	yn, sep="\t"), file = RU.grid.file, append = T, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)

write.table(paste("XLLCORNER",	x.coord.corner, sep="\t"), file = RU.grid.file, append = T, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)

write.table(paste("YLLCORNER" ,y.coord.corner, sep="\t"), file = RU.grid.file, append = T, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)

write.table(paste("CELLSIZE",	"100", sep="\t"), file = RU.grid.file, append = T, quote = FALSE, sep = "\t",        
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)

write.table(paste("NODATA_value"	,"-9999", sep="\t"), file = RU.grid.file, append = T, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)

write.table(RU.grid, file = RU.grid.file, append = T, quote = FALSE, sep = "\t",
            eol = "\n", na = "-9999", dec = ".", row.names = FALSE, col.names=FALSE)

#---------------------------------------- STAND------------------

S.grid.file<-paste0(out.dataroot,"plot_L6_21.asc")

# READ IN AN EXAMPLE PLOT GRID HEADER
write.table(paste("NCOLS",	xn_, sep="\t"), file = S.grid.file, append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)

write.table(paste("NROWS",	yn_, sep="\t"), file = S.grid.file, append = T, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)

write.table(paste("XLLCORNER",	x.coord.corner, sep="\t"), file = S.grid.file, append = T, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)

write.table(paste("YLLCORNER" ,y.coord.corner, sep="\t"), file = S.grid.file, append = T, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)

write.table(paste("CELLSIZE",	"10", sep="\t"), file = S.grid.file, append = T, quote = FALSE, sep = "\t",             ######*****if cellsize changes change here
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)

write.table(paste("NODATA_value"	,"-9999", sep="\t"), file = S.grid.file, append = T, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)

write.table(Stand.grid, file = S.grid.file, append = T, quote = FALSE, sep = "\t",
            eol = "\n", na = "-9999", dec = ".", row.names = FALSE, col.names=FALSE)



#------------------------------- MOST IMPORTANT PART ---------------------------
#                           CREATE THE TABLE FOR THE TREES
#-------------------------------------------------------------------------------


--------------------------------------------------------------------------------





#----------ALternative way to replicate values-----------------------------
# FIRST WAY WITH THE LOOP - GOOD BUT IS OVERWRITING THE DATAFRAME (any cycle repeat the coordinates(rows=trees) of the previous new data frame that is the double of the previous one, in this case i1=122x2, i2=244x2, i3=488x2)
# Replicate the same forest structure to cover the 100x100m from the data at 50x50

replicated_columns <- desired_columns[c("x", "y")]
shifts <- data.frame(
  x_shift = c(50, 50, 0),
  y_shift = c(0, 50, 50)
)
for (i in 1:3) {
  replicated_columns_shifted <- replicated_columns + as.numeric(shifts[i, ])
  colnames(replicated_columns_shifted) <- colnames(replicated_columns)
  desired_columns_shifted <- cbind(replicated_columns_shifted, 
                                   desired_columns[, c("bhdfrom", "treeheight", "species")])
  desired_columns <- rbind(desired_columns, desired_columns_shifted)
}

________________________________________________________________________________
# SECOND WAY
# Replicate the same forest structure to cover the 100x100m from the data at 50x50

# Create shifts matrix
shifts <- matrix(c(50, 0, 0, 50,
                   50, 50, 0, 0,
                   0, 50, 50, 50), ncol = 2, byrow = TRUE)

# Create a list of data frames with shifted coordinates
shifted_trees <- lapply(1:3, function(i) {
  replicated_columns <- desired_columns[c("x", "y")]
  replicated_columns <- replicated_columns + shifts[i, ]
  cbind(replicated_columns, desired_columns[, -c(1, 2)])
})

# Combine the list of data frames into a single data frame
result <- do.call(rbind, shifted_trees)

desired_columns <- rbind(desired_columns, result)
