library(readxl)
library(sf)
library(geosphere) # Install and load the geosphere package if you haven't already # if (!requireNamespace("geosphere", quietly = TRUE)) {install.packages("geosphere")}
library(raster)
library(fields)


library(rgdal)
library(tidyr)
library(dplyr)
library(geosphere)
library(sf)
library(ggpubr)
library(ggplot2)
library(writexl)
library(MASS)

# Define the directory containing the Excel files
directory <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/clean_plot/"

# List all the .xlsx files in the directory
file_paths <- list.files(directory, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty list to store data frames
data_list <- list()

# Loop through each file path
for (file_path in file_paths) {
  
  # Extract the file name without extension
  file_name <- basename(file_path)  # Get the base name of the file
  file_name <- tools::file_path_sans_ext(file_name)  # Remove the extension
  
  # Read Excel file and store in data frame
  data <- read_excel(file_path)
  
  # Convert the data to a spatial object with WGS84 CRS
  data_sf <- st_as_sf(data, coords = c("coordx", "coordy"), crs = 4326)
  
  # Project the data to a metric CRS with 1cm resolution for WGS84
  proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=cm +no_defs"
  
  # Transform the data coordinates in the new projection you set in proj
  data_proj <- st_transform(data_sf, proj)
  
  # Extract the projected x and y coordinates from the data
  data$x <- st_coordinates(data_proj)[, 1]
  data$y <- st_coordinates(data_proj)[, 2]
  
  # Create the plot
  par(mfrow = c(1,2))
  plot(data$x, data$y, xlab = "Longitude [cm]", ylab = "Latitude [cm]", main = paste("Plot -", basename(file_path), "- Metric Coordinates"))
  plot(data$coordx, data$coordy, xlab = "Longitude [degrees]", 
       ylab = "Latitude [degrees]", main = paste("Plot -", basename(file_path), "- Angular Coordinates"))
  dev.off()
  
  # Compute the distance between points in meters
  data$distance <- distGeo(as.matrix(data[, c("coordx", "coordy")])) # * 1000
  
  # Calculate the minimum and maximum x and y coordinates of the trees
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
  cat("Corner 1 for plot", basename(file_path), ": ", corner1[1], ",", corner1[2], "\n")
  cat("Corner 2 for plot", basename(file_path), ": ", corner2[1], ",", corner2[2], "\n")
  cat("Corner 3 for plot", basename(file_path), ": ", corner3[1], ",", corner3[2], "\n")
  cat("Corner 4 for plot", basename(file_path), ": ", corner4[1], ",", corner4[2], "\n")
  
  # Store data frame in the list
  data_list[[basename(file_path)]] <- data
  
  # WHERE ARE WE?
  x.coord.corner <- as.integer(corner1[1])
  y.coord.corner <- as.integer(corner1[2])
  
  # RU GRID----------------------------------------------------------------------
  # SIZE OF THE AREA THAT I WANT TO COVER
  # 1 x 1   100m -> 1x1
  
  xn <- 1
  yn <- 1
  
  # Define which values will be the IDs (environment) of resource units and stands. 
  # I define here only one resource unit:
  
  RU.values <- rep(110, xn * yn)           # but can be a sequence with this e.g.: seq(1000,length.out = xn*yn, by=1)
  RUindex.values <- c(0:(xn * yn - 1))  # this is RU index, always starts from 0, iLand is generating this during the simulation runs.
  
  # Order them into a matrix:
  RU.grid <- matrix(RU.values, ncol = xn)
  RUindex.grid <- matrix(RUindex.values, ncol = xn)
  
  # PLOT GRID
  # SIZE OF THE AREA THAT I WANT TO COVER
  # 1 x 1   100m -> 1x1
  
  xn_ <- 10
  yn_ <- 10
  
  # For stands I usually put here IDs referring to the Site index (init) for which we will populate the trees. To a unified structure I just put here one SI, SI=26
  Stand.values <- rep(110, xn_ * yn_) # Here I should change this number based on the climate and plot number related
  
  # Order them into a matrix:
  Stand.grid <- matrix(Stand.values, ncol = xn_)
  
  # Where store the RU grid files in my directory
  out.dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/gis/ru_grid/"
  
  # Construct the file name using the plot ID for RU grid
  RU.grid.file <- paste0(out.dataroot, "environment_grid_", basename(file_path), ".asc")
  
  write.table(paste("NCOLS", xn, sep="\t"), file = RU.grid.file, append = FALSE, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)
  
  write.table(paste("NROWS", yn, sep="\t"), file = RU.grid.file, append = T, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)
  
  write.table(paste("XLLCORNER", x.coord.corner, sep="\t"), file = RU.grid.file, append = T, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)
  
  write.table(paste("YLLCORNER" ,y.coord.corner, sep="\t"), file = RU.grid.file, append = T, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)
  
  write.table(paste("CELLSIZE", "100", sep="\t"), file = RU.grid.file, append = T, quote = FALSE, sep = "\t",        
              eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)
  
  write.table(paste("NODATA_value"	,"-9999", sep="\t"), file = RU.grid.file, append = T, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)
  
  write.table(RU.grid, file = RU.grid.file, append = T, quote = FALSE, sep = "\t",
              eol = "\n", na = "-9999", dec = ".", row.names = FALSE, col.names=FALSE)
  
  #-----------------------------------------------------------------------------
  # Where store the stand grid files in my directory
  #-----------------------------------------------------------------------------
  out.dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/gis/plot_grid/"
  
  # Construct the file name using the plot ID for Stand grid
  S.grid.file <- paste0(out.dataroot, basename(file_path), ".asc")
  
  write.table(paste("NCOLS", xn_, sep="\t"), file = S.grid.file, append = FALSE, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)
  
  write.table(paste("NROWS", yn_, sep="\t"), file = S.grid.file, append = T, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)
  
  write.table(paste("XLLCORNER", x.coord.corner, sep="\t"), file = S.grid.file, append = T, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)
  
  write.table(paste("YLLCORNER" ,y.coord.corner, sep="\t"), file = S.grid.file, append = T, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)
  
  write.table(paste("CELLSIZE", "10", sep="\t"), file = S.grid.file, append = T, quote = FALSE, sep = "\t",             
              eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)
  
  write.table(paste("NODATA_value"	,"-9999", sep="\t"), file = S.grid.file, append = T, quote = FALSE, sep = "\t",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=FALSE)
  
  write.table(Stand.grid, file = S.grid.file, append = T, quote = FALSE, sep = "\t",
              eol = "\n", na = "-9999", dec = ".", row.names = FALSE, col.names=FALSE)

}
  
The upper part works not the below.
Needed to interupt because was creating troubles the kernel density with some excels tables.
Somehow I am receving error because is not taking the data correctly and result in 0 values.
  
  
#-------------------------------------------------------------------------------  
# Create the Initialization input data file on tree structures  
#------------------------------------------------------------------------------

# SELECT THE COLUMNS
desired_columns <- dplyr::select(data, x, y, species, treedb, treeht)      # Select the dataset
colnames(desired_columns)<-c("x","y","species","dbh","height")      # CHANGE THE NAME OF THE COLUMNS

  desired_columns <- mutate(desired_columns, species = if_else(species == "abies", "piab", species))
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
  desired_columns <- mutate(desired_columns, species = if_else(species == "pseudoacacia", "rops", species))


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

# Check if trees_above_threshold is empty
if (nrow(trees_above_threshold) > 0) {
  # Perform kernel density estimation for trees within the threshold
  kde_result <- kde(trees_below_threshold[, c("x", "y")])
  
  # Determine number of kernels based on the number of trees outside the threshold
  num_kernels <- nrow(trees_above_threshold)
  
  # Sample new coordinates from the kernel density estimate
  sampled_points <- rkde(num_kernels, kde_result)
  
  # Reduce the numbers at 2 decimals
  sampled_points <- round(sampled_points, 2)
  
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
  
  desired_columns <- bind_rows(trees_above_threshold, trees_below_threshold) %>%
    arrange(tree_id)
  
  desired_columns <- dplyr::select(desired_columns, -tree_id)
  
  # Print the modified data frame
  print(desired_columns)
} else {
  # If trees_above_threshold is empty, proceed with replicating the forest structure directly
  desired_columns2 <- desired_columns
  desired_columns2$x <- desired_columns$x + 50
  desired_columns2$y <- desired_columns$y
  
  desired_columns3 <- desired_columns
  desired_columns3$x <- desired_columns$x
  desired_columns3$y <- desired_columns$y + 50
  
  desired_columns4 <- desired_columns
  desired_columns4$x <- desired_columns$x + 50
  desired_columns4$y <- desired_columns$y + 50
  
  final_data <- rbind(desired_columns, desired_columns2, desired_columns3, desired_columns4)
}


#---------------------INIT INPUT TABLE -----------------------------------------
  #-----------------------------------------------------------------------------
  # Where store the stand grid files in my directory
  #-----------------------------------------------------------------------------
out.dataroot<-"C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/gis/init/init/"    # use the same place

# write.table(desired_columns, file=paste(out.dataroot,"_init.txt",sep=";"), append = FALSE, quote = FALSE, sep = "\t" means sep by space,
#             eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names=TRUE)

write.table(final_data, file = paste(out.dataroot, basename(file_path), ".txt", sep = ""),
            append = FALSE, quote = FALSE, sep = ";", eol = "\n", na = "NA",
            dec = ".", row.names = FALSE, col.names = TRUE)


