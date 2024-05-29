
#                                       Dr. Marco Baldo, MSc
# 
#                                11/10/2023  CZU and Sapienza - University of Rome 


#            This script will read and manipulate the Bottoms Up database of multi-taxonomic samples divided into plots
#            for the study region of the Czech Republic and prepare the input tables required to synthesize the 
#            forest and sites structures into the iLand model of forest dynamics and disturbances simulations

#            This a clean and automatized script to synthesize the whole Bottoms-Up database into input tables readable in iLand model

library(geosphere)
library(sf)
library(ggpubr)
library(ggplot2)
library(rgdal)
library(writexl)
library(dplyr)

# Load the data
load("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/R/stsm_roma/alldata (1).RData") # Bottoms-Up data

# Create a function to process and save a site
process_site <- function(site_data, output_dir) {
  unique_plots <- unique(site_data$plotID)
  
  for (plot in unique_plots) {
    subset_df <- site_data %>% filter(plotID == plot) %>% drop_na(coordx)
    
    if (nrow(subset_df) > 0) {
      data_sf <- st_as_sf(subset_df, coords = c("coordx", "coordy"), crs = 4326)
      proj <- "+proj=utm +zone=33 +ellps=WGS84 +units=cm +no_defs"
      data_proj <- st_transform(data_sf, proj)
      subset_df$x <- st_coordinates(data_proj)[, 1]
      subset_df$y <- st_coordinates(data_proj)[, 2]
      
      file_name <- paste0("plot_", plot)
      write_xlsx(subset_df, file.path(output_dir, paste0(file_name, ".xlsx")))
    }
  }
}

# Create a directory to store the plots
plot_dir <- "C:/iLand/20230901_Bottoms_Up/plot_init/plots/New folder/"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
}

# Process each site
unique_sites <- unique(treedata$siteID)
for (site in unique_sites) {
  site_data <- subset(treedata, siteID == site)
  output_dir <- file.path(plot_dir, site)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  process_site(site_data, output_dir)
}


# You can add the section for creating and writing the RU and Stand grids within the loop after calculating the corner points of the plot. Here's the modified script with the added section
# To name the output files based on the plot ID, you can extract the plot ID from the Excel file's name and use it to generate the output file names. Here's how you can modify the script to achieve this
  
# Load the required libraries
library(readxl)
library(dplyr)
library(raster)
library(fields)

# Define the directory path where your Excel files are located
directory_path <- "C:/iLand/20230901_Bottoms_Up/plot_init/plots/clean_plot/"

# List all Excel files in the directory
file_paths <- list.files(directory_path, pattern = "\\.xlsx$", full.names = TRUE)

# Create a function to process each Excel file
process_excel_file <- function(file_path) {
  # Read the data from the Excel file
  data <- read_excel(file_path)
  
  # Extract the plot ID from the "plotID" column
  plot_id <- unique(data$plotID)
  
  # Calculate the minimum and maximum x and y coordinates of the trees
  min_x <- min(data$x)
  max_x <- max(data$x)
  min_y <- min(data$y)
  max_y <- max(data$y)
  
  # Calculate the coordinates of the four corners of the plot
  corner1 <- c(min_x - 25, min_y - 25)
  corner2 <- c(max_x + 25, min_y - 25)
  corner3 <- c(max_x + 25, max_y + 25)
  corner4 <- c(min_x - 25, max_y + 25)
  
  # Print the coordinates of the four corners of the plot
  cat("Corner 1: ", corner1[1], ",", corner1[2], "\n")
  cat("Corner 2: ", corner2[1], ",", corner2[2], "\n")
  cat("Corner 3: ", corner3[1], ",", corner3[2], "\n")
  cat("Corner 4: ", corner4[1], ",", corner4[2], "\n")
  
  #----------------------------------------------------------------
  # Write out the RU and Stand grids
  out.dataroot <- "C:/iLand/20230901_Bottoms_Up/plot_init/gis/"
  
  # WHERE ARE WE?
  x.coord.corner <- as.integer(corner1[1])
  y.coord.corner <- as.integer(corner1[2])
  
  # RU GRID----------------------------------------------------------------------
  # SIZE OF THE AREA THAT I WANT TO COVER
  # 1 x 1   100m -> 1x1
  xn <- 1
  yn <- 1
  
  RU.values <- rep(110, xn * yn)
  RUindex.values <- c(0:(xn * yn - 1))
  
  RU.grid <- matrix(RU.values, ncol = xn)
  RUindex.grid <- matrix(RUindex.values, ncol = xn)
  
  # PLOT GRID
  xn_ <- 10
  yn_ <- 10
  
  Stand.values <- rep(110, xn_ * yn_)
  Stand.grid <- matrix(Stand.values, ncol = xn_)
  
  set.panel(2, 2)
  par(mar = c(2, 4, 2, 4))
  
  #---------------------------------------ENVIRONMENT-------------------------------------
  # Write out the RU grid - Environment
  RU.grid.file <- paste0(out.dataroot, "RU_grid_", plot_id, ".asc")
  
  # Write the remaining header information for the RU grid
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
  # Write out the Stand grid
  S.grid.file <- paste0(out.dataroot, "Stand_grid_", plot_id, ".asc")
  
  # Write the remaining header information for the Stand grid
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
  # SELECT THE COLUMNS
  
  desired_columns <- dplyr::select(data, x, y, treesp, treedb, treeht)      # Select the dataset
  colnames(desired_columns)<-c("x","y","species","dbh","height")      # CHANGE THE NAME OF THE COLUMNS
  
  # Get the unique species names
  sp <- unique(desired_columns$species)
  
  {desired_columns <- mutate(desired_columns, species = if_else(species == "Picea abies", "piab", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Abies alba", "abal", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Larix decidua", "lade", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Pinus sylvestris", "pisy", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Fagus sylvatica", "fasy", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Quercus robur", "quro", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Acer pseudoplatanus", "acps", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Fraxinus excelsior", "frex", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Carpinus betulus", "cabe", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Betula pendula", "bepe", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Alnus incana", "alin", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Quercus petraea", "qupe", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Alnus glutinosa", "algl", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Castanea sativa", "casa", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Pinus nigra", "pini", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Acer campestre", "acca", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Acer platanoides", "acpl", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Quercus pubescence", "qupu", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Pinus cembra", "pice", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Sorbus aucuparia", "soau", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Sorbus aria", "soar", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Corylus avellana", "coav", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Alnus viridis", "alvi", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Populus tremula", "potr", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Populus nigra", "poni", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Tilia cordata", "tico", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Tilia platyphyllos", "tipl", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Ulmus glabra", "ulgl", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Salix caprea", "saca", species))
    desired_columns <- mutate(desired_columns, species = if_else(species == "Robinia pseudoacacia", "rops", species))}
  
  
  # Convert from meters to cm the tree height columns
  # desired_columns$treeheight <- desired_columns$treeheight * 100 # PICUS
  
  # Correct for not having the integers
  desired_columns$x <- as.integer(desired_columns$x)
  desired_columns$y <- as.integer(desired_columns$y)    
  desired_columns$height <- as.numeric(desired_columns$height)    
  
  #---------------------------------------------------------------------
  # Shift the numbers from metrics coordinates in iland quadrant coordinates
  
  # Create the element for shift the coordinates to 0 - 50 meters
  
  #--------------------------------------------------------------------
  
  # 2nd way Let's apply the corner bottom left coordinates of the plot (also more robust as method)
  {min_x <- x.coord.corner
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
  
  # Set threshold
  threshold <- 50
  
  # Filter out rows where x or y exceeding the threshold
  desired_columns <- desired_columns %>%
    filter(x <= threshold & y <= threshold)}
  
  #--------------------------------------------------------------------------------
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
  
  #-------------------------------------------------------------------
  # write 
  out.dataroot<-"C:/iLand/20230901_Bottoms_Up/plot_init/gis/init/"    # use the same place
  
  # Write out the plot input tables
  Plot.grid.file <- paste0(out.dataroot, "Plot_init_", plot_id, ".txt")
  
  write.table(desired_columns, file = paste(Plot.grid.file, sep = ""),
              append = FALSE, quote = FALSE, sep = ";", eol = "\n", na = "NA",
              dec = ".", row.names = FALSE, col.names = TRUE)
  
}

# Use lapply to process all Excel files
processed_data_list <- lapply(file_paths, process_excel_file)

# If you need to access specific processed data, you can do so like this:
# processed_data <- processed_data_list[[index]]  # Replace 'index' with the desired file's index

# Now you have a list of processed data, one for each Excel file in the directory

#-------------------------------------------------------------------------------
#                                       END
#
#-------------------------------------------------------------------------------
