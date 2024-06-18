# Install libraries for national borders
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ggspatial")

# Load libraries
library(readr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(ggspatial)

#-------------------------------------------------------------------------------
# IMPORT THE TABLE WITH THE COORDINATES AND OTHE PLOT INFORMATIONS AND CREATE GEOREFERNTIAL OBJECTS

# Directory
dataroot <- "C:/Users/baldo/Documents/GitHub/Bottoms-Up/STSM/Maps/"

# Tables where I find my coordinates and plots id
tab1 <- read_csv(file.path(dataroot, "CLIM_DATA_REQUEST.csv"))

# df structure
str(tab1)
head(tab1)

# rename columns
colnames(tab1) <- c("colnum", "keyID", "slope", "aspect", "altitude", "longitude", "latitude")

# Extract site codes
tab1$site <- sub("^(CZ_JH[1-6]_L[1-6]).*", "\\1", tab1$keyID)

# Convert to coordiante objects
bdv_plot_sf <- st_as_sf(tab1, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

#-------------------------------------------------------------------------------
# CREATE THE WORLD MAP AND FILTER FOR CZECH REP
# Get the world map with country boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter for Czech Republic
czech_republic <- world[world$name == "Czechia", ]
europe <- world[world$continent == "Europe", ]

# Calculate centroids of each country for labeling
europe_centroids <- st_centroid(europe)

# Plot CZ map with color per site
ggplot() +
  geom_sf(data = czech_republic, fill = "gray90", color = "black") +  # Plot the Czech Republic borders with gray fill
  geom_sf(data = bdv_plot_sf, aes(color = site)) +  # Plot the BDV plots
  theme_minimal() +
  labs(title = "BDV Plots in Czech Republic", x = "Longitude", y = "Latitude", color = "Site")

# Europe with country codes
ggplot() +
  geom_sf(data = europe, fill = "gray90", color = "black") +  # Plot Europe borders with gray fill
  geom_sf(data = bdv_plot_sf, aes(color = site)) +  # Plot the BDV plots
  geom_sf_text(data = europe_centroids, aes(label = iso_a3), size = 3, color = "black", check_overlap = TRUE) +  # Add country codes
  coord_sf(xlim = c(5, 25), ylim = c(45, 55), expand = FALSE) +  # Adjust limits to focus on central Europe
  theme_minimal() +
  labs(title = "BDV Plots in Europe", x = "Longitude", y = "Latitude", color = "Site")

#-------------------------------------------------------------------------------
# Add the Czech Rep. DEM at the figure!

# Directory for DEM data (example path, replace with actual path to DEM file)
dem_file <- "C:/Users/baldo/Documents/GitHub/Bottoms-Up/STSM/Maps/Czech_DEM/DEM_czech.tif"

# Load DEM data
dem <- raster(dem_file)

# plot the DEM
plot(dem)

#-------------------------------------------------------------------------------
# Downsample DEM data to reduce resolution (factor of 10 as an example)
dem_downsampled <- aggregate(dem, fact = 2, fun = mean)

# Convert Czech Republic boundary to raster's CRS
czech_republic_raster <- st_transform(czech_republic, crs = crs(dem_downsampled))

# Create a mask of the DEM with the Czech Republic boundaries
dem_clipped <- mask(dem_downsampled, czech_republic)

# Convert clipped DEM to a data frame suitable for ggplot2
dem_df <- as.data.frame(rasterToPoints(dem_clipped), xy = TRUE)

# Convert DEM to a format suitable for ggplot2
# dem_df <- as.data.frame(rasterToPoints(dem_downsampled), xy = TRUE)

# Plot using ggplot2 with a DEM layer
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +  # Overlay DEM layer
  scale_fill_gradient(low = "gray90", high = "gray10", na.value = NA) +  # Grayscale
  #geom_sf(data = czech_republic, fill = NA, color = "black") +  # Plot the Czech Republic borders
  geom_sf(data = bdv_plot_sf, aes(color = site)) +  # Plot the BDV plots
  theme_minimal() +
  labs(title = "BDV Plots in Czech Republic", x = "Longitude", y = "Latitude", color = "Site") +
  theme(legend.position = "bottom")


# Other plot with different legend position and dem map color ramp palette for the elevation gradient
# Plot using ggplot2
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +  # Overlay DEM layer
  scale_fill_viridis_c(na.value = NA) +  # Default color scale for raster images
  geom_sf(data = czech_republic, fill = NA, color = "black") +  # Plot the Czech Republic borders
  geom_sf(data = bdv_plot_sf, aes(color = site), size = 2) +  # Plot the BDV plots
  theme_minimal() +
  labs(title = "BDV Plots on DEM in Czech Republic", x = "Longitude", y = "Latitude", color = "Site", fill = "Elevation") +
  theme(legend.position = "right")

# Define specific colors for each site
site_colors <- c("CZ_JH1_L1" = "red", 
                 "CZ_JH2_L6" = "blue", 
                 "CZ_JH3_L5" = "green", 
                 "CZ_JH4_L4" = "purple", 
                 "CZ_JH5_L2" = "orange", 
                 "CZ_JH6_L3" = "pink")


ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +  # Overlay DEM layer
  scale_fill_gradientn(colors = c("#FFEDA0", "#FEB24C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026"), na.value = NA) +  # Custom color scale matching the uploaded DEM colors
  geom_sf(data = czech_republic, fill = NA, color = "black") +  # Plot the Czech Republic borders
  geom_sf(data = bdv_plot_sf, aes(color = site), size = 2) +  # Plot the BDV plots
  scale_color_manual(values = site_colors) +  # Specify colors for sites
  theme_minimal() +
  labs(title = "BDV Plots on DEM in Czech Republic", x = "Longitude", y = "Latitude", color = "Site", fill = "Elevation") +
  theme(legend.position = "right")

# Other version probably the most meaningfull
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +  # Overlay DEM layer with default colors
  geom_sf(data = bdv_plot_sf, aes(color = site), size = 2) +  # Plot the BDV plots without specifying colors
  theme_minimal() +
  labs(title = "BDV Plots on DEM in Czech Republic", x = "Longitude", y = "Latitude", color = "Site", fill = "Elevation") +
  theme(legend.position = "right")

# Define the color ramp palette
cl <- colorRampPalette(c("darkorchid3", "light blue", "lightpink4"))(100)

cl <- colorRampPalette(c("lightpink", "#FFEDA0", "darkgreen"))(100)

col <- rev(terrain.colors(255))

#scale_fill_distiller(palette = "plasma")

# Plotting code with labels and custom color ramp
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +  # Overlay DEM layer with custom color scale
  scale_fill_gradientn(colors = col, na.value = NA) +  # Use custom color ramp for elevation
  #geom_sf(data = czech_republic, fill = NA, color = "black") +  # Plot the Czech Republic borders
  geom_text(data = bdv_plot_sf, aes(label = site, x = st_coordinates(geometry)[, "X"], y = st_coordinates(geometry)[, "Y"]), size = 3, nudge_y = 0.002) +  # Add labels for sites
  geom_sf(data = bdv_plot_sf, aes(color = site), size = 2) +  # Plot the BDV plots without specifying colors
  theme_minimal() +
  labs(title = "BDV Plots on DEM in Czech Republic", x = "Longitude", y = "Latitude", color = "Site", fill = "Elevation") +
  theme(legend.position = "right")

#
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +  # Overlay DEM layer
  scale_fill_viridis_d(na.value = NA) +  # Default color scale for raster images
  geom_sf(data = czech_republic, fill = NA, color = "black") +  # Plot the Czech Republic borders
  geom_sf(data = bdv_plot_sf, aes(color = site), size = 2) +  # Plot the BDV plots
  #geom_text(data = bdv_plot_sf, aes(label = site, x = st_coordinates(geometry)[, "X"], y = st_coordinates(geometry)[, "Y"]), size = 3, nudge_y = 0.002) +  # Add labels for sites
  theme_minimal() +
  labs(title = "BDV Plots on DEM in Czech Republic", x = "Longitude", y = "Latitude", color = "Site", fill = "Elevation") +
  theme(legend.position = "right")

