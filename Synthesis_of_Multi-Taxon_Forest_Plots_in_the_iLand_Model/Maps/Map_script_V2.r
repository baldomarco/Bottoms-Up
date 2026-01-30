# Load libraries
library(readr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(ggspatial)
library(dplyr)
library(ggspatial)
library(viridis)

#-------------------------------------------------------------------------------
# IMPORT THE TABLE WITH THE COORDINATES AND OTHE PLOT INFORMATIONS AND CREATE GEOREFERNTIAL OBJECTS

# Directory
dataroot <- "C:/Users/baldo/Documents/GitHub/Bottoms-Up/Synthesis_of_Multi-Taxon_Forest_Plots_in_the_iLand_Model/Maps/"

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

# Create a unique value
bdv_plot_sf_unique <- bdv_plot_sf %>%
  group_by(site) %>%
  slice(1) %>%
  ungroup()

# Plot CZ map with color per site
ggplot() +
  geom_sf(data = czech_republic, fill = "gray90", color = "black") +  # Plot the Czech Republic borders with gray fill
  geom_sf(data = bdv_plot_sf_unique, aes(shape = site, size =2, stroke = 1.5)) +  # Plot the BDV plots
  scale_shape_manual(values = 1:length(unique(bdv_plot_sf_unique$site))) +
  theme_minimal() +
  labs(title = "BDV Plots in Czech Republic", x = "Longitude", y = "Latitude", shape = "Site")+
  guides(size = "none")

# Europe with country codes
ggplot() +
  geom_sf(data = europe, fill = "gray90", color = "black") +  # Plot Europe borders with gray fill
  geom_sf(data = bdv_plot_sf_unique, aes(shape = site, size = 2, stroke = 1)) +  # Plot the BDV plots
  geom_sf_text(data = europe_centroids, aes(label = iso_a2), size = 3, color = "black", check_overlap = TRUE) +  # Add country codes
  scale_shape_manual(values = 1:length(unique(bdv_plot_sf_unique$site))) +
  coord_sf(xlim = c(5, 25), ylim = c(45, 55), expand = FALSE) +  # Adjust limits to focus on central Europe
  theme_minimal() +
  labs(title = "BDV Plots in Europe", x = "Longitude", y = "Latitude", shape = "Site")+
  guides(size = "none")

#-------------------------------------------------------------------------------
# Add the Czech Rep. DEM at the figure!

# Directory for DEM data (example path, replace with actual path to DEM file)
dem_file <- "C:/Users/baldo/Documents/GitHub/Bottoms-Up/Synthesis_of_Multi-Taxon_Forest_Plots_in_the_iLand_Model/Maps/Czech_DEM/DEM_czech.tif"

# Load DEM data
dem <- raster(dem_file)

# plot the DEM
plot(dem)

# In this DEM the background rectangular is assigned as 0. Let's make it as NA
dem[dem <= 0] <- NA

# Now can be plotted without the background
plot(dem, colNA = "white", main = "DEM Czech Republic - Cleaned")

# Now let's invert the color gradient
cols_inv <- terrain.colors(255)

plot(dem, col = cols_inv, colNA = "white")

# Now let's make it with the magma color palette
plot(dem, col = rev(magma(255)), colNA = "white")

#-------------------------------------------------------------------------------
# Convert clipped DEM to a data frame suitable for ggplot2
dem_df <- as.data.frame(rasterToPoints(dem), xy = TRUE)
names(dem_df)[3] <- "elevation"

# Color Palette
col <- terrain.colors(255)

# BEST - Plotting code with labels and custom color ramp
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +  # Overlay DEM layer with custom color scale
  scale_fill_gradientn(colors = col, na.value =  "white") +  # Use custom color ramp for elevation
  #geom_sf(data = czech_republic, fill = NA, color = "black") +  # Plot the Czech Republic borders
  #geom_text(data = bdv_plot_sf, aes(label = site, x = st_coordinates(geometry)[, "X"], y = st_coordinates(geometry)[, "Y"]), size = 3, nudge_y = 0.002) +  # Add labels for sites
  geom_sf(data = bdv_plot_sf_unique, aes(shape = site), size = 3, stroke = 1.5) +
  scale_shape_manual(values = 1:length(unique(bdv_plot_sf_unique$site))) +
  theme_minimal() +
  labs(title = "BDV Plots on DEM in Czech Republic", x = "Longitude", y = "Latitude", shape = "Site", fill = "Elevation") +
  theme(legend.position = "right")


#-------------------------------------------------------------------------------
# DOWNSCALE THE DEM RESOLUTION FOR A MATTER OF SIZE WHEN PLOTTING
#-------------------------------------------------------------------------------
# Downsample DEM data to reduce resolution (factor of 10 as an example)
dem_downsampled <- aggregate(dem, fact = 2, fun = mean)

# Convert Czech Republic boundary to raster's CRS
czech_republic_raster <- st_transform(czech_republic, crs = crs(dem_downsampled))

# Create a mask of the DEM with the Czech Republic boundaries
dem_clipped <- mask(dem_downsampled, czech_republic_raster)

# Convert clipped DEM to a data frame suitable for ggplot2
dem_df <- as.data.frame(rasterToPoints(dem_clipped), xy = TRUE)

names(dem_df)[3] <- "elevation"


# Color Palette
col <- terrain.colors(255)

# col <- rev(terrain.colors(255))

# BEST - Plotting code with labels and custom color ramp
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +  
  scale_fill_gradientn(colors = col, na.value = "white") +
  geom_sf(data = bdv_plot_sf_unique, aes(shape = site), size = 3, stroke = 1.5) + 
  scale_shape_manual(values = 1:length(unique(bdv_plot_sf_unique$site))) +
  labs(title = "BDV Plots on DEM in Czech Republic", x = "Longitude", y = "Latitude", shape = "Site", fill = "Elevation") +
  theme_minimal() +
  theme(legend.position = "right")+
  annotation_scale(location = "bl", width_hint = 0.2) +  
  annotation_north_arrow(location = "br", which_north = "true", 
                         style = north_arrow_fancy_orienteering())


# Other plot with different legend position and dem map color ramp palette for the elevation gradient
# Plot using ggplot2 - viridis color ramp for the dem
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +  # Overlay DEM layer
  scale_fill_viridis_c(na.value =  "white") +  # Default color scale for raster images
  #geom_sf(data = czech_republic, fill = NA, color = "black") +  # Plot the DEM borders
  geom_sf(data = bdv_plot_sf_unique, aes(shape = site), size = 3, stroke = 1.5) +  # Plot the BDV plots
  scale_shape_manual(values = 1:length(unique(bdv_plot_sf_unique$site))) +
  theme_minimal() +
  labs(title = "BDV Plots on DEM in Czech Republic", x = "Longitude", y = "Latitude", shape = "Site", fill = "Elevation") +
  theme(legend.position = "right")


#-------------------------------------------------------------------------------
# PLOT FOR HAVING THE SHAPE FOR THE UNIQUE SITE

# Create a unique value
bdv_plot_sf_unique <- bdv_plot_sf %>%
  group_by(site) %>%
  slice(1) %>%
  ungroup()

# Now plot with different shapes for each site and white for NAs
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +  # Overlay DEM layer
  scale_fill_gradientn(colors = col, na.value = "white") +  # Custom color ramp with NA as white
  geom_sf(data = bdv_plot_sf_unique, aes(color = site, shape = site), size = 3, stroke = 1.5) +  # Add shapes for sites
  scale_shape_manual(values = 1:length(unique(bdv_plot_sf_unique$site))) +  # Assign a different shape per site
  theme_minimal() +
  labs(title = "BDV Plots on DEM in Czech Republic", x = "Longitude", y = "Latitude", color = "Site", fill = "Elevation") +
  theme(legend.position = "right")

#-------------------------------------------------------------------------------
# PLOT WITH MAGMA BACKGROUND AND SINGLE PLOT DOTS LIGHT BLUE

# Full color per dots-plots
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradientn(colors = rev(magma(255)), na.value = "white") +
  geom_sf(data = bdv_plot_sf, color = "#00FFFF", shape = 21, fill  = "#00FFFF", stroke = 0.4, size = 2)+
  theme_minimal() +
  labs(title = "BDV plots on DEM (Czech Republic)", x = "Longitude", y = "Latitude", fill = "Elevation")

# Empty circle per site
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradientn(colors = rev(magma(255)), na.value = "white") +
  geom_sf(data = bdv_plot_sf_unique, shape  = 21, fill   = NA, color  = "#00FFFF", size   = 3, stroke = 1.5) +
  theme_minimal() +
  labs(title = "BDV sites on DEM (Czech Republic)", x = "Longitude", y = "Latitude", fill = "Elevation")
