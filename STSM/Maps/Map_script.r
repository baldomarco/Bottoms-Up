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
