#------------------------------------------------------------------------------- site
# Crete the species proportion per site

library(dplyr)

tree_data <- data %>%
    mutate(Site = sub("_[0-9]+", "", plotID),
         Plot = sub("([A-Z]+)[0-9]+_[0-9]+", "\\1\\2", plotID))

head(tree_data)


# Count the number of trees for each species within each site
site_species_counts <- tree_data %>%
  group_by(Site, treesp) %>%
  summarise(TreeCount = n())

# Calculate species proportions within each site
site_species_proportions_n <- site_species_counts %>%
  group_by(Site) %>%
  mutate(SpeciesProportion = TreeCount / sum(TreeCount))

print(site_species_proportions_n)
show(site_species_proportions_n)

# Filter the data for the specific site 'L1'
site_L1_proportions_n <- subset(site_species_proportions_n, Site == 'L1')

# Extract the species proportions for 'L1'
species_proportions_L1_n <- site_L1_proportions_n$SpeciesProportion

# Print the array or string of species proportions
cat("Species Proportions in Site L1:", species_proportions_L1_n, "\n")


#---------------------------------------------------------------------------------- plot
# Do the same but for plot instead that for sites

library(dplyr)

# Define tree_data with Site and Plot
tree_data <- data %>%
  mutate(
    Site = sub("_[0-9]+", "", plotID),  # Extract the site part (e.g., L1, L2)
    Plot = plotID  # Retain the full plotID as Plot
  )

head(tree_data)

# Count the number of trees for each species within each plot
plot_species_counts <- tree_data %>%
  group_by(Site, Plot, treesp) %>%
  summarise(TreeCount = n(), .groups = 'drop')

# Calculate species proportions within each plot
plot_species_proportions_n <- plot_species_counts %>%
  group_by(Site, Plot) %>%
  mutate(SpeciesProportion = TreeCount / sum(TreeCount))

print(plot_species_proportions_n)
show(plot_species_proportions_n)

# Filter the data for the specific site 'L1'
plot_L1_proportions_n <- subset(plot_species_proportions_n, Plot == 'L1_03')

# Extract the species proportions for 'L1'
species_proportions_L1_n <- plot_L1_proportions_n$SpeciesProportion

# Print the array or string of species proportions
cat("Species Proportions in Site L1:", species_proportions_L1_n, "\n")


#------------------------------------------------------------------------------- site
# Proportion per site in terms of volume
library(dplyr)

# Data transformation to create Site and Plot columns
tree_data <- data %>%
  mutate(Site = sub("_[0-9]+", "", plotID),
         Plot = plotID)  # Retain the full plotID as Plot

head(tree_data)

# Ensure treevol is numeric and replace NA with 0
tree_data <- tree_data %>%
  mutate(treevol = as.numeric(treevol),
         treevol = ifelse(is.na(treevol), 0, treevol))

# Summarize the total tree volume for each species within each site
site_species_volumes <- tree_data %>%
  group_by(Site, treesp) %>%
  summarise(TotalVolume = sum(treevol, na.rm = TRUE))

# Calculate species proportions based on total volume within each site
site_species_proportions_vol <- site_species_volumes %>%
  group_by(Site) %>%
  mutate(SpeciesProportion = TotalVolume / sum(TotalVolume))

print(site_species_proportions_vol)
show(site_species_proportions_vol)

# Filter the data for the specific site 'L1'
site_L1_proportions_vol <- subset(site_species_proportions_vol, Site == 'L1')

# Extract the species proportions for 'L1'
species_proportions_L1_vol <- site_L1_proportions_vol$SpeciesProportion

# Print the array or string of species proportions
cat("Species Proportions in Site L1:", species_proportions_L1_vol, "\n")
 
#------------------------------------------------------------------------------- plot
# Do the same but for plot instead that for plot relative at the wood volume
library(dplyr)

# Define tree_data with Site and Plot
tree_data <- data %>%
  mutate(
    Site = sub("_[0-9]+", "", plotID),  # Extract the site part (e.g., L1, L2)
    Plot = plotID  # Retain the full plotID as Plot
  )

head(tree_data)

# Ensure treevol is numeric and replace NA with 0
tree_data <- tree_data %>%
  mutate(treevol = as.numeric(treevol),
         treevol = ifelse(is.na(treevol), 0, treevol))

# Summarize the total tree volume for each species within each plot
plot_species_volumes <- tree_data %>%
  group_by(Plot, treesp) %>%
  summarise(TotalVolume = sum(treevol, na.rm = TRUE))

# Calculate species proportions based on total volume within each plot
plot_species_proportions_vol <- plot_species_volumes %>%
  group_by(Plot) %>%
  mutate(SpeciesProportion = TotalVolume / sum(TotalVolume))

print(plot_species_proportions_vol)
show(plot_species_proportions_vol)

# Filter the data for the specific plot 'L1_33'
plot_L1_33_proportions_vol <- subset(plot_species_proportions_vol, Plot == 'L2_16')
plot_L1_33_proportions_vol

# Extract the species proportions for 'L1_33'
species_proportions_L1_33_vol <- plot_L1_33_proportions_vol$SpeciesProportion

# Print the array or string of species proportions
cat("Species Proportions in Plot L1_33:", species_proportions_L1_33_vol, "\n")


#-------------------------------------------------------------------------------
# Create site and plot species proportion in a excel table

library(writexl)

dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/species_proportion/"

# sp prop per site based on n. of trees
write_xlsx(site_species_proportions_n, file.path(dataroot, "sp_prop_site_n.xlsx"))

# sp prop per plot based on n. of trees
write_xlsx(plot_species_proportions_n, file.path(dataroot, "sp_prop_plot_n.xlsx"))

# sp prop per site based on volume per species
write_xlsx(site_species_proportions_vol, file.path(dataroot, "sp_prop_site_vol.xlsx"))

# sp prop per plot based on rel vol per species
write_xlsx(plot_species_proportions_vol, file.path(dataroot, "sp_prop_plot_vol.xlsx"))


