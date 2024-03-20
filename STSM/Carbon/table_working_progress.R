# Marco Baldo - Deadwood conversion table - 30 -10 -2023


library(ggplot2)
library(dplyr)
library(readxl)
library(writexl)

#-------------------------------------------------------------------------------
# Edit the table in the way to have consistances betweet alive and dead trees excel sheets
# Load data from "alive_dead_trees" and "dead_trees" sheets
live_dead_trees_data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/plot_init/Jenik/final_table_imp/Raw_data_structure_CZ_JH1_final_corr.xlsx", sheet = "live_dead_trees")
standing_lying_deadwood_data <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/plot_init/Jenik/final_table_imp/Raw_data_structure_CZ_JH1_final_corr.xlsx", sheet = "standing_lying_deadwood")
decstag <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/plot_init/Jenik/final_table_imp/DeadwoodDensityGlobal_Katka_corr.xlsx")

writexl::write_xlsx(decstag, "C:/iLand/2023/20230901_Bottoms_Up/plot_init/Jenik/final_table_imp/DeadwoodDensityGlobal_Katka_corr.xlsx")

# Make a clean table for the analysis of the deadwood pools

clean_deadwood_iland <- standing_lying_deadwood_data %>%
  select(keyID, siteID, standID, plotID, treesp, typldw, volume, decsta)

# Deadwood decay stage basic wood density conversion from 5 to 4 classes in Eu tree sp (loop). This conversion is based on the adjusted values for a generalization in Europe of https://doi.org/10.1016/j.foreco.2023.121431. Tree species not into the table coming from the laboratory analysis on the deadwood deacy stages of every analyzed species are categorized as deciduous or coniferous unless the Genus was shared as in Alnus, Quercus, Pinus, Abies and Fraxinus see line 215 tree species conversion 
{
# Create a list of original values for each species
species_decstag <- list(
  Abies = c(343, 305, 247, 174, 149),
  Alnus = c(422, 359, 286, 197, 120),
  deciduous = c(523, 442, 345, 241, 152),
  Carpinus = c(428, 392, 336, 211, 140),
  conifer = c(374, 334, 271, 198, 160),
  Fagus = c(520, 379, 261, 229, 220),
  Fraxinus = c(452, 403, 392, 227, 151),
  Picea = c(381, 340, 270, 190, 157),
  Pinus = c(379, 334, 277, 214, 165),
  Quercus = c(614, 518, 397, 300, 195)
)

# Create an empty list to store the results for each species
results_list <- list()

# Loop through each species and calculate quartiles
for (species in names(species_decstag)) {
  original_values <- species_decstag[[species]]
  
  # Determine quartiles
  quartiles <- quantile(original_values, probs = c(0, 0.333333333333, 0.66666666666, 1))
  
  # Store the results in the list
  results_list[[species]] <- quartiles
}

# Print the quartiles for each species
for (species in names(species_decstag)) {
  cat("Quartiles for", species, ":", results_list[[species]], "\n")
}

}
# More mechanical way to create the conversion
{
# Conver decay classes for Abies
  original_values <- c(48.90, 49.43, 49.91, 50.42, 50.37)

# Determine quantiles and copy in the new csv table in hybrid mode (manually): 
# This step calculates quantiles of the original values, which divides the values into segments.
quartiles <- quantile(original_values, probs = c(0, 0.333333333333, 0.66666666666, 1))
quartiles

# Conver decay classes for Abies
original_values <- c(343, 305, 247, 174, 149)

# Determine quantiles and copy in the new csv table in hybrid mode (manually): 
# This step calculates quantiles of the original values, which divides the values into segments.
quartiles <- quantile(original_values, probs = c(0, 0.333333333333, 0.66666666666, 1))
quartiles

# Conver decay classes for Alnus
original_values <- c(422, 359, 286, 197, 120)

# Determine quantiles and copy in the new csv table in hybrid mode (manually): 
# This step calculates quantiles of the original values, which divides the values into segments.
quartiles <- quantile(original_values, probs = c(0, 0.333333333333, 0.66666666666, 1))
quartiles

# Conver decay classes for deciduous
original_values <- c(523, 442, 345, 241, 152)

# Determine quantiles and copy in the new csv table in hybrid mode (manually): 
# This step calculates quantiles of the original values, which divides the values into segments.
quartiles <- quantile(original_values, probs = c(0, 0.333333333333, 0.66666666666, 1))
quartiles

# Conver decay classes for Carpinus
original_values <- c(428, 392, 336, 211, 140)

# Determine quantiles and copy in the new csv table in hybrid mode (manually): 
# This step calculates quantiles of the original values, which divides the values into segments.
quartiles <- quantile(original_values, probs = c(0, 0.333333333333, 0.66666666666, 1))
quartiles

# Conver decay classes for conifer
original_values <- c(374, 334, 271, 198, 160)

# Determine quantiles and copy in the new csv table in hybrid mode (manually): 
# This step calculates quantiles of the original values, which divides the values into segments.
quartiles <- quantile(original_values, probs = c(0, 0.333333333333, 0.66666666666, 1))
quartiles

# Conver decay classes for Fagus
original_values <- c(520, 379, 261, 229, 220)

# Determine quantiles and copy in the new csv table in hybrid mode (manually): 
# This step calculates quantiles of the original values, which divides the values into segments.
quartiles <- quantile(original_values, probs = c(0, 0.333333333333, 0.66666666666, 1))
quartiles

# Conver decay classes for Fraxinus
original_values <- c(452, 403, 392, 227, 151)

# Determine quantiles and copy in the new csv table in hybrid mode (manually): 
# This step calculates quantiles of the original values, which divides the values into segments.
quartiles <- quantile(original_values, probs = c(0, 0.333333333333, 0.66666666666, 1))
quartiles

# Conver decay classes for Fraxinus
original_values <- c(452, 403, 392, 227, 151)

# Determine quantiles and copy in the new csv table in hybrid mode (manually): 
# This step calculates quantiles of the original values, which divides the values into segments.
quartiles <- quantile(original_values, probs = c(0, 0.333333333333, 0.66666666666, 1))
quartiles

# Conver decay classes for Picea
original_values <- c(381, 340, 270, 190, 157)

# Determine quantiles and copy in the new csv table in hybrid mode (manually): 
# This step calculates quantiles of the original values, which divides the values into segments.
quartiles <- quantile(original_values, probs = c(0, 0.333333333333, 0.66666666666, 1))
quartiles

# Conver decay classes for Pinus
original_values <- c(379, 334, 277, 214, 165)

# Determine quantiles and copy in the new csv table in hybrid mode (manually): 
# This step calculates quantiles of the original values, which divides the values into segments.
quartiles <- quantile(original_values, probs = c(0, 0.333333333333, 0.66666666666, 1))
quartiles

# Conver decay classes for Quercus
original_values <- c(614, 518, 397, 300, 195)

# Determine quantiles and copy in the new csv table in hybrid mode (manually): 
# This step calculates quantiles of the original values, which divides the values into segments.
quartiles <- quantile(original_values, probs = c(0, 0.333333333333, 0.66666666666, 1))
quartiles
}

--------------------------------------------------------------------------------
# Theory behind the statistical function
# {
# The `quantile` function calculates quantiles based on a specified probability distribution. The formula for calculating the quantiles is based on the definition of quantiles and the probability distribution. 
#   
#   The formula for the quantile at probability `p` (where `p` is a value between 0 and 1) is defined as:
#     
#     Q(p) = (1 - g) * x[j] + g * x[j+1]
#   
#   Where:
#     - `Q(p)` is the quantile at probability `p`.
#   - `x[j]` is the `j`-th order statistic, which is the data point just below the `p`-th percentile.
#   - `x[j+1]` is the `j+1`-th order statistic, which is the data point just above the `p`-th percentile.
#   - `g` is the fractional part of the index `j` (i.e., the decimal portion).
#   
#   The `quantile` function in R uses linear interpolation to calculate quantiles. It estimates the quantiles by finding the order statistics `x[j]` and `x[j+1]` based on the specified `p`, and then performs the linear interpolation using the formula above to estimate the quantile value.
#   
#   In simpler terms, the `quantile` function finds the two data points that bound the specified quantile, calculates a weighted average of those points, and returns the estimated quantile value. This allows you to divide your data into segments based on the specified probabilities (e.g., quartiles at p = 0.25, 0.5, 0.75).
# }
--------------------------------------------------------------------------------

# Run it always otherwise you will lose the consistency between clean_deadwood_iland and decstag
# Change the tree species names for the tables merging 

{clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Picea abies", "Picea", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Abies alba", "Abies", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Larix decidua", "coniferous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Pinus sylvestris", "Pinus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Fagus sylvatica", "Fagus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Quercus robur", "Quercus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Acer pseudoplatanus", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Fraxinus excelsior", "Fraxinus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Carpinus betulus", "Carpinus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Betula pendula", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Alnus incana", "Alnus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Quercus petraea", "Quercus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Alnus glutinosa", "Alnus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Castanea sativa", "Quercus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Pinus nigra", "Pinus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Acer campestre", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Acer platanoides", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Quercus pubescence", "Quercus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Pinus cembra", "Pinus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Sorbus aucuparia", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Sorbus aria", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Corylus avellana", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Alnus viridis", "Alnus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Populus tremula", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Populus nigra", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Populus alba", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Salix alba", "Alnus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Pseudotsuga menziesii", "Abies", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Tilia cordata", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Tilia platyphyllos", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Ulmus glabra", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Salix caprea", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Robinia pseudoacacia", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Pinus strobus", "Pinus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Sambucus nigra", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Sambucus racemosa", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Sorbus torminalis", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Ulmus minor", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Quercus pubescens", "Quercus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Quercus rubra", "Quercus", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Prunus spinosa", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Cornus sanguinea", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Sambucus nigra", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Crataegus", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Rhamnus cathartica", "deciduous", treesp))
  clean_deadwood_iland <- mutate(clean_deadwood_iland, treesp = if_else(treesp == "Pyrus communis", "deciduous", treesp))}

# Create consistency on the tables column names
clean_deadwood_iland <- clean_deadwood_iland %>% 
  rename(Decay_class = decsta,
         GenusGlobal = treesp)

# Create the C density varibale/column
decstag <- decstag %>% 
  mutate(C_density_kgm3 = Density_kgm3 * C_fraction/100)

# Eliminate the rows in which I have Avarage in the decay classes
decstag <- decstag %>%
  filter(Decay_class != "Average")

# Find the most common value of "GenusGlobal" per "keyID"
most_common_genus <- clean_deadwood_iland %>%
  group_by(keyID) %>%
  filter(GenusGlobal != "undetermined") %>%  # Remove "undetermined" values
  count(GenusGlobal) %>%
  filter(n == max(n)) %>%
  pull(GenusGlobal)

                                                                           {# Alternative way with weighted mean
  # weighted mean
  {# Look at the species proportion per plot to assign a classification to those piece of wood not classified. Here I decided to assign it at the plot dominant species
    species_proportion <- clean_deadwood_iland %>%
      group_by(plotID, treesp) %>%
      summarise(volume = n()) %>%
      group_by(plotID) %>%
      mutate(
        proportion = volume / sum(volume),
        treesp = if (any(treesp != "undetermined" & treesp != names(which.max(volume)))) {
          ifelse(treesp == "undetermined", names(which.max(volume)), treesp)
        } else {
          treesp
        }
      )
    
    unique(standing_lying_deadwood_data$typldw)}
  }  

# Replace "undetermined" with the most common value per "keyID"
clean_deadwood_iland <- clean_deadwood_iland %>%
  group_by(keyID) %>%
  mutate(GenusGlobal = ifelse(GenusGlobal == "undetermined", most_common_genus, GenusGlobal))

# View the resulting dataframe
print(clean_deadwood_iland)

# Convert the "Decay_class" column to character in both dataframes
clean_deadwood_iland$Decay_class <- as.character(clean_deadwood_iland$Decay_class)
decstag$Decay_class <- as.character(decstag$Decay_class)

# Merge the two data frames while adding missing variables
merged_data <- clean_deadwood_iland %>%
  left_join(decstag, by = c("GenusGlobal", "Decay_class"))

# Calculate the new column by multiplying deadwood and carbon_density
merged_data <- merged_data %>%
  mutate(C_kg = volume * C_density_kgm3)

# View the resulting dataframe
print(merged_data)

# Create a new data frame for counting the numbers of standing dead trees and snags per plot
swdC <- c("stump or snag",  "standing dead wood")
yrC <- c("branch" , "logging residuals" , "log")

# Calculate the number of occurrences per unique "keyID" and specific values
swdCount <- merged_data %>%
  filter(typldw %in% swdC) %>%  # Filter rows with specific values
  group_by(keyID) %>%
  summarize(
    count = n()
  )

# View the resulting dataframe
print(swdCount)

# Create the pools of carbon per deadwood types
# Change the names in type of deadwood to classify carbon in standing wood debris (swdC)  
merged_data <- mutate(merged_data, typldw = if_else(typldw == "standing dead wood", "swdC", typldw))
merged_data <- mutate(merged_data, typldw = if_else(typldw == "stump or snag", "swdC", typldw))
# Change the names in type of deadwood to classify carbon in other lying piece of deadwood. youngRefractoryC (yrC)  
merged_data <- mutate(merged_data, typldw = if_else(typldw == "branch", "yrC", typldw))
merged_data <- mutate(merged_data, typldw = if_else(typldw == "logging residuals", "yrC", typldw))
merged_data <- mutate(merged_data, typldw = if_else(typldw == "log", "yrC", typldw))

# Summarize swdC and normalized it per ha
swdC <- merged_data %>%
  filter(typldw == "swdC") %>%
  group_by(keyID, typldw) %>%
  summarize(swdC_kgha = sum(C_kg)*4) # to normilized at ha scale cause we are in 50x50 with the obs data

# View the "swdC" summary dataframe
print(swdC)

# Summarize swdC and normalized it per ha
yrC <- merged_data %>%
  filter(typldw == "yrC") %>%
  group_by(keyID, typldw) %>%
  summarize(yrC_kgha = sum(C_kg)*4)

# View the "swdC" summary dataframe
print(yrC)

# Merge the two dataframes based on "keyID"
C_pools <- swdC %>%
  select(-typldw)%>% # to remove a not needed column
  left_join(yrC %>% select(keyID, yrC_kgha), by = "keyID")%>% # to select which column I want to merge, same below but in a different data frame
  left_join(swdCount %>% select(keyID, count), by = "keyID")

# Add a series for the total C pool
C_pools <- C_pools %>% 
  mutate(totalC_DW_kgha = sum(swdC_kgha + yrC_kgha))

# In case :: Replace missing values with 0 (if needed)
# merged_data[is.na(merged_data)] <- 0

# View the merged dataframe
print(C_pools)

# write excel
writexl::write_xlsx(C_pools, "C:/iLand/2023/20230901_Bottoms_Up/plot_init/Jenik/final_table_imp/CZ_JH1_C_pools.xlsx")


#-------------------------------------------------------------------------------
# Second part on the filling of the the Carbon pools to be calibrated into iLand
# Project file

# remember to create a table with otherswdC +30 %
# for example in L1_10 and L1_22
otherC <- 5103 * 30/100
otherC <- 749 * 30/100

# N in litter conversion in ha
0.617 kgC.m-2
0.617 * 10000

2826/418 # CN ratio in yrCN pool plotL1_22 so from this calculation I extracted the N kg
233/418
#-------------------------------------------------------------------------------


{# Convert the volume in C stocks per species
# Create a new dataframe with a unique combination of species and decay_class
unique_combinations <- expand.grid(
  GenusGlobal = unique(clean_deadwood_iland$GenusGlobal),
  Decay_class = unique(clean_deadwood_iland$Decay_class)
)

# Use left_join to combine data from the 2 tables based on species
combined_data <- unique_combinations %>%
  left_join(clean_deadwood_iland, by = c("GenusGlobal", "Decay_class")) %>%
  left_join(decstag, by = "GenusGlobal")

# Calculate the new column by multiplying deadwood and carbon_density
combined_data <- combined_data %>%
  mutate(new_column = volume * C_density_kgm3)
}
# View the resulting dataframe
print(combined_data)
View(combined_data)
writexl::write_xlsx(C_pools, "C:/iLand/2023/20230901_Bottoms_Up/plot_init/Jenik/final_table_imp/CZ_JH1_C_combined_data.xlsx")


#----------------------------------------------------------------------------------
# MAKE THE OPPOSITE PASS FROM CARBON IN SNAG POOL - ILAND, TO VOLUME