# New species table synthesis in the main stat table

#-------------------------------------------------------------------------------
# Load necessary libraries
library(readxl)
library(dplyr)
library(readxl)
library(corrplot)
library(Hmisc)
library(RColorBrewer)
library(GGally)

#-------------------------------------------------------------------------------
# Define the path to the Excel file
file_path <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/Edit_Data_A_rozšířená_verze_COST_Marco_Baldo.xlsx"

#-------------------------------------------------------------------------------
# Get the sheet names from the Excel file
sheet_names <- excel_sheets(file_path)

#-------------------------------------------------------------------------------
# Create a list to store each sheet as a data frame
sheets_list <- lapply(sheet_names, function(sheet) {
  read_xlsx(file_path, sheet = sheet)
})

#-------------------------------------------------------------------------------
# Assign names to the list based on the sheet names
names(sheets_list) <- sheet_names

#-------------------------------------------------------------------------------
# Optionally, create individual R objects for each sheet (not recommended for many sheets)
list2env(sheets_list, envir = .GlobalEnv)

#-------------------------------------------------------------------------------
# Inspect the loaded data
print(sheet_names)    # List of sheet names
print(sheets_list)    # All sheets loaded into a list
str(sheets_list)      # Structure of the list

# Make the name consistent and check the tables
Terrestrial_isopods<- (`Terrestrial isopods`)

Vascular_plants
Bryophytes
Macromycetes
Lichens
Moths
Beetles
Millipedes
Terrestrial_isopods
Birds

#-------------------------------------------------------------------------------
# Calculate unique species count for each Plot ID in Vascular_plants
vascular_plants_species_counts <- Vascular_plants %>%
  filter(!is.na(Species)) %>%  # Remove rows where Species is NA
  group_by(Plot) %>%
  summarise(UniqueSpeciesCount = n_distinct(Species), .groups = "drop") %>%
  mutate(Plot = gsub("_(\\d)$", "_0\\1", Plot))  # Correct the Plot column to ensure two digits

#-------------------------------------------------------------------------------
# Calculate unique species count for each Plot ID in Bryophytes
bryophytes_species_counts <- Bryophytes %>%
  filter(!is.na(Species)) %>%  # Remove rows where Species is NA
  group_by(Plot) %>%
  summarise(UniqueSpeciesCount = n_distinct(Species), .groups = "drop") %>%
  mutate(Plot = gsub("_(\\d)$", "_0\\1", Plot))  # Correct the Plot column to ensure two digits

#-------------------------------------------------------------------------------
# Calculate unique species count for each Plot ID in Macromycetes
macromycetes_species_counts <- Macromycetes %>%
  filter(!is.na(Species)) %>%  # Remove rows where Species is NA
  group_by(Plot) %>%
  summarise(UniqueSpeciesCount = n_distinct(Species), .groups = "drop") %>%
  mutate(Plot = gsub("_(\\d)$", "_0\\1", Plot))  # Correct the Plot column to ensure two digits

#-------------------------------------------------------------------------------
# Calculate unique species count for each Plot ID in Lichens
lichens_species_counts <- Lichens %>%
  filter(!is.na(Species)) %>%  # Remove rows where Species is NA
  group_by(Plot) %>%
  summarise(UniqueSpeciesCount = n_distinct(Species), .groups = "drop") %>%
  mutate(Plot = gsub("_(\\d)$", "_0\\1", Plot))  # Correct the Plot column to ensure two digits

#-------------------------------------------------------------------------------
# Calculate unique species count for each Plot ID in Moths
moths_species_counts <- Moths %>%
  filter(!is.na(Species)) %>%  # Remove rows where Species is NA
  group_by(Plot) %>%
  summarise(UniqueSpeciesCount = n_distinct(Species), .groups = "drop") %>%
  mutate(Plot = gsub("_(\\d)$", "_0\\1", Plot))  # Correct the Plot column to ensure two digits

#-------------------------------------------------------------------------------
# Calculate unique species count for each Plot ID in Beetles
beetles_species_counts <- Beetles %>%
  filter(!is.na(Species)) %>%  # Remove rows where Species is NA
  group_by(Plot) %>%
  summarise(UniqueSpeciesCount = n_distinct(Species), .groups = "drop") %>%
  mutate(Plot = gsub("_(\\d)$", "_0\\1", Plot))  # Correct the Plot column to ensure two digits

#-------------------------------------------------------------------------------
# Calculate unique species count for each Plot ID in Millipedes
millipedes_species_counts <- Millipedes %>%
  filter(!is.na(Species)) %>%  # Remove rows where Species is NA
  group_by(Plot) %>%
  summarise(UniqueSpeciesCount = n_distinct(Species), .groups = "drop") %>%
  mutate(Plot = gsub("_(\\d)$", "_0\\1", Plot))  # Correct the Plot column to ensure two digits

#-------------------------------------------------------------------------------
# Calculate unique species count for each Plot ID in Terrestrial_isopods
terrestrial_isopods_species_counts <- Terrestrial_isopods %>%
  filter(!is.na(Species)) %>%  # Remove rows where Species is NA
  group_by(Plot) %>%
  summarise(UniqueSpeciesCount = n_distinct(Species), .groups = "drop") %>%
  mutate(Plot = gsub("_(\\d)$", "_0\\1", Plot))  # Correct the Plot column to ensure two digits

#-------------------------------------------------------------------------------
# Calculate unique species count for each Plot ID in Birds
birds_species_counts <- Birds %>%
  filter(!is.na(Species)) %>%  # Remove rows where Species is NA
  group_by(Plot) %>%
  summarise(UniqueSpeciesCount = n_distinct(Species), .groups = "drop") %>%
  mutate(Plot = gsub("_(\\d)$", "_0\\1", Plot))  # Correct the Plot column to ensure two digits

#-------------------------------------------------------------------------------
# Import the plot table for the multitaxon richness pred

tab1 <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/21_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood_test.xlsx")

#-------------------------------------------------------------------------------
# Manipulate and inspect the table
print(tab1)
str(tab1)

#-------------------------------------------------------------------------------
# Now make the plotID codes the same as in Plot column in the taxa group tables of species richness x plot

# Remove the letter 'L' from the plotID column
tab1 <- tab1 %>%
  mutate(plotID = gsub("^L", "", plotID))

# Inspect the updated table
print(tab1)

#-------------------------------------------------------------------------------
# Now let's compare the two vectors and remove all the plots where I do not have data

# Filter Vascular_plants_counts to include only rows with matching plotID in tab1
filtered_vascular_plants_counts <- vascular_plants_species_counts %>%
  filter(Plot %in% tab1$plotID)

# Filter bryophytes_counts to include only rows with matching plotID in tab1
filtered_bryophytes_counts <- bryophytes_species_counts %>%
  filter(Plot %in% tab1$plotID)

# Filter macromycetes_counts to include only rows with matching plotID in tab1
filtered_macromycetes_counts <- macromycetes_species_counts %>%
  filter(Plot %in% tab1$plotID)

# Filter lichens_counts to include only rows with matching plotID in tab1
filtered_lichens_counts <- lichens_species_counts %>%
  filter(Plot %in% tab1$plotID)

# Filter moths_counts to include only rows with matching plotID in tab1
filtered_moths_counts <- moths_species_counts %>%
  filter(Plot %in% tab1$plotID)

# Filter beetles_counts to include only rows with matching plotID in tab1
filtered_beetles_counts <- beetles_species_counts %>%
  filter(Plot %in% tab1$plotID)

# Filter millipedes_counts to include only rows with matching plotID in tab1
filtered_millipedes_counts <- millipedes_species_counts %>%
  filter(Plot %in% tab1$plotID)

# Filter terrestrial_isopods_counts to include only rows with matching plotID in tab1
filtered_terrestrial_isopods_counts <- terrestrial_isopods_species_counts %>%
  filter(Plot %in% tab1$plotID)

# Filter birds_counts to include only rows with matching plotID in tab1
filtered_birds_counts <- birds_species_counts %>%
  filter(Plot %in% tab1$plotID)

#-------------------------------------------------------------------------------
# To finish let's write a unique excel table with the count per plot per taxa
library(writexl)

# Create a unique ID for taxa
vascular_plants_counts <- filtered_vascular_plants_counts %>% mutate(Taxa = "Vascular_Plants")
bryophytes_counts <- filtered_bryophytes_counts %>% mutate(Taxa = "Bryophytes")
macromycetes_counts <- filtered_macromycetes_counts %>% mutate(Taxa = "Macromycetes")
lichens_counts <- filtered_lichens_counts %>% mutate(Taxa = "Lichens")
moths_counts <- filtered_moths_counts %>% mutate(Taxa = "Moths")
beetles_counts <- filtered_beetles_counts %>% mutate(Taxa = "Beetles")
millipedes_counts <- filtered_millipedes_counts %>% mutate(Taxa = "Millipedes")
terrestrial_isopods_counts <- filtered_terrestrial_isopods_counts %>% mutate(Taxa = "Terrestrial_Isopods")
birds_counts <- filtered_birds_counts %>% mutate(Taxa = "Birds")

# Combine all filtered counts into one table
combined_counts <- bind_rows(
  vascular_plants_counts,
  bryophytes_counts,
  macromycetes_counts,
  lichens_counts,
  moths_counts,
  beetles_counts,
  millipedes_counts,
  terrestrial_isopods_counts,
  birds_counts
)

# Reshape the table for better readability
final_table <- combined_counts %>%
  pivot_wider(names_from = Taxa, values_from = UniqueSpeciesCount, values_fill = 0)

# Add the letter 'L' to the Plot column
final_table <- final_table %>%
  mutate(Plot = paste0("L", Plot))

# Write to an Excel file
write_xlsx(final_table, "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/final_plot_counts_per_taxa.xlsx")

# Inspect the final table
print(final_table)


