library(RSQLite)

# 4th scenarios browsing probability = 0.6

file<-"C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/Test_L1_10_management1_brow0.6.sqlite"   # file to read


sqlite.driver <- dbDriver("SQLite")
db4 <- dbConnect(sqlite.driver, dbname = file)  # connect to the file
tables.in.the.file<-dbListTables(db4)           # explore the tables in the file
print(tables.in.the.file)


#-----------------------------------------------
# READ IN different tables:    

abeStand_4scen <- dbReadTable(db4, "abeStand")
abeStandDetail_4scen <- dbReadTable(db4, "abeStandDetail")
abeStandRemoval_4scen <- dbReadTable(db4, "abeStandRemoval")
abeUnit_4scen <- dbReadTable(db4, "abeUnit")
barkbeetle_4scen <- dbReadTable(db4,"barkbeetle")
carbon_4scen <- dbReadTable(db4,"carbon")
carbonflow_4scen <- dbReadTable(db4, "carbonflow")
dynamicstand_4scen <- dbReadTable(db4, "dynamicstand")
landscape_4scen <- dbReadTable(db4,"landscape")
landscape_removed_4scen <- dbReadTable(db4,"landscape_removed")
stand_4scen <- dbReadTable(db4, "stand")
tree_4scen <- dbReadTable(db4, "tree")

dbDisconnect(db4)    # close the file
}

# Create the LAI time series




# EDIT IT FOR THE PREDICTORS BA BROADLEAF - TREE_10_40 - BROAD>40

#-------------------------------------------------------------------------------
# Sum BA for every species in every plot

# Group by plotID and species, then calculate the sum of basal areas
summed_tree_areas <- new_data %>%
  group_by(plotID, treesp) %>%
  summarize(total_basal_area = sum(basal_area))

# Print the resulting dataframe
print(summed_tree_areas)

unique_plots <- unique(summed_tree_areas$plotID)  # alternative unique_plots <- unique(CZ_JH1[,"plotID"])
print(unique_plots) # 99 plots

write.csv(summed_tree_areas, "C:/iLand/2023/plot_bottoms_up/Jenik/summed_tree_areas.csv")

# Calculate the Shannon diversity index
shannon_index <- summed_tree_areas %>%
  group_by(plotID) %>%
  summarize(shannon = diversity(total_basal_area, base = exp(1)))

# Print the resulting dataframe
print(shannon_index)

write.csv(shannon_index, "C:/iLand/2023/plot_bottoms_up/Jenik/shannon_index_ba.csv")

#-------------------------------------------------------------------------------
# Calculate the number of trees between 10 and 40 cm
library(readxl)
library(writexl)

# Specify the range for DBH
dbh_min <- 10
dbh_max <- 40

# Calculate the number of trees in each plot with DBH between 10 and 40 cm
result <- data %>%
  filter(treedb >= dbh_min, treedb <= dbh_max) %>%
  group_by(plotID) %>%
  summarise(tree_10_40 = n())

# Generate a list of all unique plotIDs
all_plotIDs <- data %>%
  distinct(plotID)

# Left join the summarized data with the list of all unique plotIDs
result <- left_join(all_plotIDs, result, by = "plotID")

# Replace NA values with 0
result[is.na(result)] <- 0

# Print or use the result
print(result)

# In this modified script, we first generate a list of all unique plotID values and then use a left join to combine it with the summarized data. This ensures that even the plots with zero trees in the specified DBH range are included in the result. Finally, we replace any NA values with 0 to represent the plots with no matching trees in the given DBH range.

# Save the result to a new Excel file
write_xlsx(result, "trees_10_40_file.xlsx")


#-------------------------------------------------------------------------------
# Calculate the basal area only of the broadleave with a dbh > 40cm
# Define the conditions for filtering
dbh_condition_2 <- new_data$treedb > 40

# To define the species to be removed
unique_sp <- unique(new_data$treesp)  # alternative unique_plots <- unique(CZ_JH1[,"plotID"])
print(unique_sp) # in 99 plots

species_to_remove <- c("Picea abies", "Pinus sylvestris", "Larix decidua",
                       "Abies alba","Pinus nigra","Pinus strobus")

# Use subset to filter the dataframe
filtered_df <- subset(new_data, dbh_condition_2 & !treesp %in% species_to_remove)

# Print the filtered dataframe
print(filtered_df)

write.csv(filtered_df, "C:/iLand/2023/plot_bottoms_up/Jenik/broadl_filtered_results.csv")

# Now sum the basal area 
# Group by plotID and species, then calculate the sum of basal areas

summed_tree_areas_sp <- filtered_df %>%
  group_by(plotID, treesp) %>%
  summarize(total_basal_area = sum(basal_area))

# Print the resulting dataframe
print(summed_tree_areas_sp)

write.csv(summed_tree_areas_sp, "C:/iLand/2023/plot_bottoms_up/Jenik/summed_tree_areas_sp_over40dbh.csv")

# Do the same but only one value for each plot
summed_tree_areas_plot <- filtered_df %>%
  group_by(plotID) %>%
  summarize(total_basal_area = sum(basal_area))

# Print the resulting dataframe
print(summed_tree_areas_plot)

write.csv(summed_tree_areas_plot, "C:/iLand/2023/plot_bottoms_up/Jenik/summed_tree_ba_broadl_over40dbh.csv")

#-------------------------------------------------------------------------------
# Clean and harmonize 2 dataframes in the way to have the same plotID and n of rows

# Filter df1 to keep only rows with plotID values that exist in df2
tab2_broadl <- tab2 %>%
  filter(plotID %in% summed_tree_areas_plot$plotID)

# Use left_join to merge the harmonized dataframes based on plotID
merged_df <- left_join(tab2_broadl, summed_tree_areas_plot, by = "plotID")

# Print the merged dataframe
print(merged_df)

write.csv(merged_df, "C:/iLand/2023/plot_bottoms_up/Jenik/Bdv_predictors_table_final_broadl_40.csv")

#-------------------------------------------------------------------------------
{# Second way - To create the basal area of any plots per species from their dbh
  
  # Create a vector of DBH measurements for each tree in the plot
  dbh_measurements <- c(30, 40, 35, 25, 42, 38)  # Replace with your actual DBH measurements
  
  # Calculate the basal area for each tree
  basal_area_each_tree <- pi * (dbh_measurements / 200)^2  # Dividing by 2 and squaring gives area in square meters
  
  # Calculate the total basal area for the plot by summing the individual tree basal areas
  total_basal_area <- sum(basal_area_each_tree)
  
  # Print the results
  cat("Basal Area for Each Tree (in square meters):\n")
  print(basal_area_each_tree)
  
  cat("\nTotal Basal Area for the Plot (in square meters):\n")
  print(total_basal_area)
}




# Age - comes from the avarage age




# DW volume - USE snags_c divided by 4 and converted into volume 
# Alternative use the Carbon instead that volume




