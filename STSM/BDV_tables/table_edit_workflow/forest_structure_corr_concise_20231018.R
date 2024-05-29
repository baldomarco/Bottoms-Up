library(ggplot2)
library(GGally)
library(cowplot)
library(corrplot)
library(dplyr)
library(fields)

#install.packages("readxl")
library(readxl)

tab1 <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/Bdv_predictors_clean_correlation.xlsx") 

tab_brow <-read.csv("I:/iLand/2022/20220604_browsing_first/variables.DB_20220728.csv")

tab2 <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/Bdv_predictors_table_final_20231002.xlsx")

tab2 <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/Bdv_predictors_table_final_20240528.xlsx")

#head(tab1)
head(tab2)

plot(tab2$age, tab2$basal_area)

col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", "cyan", "#007FFF", "blue", "#00007F"))


#---------------------------------- just do the correlation plot all together
a.num<-tab2[,3:17]

# Look them all:
par(mfrow = c(1, 1), pty="m", mar=c(3,3,3,3), oma=c(0,0,0,0))
corrplot.mixed(cor(a.num),upper.col = col4(10),lower.col = "black", mar=c(0,0,0,0), tl.pos = "d")#, diag = "l")


#---------------------------------- 
# just do the correlation plot with the selected variables

scenarios <- tab2 %>% 
  filter(case == "B0_Tby1_PRby0")

a.num_selected <-scenarios[,-1]
# Look them all:
par(mfrow = c(1, 1), pty="m", mar=c(3,3,3,3), oma=c(0,0,0,0))
corrplot.mixed(cor(a.num_selected),upper.col = col4(10),lower.col = "black", mar=c(0,0,0,0), tl.pos = "d")#, diag = "l")


#---------------------------------- just do the correlation plot with the selected variables

ggpairs(a.num)


# Second part - Calculate Shannon of every plot based on the species basal area

library(vegan)

# Calculate the basal area per every tree based on their dbh

raw_data <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/Raw_data_structure_CZ_JH1_final.xlsx"

excel_sheets(raw_data)

raw_data_sp <- "live_dead_trees"

data <- read_excel(raw_data,raw_data_sp, col_names=T)

new_data <- data %>% 
  mutate(basal_area = pi * (treedb / 200)^2)

print(new_data)

# write.csv(new_data, "C:/iLand/2023/plot_bottoms_up/Jenik/Raw_data_with_basal_area.csv")

# Visualize the data in a single plot

chosen_plot_id <- "L5_37"

# Filter data for the chosen plot
filtered_data <- new_data %>%
  filter(plotID == chosen_plot_id)

print(filtered_data)


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
write_xlsx(result, "result_file.xlsx")


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

write.csv(filtered_df, "C:/iLand/2023/plot_bottoms_up/Jenik/hhh.csv")

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
{# This in case I want to add rows instead then remove them
  # Step 1: Identify missing plotID values
  missing_plotID <- setdiff(tab2$plotID, summed_tree_areas_plot$plotID)
  
  # Step 2: Create rows with missing plotID values in df2
  missing_rows <- data.frame(plotID = missing_plotID)
  tab2_edit <- rbind(tab2, missing_rows)
  
  # Step 3: Merge the dataframes
  merged_df <- merge(tab2, summed_tree_areas_plot, by = "plotID", all = TRUE)
  
  # Print the merged dataframe
  print(merged_df)}
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

#-------------------------------------------------------------------------------
# Create the proportion for the species seed background probability 

tab2 <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/Bdv_predictors_table_final_20231002.xlsx")

#head(tab1)
head(tab1)

# import the tree data

raw_data <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/Raw_data_structure_CZ_JH1_final.xlsx"

excel_sheets(raw_data)

raw_data_sp <- "live_dead_trees"

data <- read_excel(raw_data,raw_data_sp, col_names=T)

print(data)

# write.csv(new_data, "C:/iLand/2023/plot_bottoms_up/Jenik/Raw_data_with_basal_area.csv")

#-------------------------------------------------------------------------------
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
site_species_proportions <- site_species_counts %>%
  group_by(Site) %>%
  mutate(SpeciesProportion = TreeCount / sum(TreeCount))

print(site_species_proportions)
show(site_species_proportions)

# Filter the data for the specific site 'L1'
site_L1_proportions <- subset(site_species_proportions, Site == 'L1')

# Extract the species proportions for 'L1'
species_proportions_L1 <- site_L1_proportions$SpeciesProportion

# Print the array or string of species proportions
cat("Species Proportions in Site L1:", species_proportions_L1, "\n")

#-------------------------------------------------------------------------------------------------

# we select here the variables that we want based on the correlation plot:
# early species proportion
a <-data.frame(year=tab1$year,case=tab1$case,dbh=tab1$dbh,esp_BA_prop=tab1$esp_BA_prop,tot_carbon=tab1$tot_carbon, age=tab1$age)

head(a)

# we select here the variables that we want based on the correlation plot:
k_csp<-data.frame(year=tab2$year,case=tab2$case,dbh=tab2$dbh,csp_BA_prop=tab2$csp_BA_prop,tot_carbon=tab2$tot_carbon, age=tab2$age)

head(k_csp)

############################################################################# not smoothed
#--------------------------- dbh, earlyspecies, total carbon


library(plotly)
data<-data.frame(x=k_csp$dbh,y=k_csp$csp_BA_prop,z=k_csp$tot_carbon, case=k_csp$case)
plot_ly(data, x = ~x, y = ~y, z = ~z, split=~case, type = 'scatter3d', mode = 'lines')

#--------------------------- age, earlyspecies, total carbon

data<-data.frame(x=a$age,y=a$esp_BA_prop,z=a$tot_carbon,years=c(1:nyears), case=a$case)
plot_ly(data, x = ~x, y = ~y, z = ~z, split=~case, type = 'scatter3d', mode = 'lines')


#--------------------------- other trial:
data<-data.frame(x=a$dbh,y=a$H.BA,z=a$tot_carbon,years=c(1:nyears), case=a$case)
plot_ly(data, x = ~x, y = ~y, z = ~z, split=~case, type = 'scatter3d', mode = 'lines')

#---------------------------------------------------------------------------------------------------------
# Do the smoothing

library("zoo")

k_esp<-a_esp %>% group_by(case) %>% mutate(s.dbh=rollmean(dbh,11,align = "left",na.pad = T), 
                                           s.esp_BA_prob=rollmean(esp_BA_prop,11,align = "left",na.pad = T),
                                           s.tot_carbon=rollmean(tot_carbon,11,align = "left",na.pad = T))

# IT WAS THE PREVIOUS 
# data<-data.frame(x=k_esp$s.dbh,y=k_esp$s.esp_BA_prob,z=k_esp$s.tot_carbon,years=c(1:nyears), case=k_esp$case)

data<-data.frame(x=k_esp$s.dbh,y=k_esp$s.esp_BA_prob,z=k_esp$s.tot_carbon, case=k_esp$case)
plot_ly(data, x = ~x, y = ~y, z = ~z, split=~case, type = 'scatter3d', mode = 'lines')

#+ stat_smooth()

# 3D plot second table with Conifer proportion


k_csp<-k_csp %>% group_by(case) %>% mutate(s.dbh=rollmean(dbh,20,align = "left",na.pad = T), 
                                           s.csp_BA_prob=rollmean(csp_BA_prop,20,align = "left",na.pad = T),
                                           s.age=rollmean(age,20,align = "left",na.pad = T),
                                           s.tot_carbon=rollmean(tot_carbon,20,align = "left",na.pad = T))


#csp<-data.frame(x=k_csp$s.dbh,y=k_csp$s.csp_BA_prob,z=k_csp$s.tot_carbon,years=c(1:nyears), case=a_csp$case)
#plot_ly(data, x = ~x, y = ~y, z = ~z, split=~case, type = 'scatter3d', mode = 'lines')



csp<-data.frame(x=k_csp$s.age,y=k_csp$s.csp_BA_prob,z=k_csp$s.tot_carbon, case=k_csp$case)


a <- plot_ly(csp, x = ~x, y = ~y, z = ~z, split=~case, type = 'scatter3d', mode = 'lines', 
             colors = c("grey50", "blue", "red"))
a

pdf(a)

