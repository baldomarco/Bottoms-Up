library(ggplot2)
library(GGally)
library(cowplot)
library(corrplot)
library(dplyr)
library(fields)

#install.packages("readxl")
library(readxl)

#-------------------------------------------------------------------------------
# import tables
tab1 <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/Bdv_predictors_clean_correlation.xlsx") 

tab_brow <-read.csv("I:/iLand/2022/20220604_browsing_first/variables.DB_20220728.csv")

tab2 <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/Bdv_predictors_table_final_20231002.xlsx")

#-------------------------------------------------------------------------------
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

# stat_smooth()

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

#-------------------------------------------------------------------------------











# library(readxl) 
#-------------------------------------------------------------------------------
# NEW DATAFRAME 2024 06 05 MANAGEMENT AND SPECIES RICHNESS VS FOREST STRUCTURES

tab_sp <- read.csv("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/df_sr_c.csv")

tab2_mng <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/Bdv_predictors_table_final_20240528_managed.xlsx")

tab1 <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/snags_fun/Snags_function_all.xlsx")

#-------------------------------------------------------------------------------
# Manipulate and merge tables of species richness and forest structures
print(tab2_mng)
print(tab_sp)

# Using gsub to remove multiple substrings
tab_sp$plotID <- gsub("CZ_JH1_L1X|CZ_JH1_L2X|CZ_JH1_L3X|CZ_JH1_L4X|CZ_JH1_L5X|CZ_JH1_L6X", "", tab_sp$plotID)
print(tab_sp)
print(tab2_mng)

# First catch the mismatching 
# Print mismatching names
diff <- setdiff(tab_sp$plotID, tab2_mng$plotID)
diff

diff <- setdiff(tab2_mng$plotID, tab_sp$plotID)
diff

diff <- setdiff(tab1$plotID, tab_sp$plotID)
diff

diff <- setdiff(tab_sp$plotID, tab1$plotID)
diff

diff <- setdiff(tab1$plotID, tab2_mng$plotID)
diff


# Remove rows in taxon species richness in plots where are not present carbon stocks outputs
tab_sp_mng <- tab_sp %>% 
  filter(! (plotID %in% c("L1_18", "L1_22", "L1_24", "L1_26", "L1_27", "L2_27", "L2_30", "L2_33", "L2_34", "L3_06", "L3_09", "L3_10", "L3_16", "L4_06", "L4_09", "L4_31", "L4_33", "L6_05", "L6_08", "L6_10")))

#"L1_10" "L1_43" "L5_25" "L5_28" "L5_37" "L6_11" "L6_17"
tab2_mng <- tab2_mng %>% 
  filter(! (plotID %in% c("L1_10", "L1_18", "L1_43", "L5_25", "L5_28", "L5_37", "L6_11", "L6_17")))

tab1 <- tab1 %>% 
  filter(! (plotID %in% c("L1_10", "L1_27","L1_43", "L2_33", "L5_25", "L5_28", "L5_37", "L6_11", "L6_17", "L1_18", "L1_22", "L1_24", "L1_26", "L2_27", "L2_30", "L2_34", "L3_06", "L3_09", "L3_10", "L3_16", "L4_06", "L4_09", "L4_31", "L4_33", "L6_05", "L6_08", "L6_10")))

# Check the df str
str(tab_sp_mng)
str(tab2_mng)
str(tab1)

# Create the new data frame for needed variables
BDV_CORR_MNG <- data.frame(
  tab_sp_mng,
  age = tab2_mng$age,
  volume_dw = tab2_mng$volume_dw,
  ba = tab2_mng$ba,
  lai_sim = tab2_mng$lai_sim,
  lai_emp = tab2_mng$lai_emp,
  sh_tree = tab2_mng$sh,
  ba_bl = tab2_mng$ba_bl,
  max_dbh = tab2_mng$max_dbh,
  `tree<=40` = tab2_mng$`tree<=40`,
  `conf>40` = tab2_mng$`conf>40`,
  `bl>40` = tab2_mng$`bl>40`,
  `ba_bl>40` = tab2_mng$`ba_bl>40`,
  tot_dw_c = tab2_mng$tot_dw_c,
  snag_c = tab2_mng$snag_c,
  snag_c_sim = tab1$average,
  Age_MeanGAM = tab2_mng$Age_MeanGAM
)

# Remove the prefix 'tab_sp_mng.' from the column names
colnames(BDV_CORR_MNG) <- sub("tab_sp_mng\\.", "", colnames(BDV_CORR_MNG))

# Print the updated data frame to check the new column names
print(colnames(BDV_CORR_MNG))
print(BDV_CORR_MNG)

str(BDV_CORR_MNG)
head(BDV_CORR_MNG)
# SAVE THE DATAFRAME IN JENIK/TABLE_IMP
#write.csv(BDV_CORR_MNG, "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/BDV_CORR_MNG.csv")
# MAKE CONSISTENCY WITH THE SECOND PART OF THE CODE SO CALL THE DF TAB2
BDV_CORR_MNG

#-------------------------------------------------------------------------------
a.num<-BDV_CORR_MNG[,3:27]


# Look them all:
par(mfrow = c(1, 1), pty="m", mar=c(3,3,3,3), oma=c(0,0,0,0))
corrplot.mixed(cor(a.num),upper.col = col4(10),lower.col = "black", mar=c(0,0,0,0), tl.pos = "d")#, diag = "l")

#---------------------------------- just do the correlation plot with the selected variables

ggpairs(a.num)


#-------------------------------------------------------------------------------
# NEW WAY WITH THE BDV STAT TABLE FOR MANAGED STANDS
tab1 <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/15_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood.xlsx")
                                                                                                                                                            
#-------------------------------------------------------------------------------
# Manipulate and merge tables of species richness and forest structures
print(tab1)
str(tab1)


#-------------------------------------------------------------------------------
a.num<-tab1[,3:25]


# Look them all:
par(mfrow = c(1, 1), pty="m", mar=c(3,3,3,3), oma=c(0,0,0,0))
corrplot.mixed(cor(a.num),upper.col = col4(10),lower.col = "black", mar=c(0,0,0,0), tl.pos = "d")#, diag = "l")

#---------------------------------- just do the correlation plot with the selected variables

ggpairs(a.num)



#-------------------------------------------------------------------------------
# NEW WAY WITH THE BDV STAT TABLE FOR THE WHOLE DATASET INCLUDING CARBON POOLS
tab1 <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/11_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood - Copy.xlsx")
                   
#-------------------------------------------------------------------------------
# Manipulate and merge tables of species richness and forest structures
print(tab1)
str(tab1)


#-------------------------------------------------------------------------------
a.num<-tab1[,3:18]


# Look them all:
par(mfrow = c(1, 1), pty="m", mar=c(3,3,3,3), oma=c(0,0,0,0))
corrplot.mixed(cor(a.num),upper.col = col4(10),lower.col = "black", mar=c(0,0,0,0), tl.pos = "d")#, diag = "l")

#---------------------------------- just do the correlation plot with the selected variables

ggpairs(a.num)


#-------------------------------------------------------------------------------
# Here the is a section for create also the subset per site of the ggpair plot
#-------------------------------------------------------------------------------
# small function to display plots only if it is interactive
p_ <- GGally::print_if_interactive

pm <- ggpairs(tab1, columns = 2:18, ggplot2::aes(colour = management_type))
p_(pm)

ggpairs(tab1) # Use it to plot also box plots

# Create a scatter plot matrix
ggpairs(tab1, aes(colour = siteID)) # the same of above but with colors

#-------------------------------------------------------------------------------
# MANAGED - UNMANAGED
# small function to display plots only if it is interactive

tab1 <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/11_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood - Copy.xlsx")

p_ <- GGally::print_if_interactive

pm <- ggpairs(tab1, columns = 2:18, ggplot2::aes(colour = Managed))
p_(pm)

ggpairs(tab1) # Use it to plot also box plots

# Create a scatter plot matrix
ggpairs(tab1, aes(colour = Managed)) # the same of above but with colors
























































#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Load necessary libraries
library(readxl)
library(corrplot)
library(Hmisc)
library(RColorBrewer)
library(GGally)

#-------------------------------------------------------------------------------
# Load the dataset
tab1 <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/21_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood_test_clean.xlsx")

tab1 <- tab1 %>%
  mutate(DeadWood_C = DeadWood_C / 4)

# Second case
tab1 <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/20_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood.xlsx")

tab1 <- tab1 %>%
  mutate(DeadWood_C = DeadWood_C / 4)

#-------------------------------------------------------------------------------
# Manipulate and inspect the table
print(tab1)
str(tab1)

set.panel(2,2)

# Instogram of the ages

hist(tab1$Age25percentClosestTreesToDs)
hist(tab1$Age20percentOldestTrees)
hist(tab1$MeanGAMAge)
hist(tab1$MeanGAMAge_2)

set.panel(1,2)
hist(tab1$MeanGAMAge, breaks = 20)
hist(tab1$MeanGAMAge_2, breaks = 20)


#-------------------------------------------------------------------------------
# Subset the relevant columns for correlation (adjust indices or use column names as necessary)
a.num <- tab1[,4:16]

col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", "cyan", "#007FFF", "blue", "#00007F"))

# Look them all:
par(mfrow = c(1, 1), pty="m", mar=c(3,3,3,3), oma=c(0,0,0,0))
corrplot.mixed(cor(a.num),upper.col = col4(1000),lower.col = "black", mar=c(0,0,0,0), tl.pos = "d")#, diag = "l")


#---------------------------------- just do the correlation plot with the selected variables

ggpairs(a.num) # https://www.r-bloggers.com/2021/06/ggpairs-in-r-a-brief-introduction-to-ggpairs/

# To create boxplot interactive function
p_ <- GGally::print_if_interactive

# Not include management_type in the column list to not have a boxplot in the upper part otherwise include it if you want
pm <- ggpairs(tab1, columns = 3:29, ggplot2::aes(colour = management_type))

# To create boxplots in the upper part
p_(pm)


# METHOD 4
#install.packages("ggcorrplot")
library(ggcorrplot) # http://www.sthda.com/english/articles/32-r-graphics-essentials/130-plot-multivariate-continuous-data/#correlation-analysis
# Compute a correlation matrix
 my_data <- a.num[, c(1,2,3,4,5,6,7,8,9,10,11,12)]
 corr <- round(cor(my_data), 1)

# Visualize
ggcorrplot(corr, p.mat = cor_pmat(my_data),
           hc.order = FALSE, type = "lower",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE)

# METHOD 5
summary(a.num)
range(a.num)
a.num <- cor(a.num)
corrplot(a.num, type = "lower")
# METHOD 6
corrplot(a.num, type="upper", order="hclust", tl.col="black", tl.srt=45)
# METHOD 6
corrplot(a.num, type="upper", order="hclust")

# METHOD 7
# Create the corrplot with the desired settings
corrplot(
  a.num,                          # Your correlation matrix
  method = "color",               # Use squared layout
  type = "lower",                  # Keep the upper part of the matrix
  #order = "hclust",                # Clustered ordering
  addCoef.col = "black",           # Display coefficients inside the squares with black color
  #tl.srt = 90,                     # Rotate the text labels by 90 degrees
  tl.col = "red",                # Set the color of the text labels
  #col = colorRampPalette(c("blue", "white", "red"))(200),  # Color palette for correlation values
  cl.pos = "b"                     # Position the color legend to the right
)

# METHOD 8
# mat : matrice de donnée
# ... : Arguments supplémentaire à passer à la fonction cor.test
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# Matrice de p-value de la corrélation
p.mat <- cor.mtest(a.num)
head(p.mat[, 1:5])

# Method 8
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(a.num, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)

#-------------------------------------------------------------------------------
# Scatter plots
# Scatter plot using the original dataframe
ggplot(tab1) +
  # Points for Age25percentClosestTreesToDs
  geom_point(aes(x = Age25percentClosestTreesToDs, y = age, color = "Age25percentClosestTreesToDs")) +
  # Points for Age20percentOldestTrees
  geom_point(aes(x = Age20percentOldestTrees, y = age, color = "Age20percentOldestTrees")) +
  # Linear model fit for Age25percentClosestTreesToDs with dashed line
  geom_smooth(aes(x = Age25percentClosestTreesToDs, y = age, color = "Age25percentClosestTreesToDs"),
              method = "lm", linetype = "dashed") +
  # Linear model fit for Age20percentOldestTrees with dashed line
  geom_smooth(aes(x = Age20percentOldestTrees, y = age, color = "Age20percentOldestTrees"),
              method = "lm", linetype = "dashed") +
  # 1x1 line (continuous)
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black") +
  # Labels and title
  labs(x = "Tree Ages GAM models", y = "Observed Ages", title = "Observed Age vs. Tree Age Groups with LM Fit") +
  # Minimal theme
  theme_minimal()

#-------------------------------------------------------------------------------
# Scatter plots
# Scatter plot using the original dataframe
ggplot(tab1) +
  # Points for Age25percentClosestTreesToDs
  geom_point(aes(x = MeanGAMAge, y = age, color = "MeanGAMAge")) +
  # Points for Age20percentOldestTrees
  geom_point(aes(x = MeanGAMAge_2, y = age, color = "MeanGAMAge_2")) +
  # Linear model fit for Age25percentClosestTreesToDs with dashed line
  geom_smooth(aes(x = MeanGAMAge, y = age, color = "MeanGAMAge"),
              method = "lm", linetype = "dashed") +
  # Linear model fit for Age20percentOldestTrees with dashed line
  geom_smooth(aes(x = MeanGAMAge_2, y = age, color = "MeanGAMAge_2"),
              method = "lm", linetype = "dashed") +
  # 1x1 line (continuous)
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black") +
  # Labels and title
  labs(x = "Tree MeanGAMAges GAM models", y = "Observed Ages", title = "Observed Age vs. Tree meanGAMAge Groups with LM Fit") +
  # Minimal theme
  theme_minimal()


#-------------------------------------------------------------------------------
# Scatter plots
# Scatter plot using the original dataframe
ggplot(tab1) +
  # Points for Age25percentClosestTreesToDs
  geom_point(aes(age, MeanGAMAge, colour = management_type)) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black") +
  # Labels and title
  labs(x = "Tree MeanGAMAges 1 [year]", y = "Observed Ages [year]", title = "Tree Mean Ages based on dbh-height proportion GAM model") +
  # Minimal theme
  theme_minimal()

# Scatter plot using the original dataframe
ggplot(tab1) +
  # Points for Age25percentClosestTreesToDs
  geom_point(aes(age, MeanGAMAge_2, colour = management_type)) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black") +
  # Labels and title
  labs(x = "Tree MeanGAMAges 2 [year]", y = "Observed Ages [year]", title = "Tree Mean Ages based on dbh-height weighted GAM model") +
  # Minimal theme
  theme_minimal()


# Scatter plot using the original dataframe
ggplot(tab1) +
  # Points for Age25percentClosestTreesToDs
  geom_point(aes(age, Age20percentOldestTrees, colour = management_type)) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black") +
  # Labels and title
  labs(x = "20% Oldest Tree Ages [year]", y = "Observed Ages [year]", title = "20% Oldest Tree Ages based on dbh-height weighted GAM model") +
  # Minimal theme
  theme_minimal()


# Scatter plot using the original dataframe
ggplot(tab1) +
  # Points for Age25percentClosestTreesToDs
  geom_point(aes(age, Age25percentClosestTreesToDs, colour = management_type)) +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black") +
  # Labels and title
  labs(x = "25% Oldest Tree Ages [year]", y = "Observed Ages [year]", title = "25% Oldest Tree Ages based on dbh-height proportion GAM model") +
  # Minimal theme
  theme_minimal()


#-------------------------------------------------------------------------------
# Box plots
ggplot(tab1) +
  # Boxplot for observed_age
  geom_boxplot(aes(x = "Observed Age", y = age), outlier.shape = 8, outlier.colour = "red") +  # Keep outliers
  geom_jitter(aes(x = "Observed Age", y = age, color = management_type), shape = 16, size = 2, width = 0.2) +
  
  # Boxplot for Age25percentClosestTreesToDs
  geom_boxplot(aes(x = "Age25percentClosestTreesToDs", y = Age25percentClosestTreesToDs), outlier.shape = 8, outlier.colour = "red") +
  geom_jitter(aes(x = "Age25percentClosestTreesToDs", y = Age25percentClosestTreesToDs, color = management_type), shape = 16, size = 2, width = 0.2) +
  
  # Boxplot for Age20percentOldestTrees
  geom_boxplot(aes(x = "Age20percentOldestTrees", y = Age20percentOldestTrees), outlier.shape = 8, outlier.colour = "red") +
  geom_jitter(aes(x = "Age20percentOldestTrees", y = Age20percentOldestTrees, color = management_type), shape = 16, size = 2, width = 0.2) +
  
  # Labels and title
  labs(x = "Age Group", y = "Age Value [year]", title = "Boxplot with Red Stars for Outliers and Managed Groups in Colors") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))  # Bold black border

#-------------------------------------------------------------------------------
# Scatter plot to compare the species richness - age

ggplot(tab1) +
  # Points for Age25percentClosestTreesToDs vs. species richness
  geom_point(aes(x = Age25percentClosestTreesToDs, y = tab1$`Lichens (0.137)`, color = "Age25percentClosestTreesToDs")) +
  
  # Points for Age20percentOldestTrees vs. species richness
  geom_point(aes(x = Age20percentOldestTrees, y = tab1$`Lichens (0.137)`, color = "Age20percentOldestTrees")) +
  
  # Linear model fit for Age25percentClosestTreesToDs vs. species richness with dashed line
  geom_smooth(aes(x = Age25percentClosestTreesToDs, y = tab1$`Lichens (0.137)`, color = "Age25percentClosestTreesToDs"),
              method = "lm", linetype = "dashed") +
  
  # Linear model fit for Age20percentOldestTrees vs. species richness with dashed line
  geom_smooth(aes(x = Age20percentOldestTrees, y = tab1$`Lichens (0.137)`, color = "Age20percentOldestTrees"),
              method = "lm", linetype = "dashed") +
  
  # 1x1 line (continuous) - optional, if you want a comparison to the "1:1" relationship
  geom_abline(slope = 0.05, intercept = 0, linetype = "solid", color = "black") +
  
  # Labels and title
  labs(x = "Tree Ages (GAM Models)", y = "Species Richness (Lichens)", title = "Comparison of Tree Ages GAM Models vs. Species Richness") +
  
  # Minimal theme
  theme_minimal() +
  
  # Adding color legend and titles for the two models
  scale_color_manual(name = "GAM Models",
                     values = c("Age25percentClosestTreesToDs" = "blue",
                                "Age20percentOldestTrees" = "green"))

#-------------------------------------------------------------------------------
# AFTER NORMALIZATION
#-------------------------------------------------------------------------------

# Normalize function (min-max scaling)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Function to normalize the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to the relevant columns in your dataframe
tab1$Age25percentClosestTreesToDs_norm <- normalize(tab1$Age25percentClosestTreesToDs)
tab1$Age20percentOldestTrees_norm <- normalize(tab1$Age20percentOldestTrees)
tab1$obs_age_norm <- normalize(tab1$age)

# BRYOPHYTES
tab1$species_richness_bryophytes_norm <- normalize(tab1$`Epiphytic / epixilic bryophytes (0.212)`)
bryophytes <- ggplot(tab1) +
  geom_point(aes(x = Age25percentClosestTreesToDs_norm, y = species_richness_bryophytes_norm, color = "Age25percentClosestTreesToDs")) +
  geom_point(aes(x = Age20percentOldestTrees_norm, y = species_richness_bryophytes_norm, color = "Age20percentOldestTrees")) +
  geom_point(aes(x = obs_age_norm, y = species_richness_bryophytes_norm, color = "obs_age")) +
  geom_smooth(aes(x = Age25percentClosestTreesToDs_norm, y = species_richness_bryophytes_norm, color = "Age25percentClosestTreesToDs"),
              method = "lm", linetype = "dashed") +
  geom_smooth(aes(x = Age20percentOldestTrees_norm, y = species_richness_bryophytes_norm, color = "Age20percentOldestTrees"),
              method = "lm", linetype = "dashed") +
  geom_smooth(aes(x = obs_age_norm, y = species_richness_bryophytes_norm, color = "obs_age"),
              method = "lm", linetype = "dashed") +
  # 1x1 line (continuous) - optional, if you want a comparison to the "1:1" relationship
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black") +
  
  labs(x = "Normalized Tree Ages (GAM Models and Observed)", y = "Normalized Species Richness (Bryophytes)", 
       title = "Bryophytes") +
  theme_minimal() +
  scale_color_manual(name = "Age Models",
                     values = c("Age25percentClosestTreesToDs" = "blue",
                                "Age20percentOldestTrees" = "green",
                                "obs_age" = "red"))

# LICHENS
tab1$species_richness_lichens_norm <- normalize(tab1$`Lichens (0.137)`)
lichens <- ggplot(tab1) +
  geom_point(aes(x = Age25percentClosestTreesToDs_norm, y = species_richness_lichens_norm, color = "Age25percentClosestTreesToDs")) +
  geom_point(aes(x = Age20percentOldestTrees_norm, y = species_richness_lichens_norm, color = "Age20percentOldestTrees")) +
  geom_point(aes(x = obs_age_norm, y = species_richness_lichens_norm, color = "obs_age")) +
  geom_smooth(aes(x = Age25percentClosestTreesToDs_norm, y = species_richness_lichens_norm, color = "Age25percentClosestTreesToDs"),
              method = "lm", linetype = "dashed") +
  geom_smooth(aes(x = Age20percentOldestTrees_norm, y = species_richness_lichens_norm, color = "Age20percentOldestTrees"),
              method = "lm", linetype = "dashed") +
  geom_smooth(aes(x = obs_age_norm, y = species_richness_lichens_norm, color = "obs_age"),
              method = "lm", linetype = "dashed") +
  # 1x1 line (continuous) - optional, if you want a comparison to the "1:1" relationship
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black") +
  
  labs(x = "Normalized Tree Ages (GAM Models and Observed)", y = "Normalized Species Richness (Lichens)", 
       title = "Lichens") +
  theme_minimal() +
  scale_color_manual(name = "Age Models",
                     values = c("Age25percentClosestTreesToDs" = "blue",
                                "Age20percentOldestTrees" = "green",
                                "obs_age" = "red"))

# MACROFUNGI
tab1$species_richness_macrofungi_norm <- normalize(tab1$`Macrofungi (2.118)`)
macrofungi <- ggplot(tab1) +
  geom_point(aes(x = Age25percentClosestTreesToDs_norm, y = species_richness_macrofungi_norm, color = "Age25percentClosestTreesToDs")) +
  geom_point(aes(x = Age20percentOldestTrees_norm, y = species_richness_macrofungi_norm, color = "Age20percentOldestTrees")) +
  geom_point(aes(x = obs_age_norm, y = species_richness_macrofungi_norm, color = "obs_age")) +
  geom_smooth(aes(x = Age25percentClosestTreesToDs_norm, y = species_richness_macrofungi_norm, color = "Age25percentClosestTreesToDs"),
              method = "lm", linetype = "dashed") +
  geom_smooth(aes(x = Age20percentOldestTrees_norm, y = species_richness_macrofungi_norm, color = "Age20percentOldestTrees"),
              method = "lm", linetype = "dashed") +
  geom_smooth(aes(x = obs_age_norm, y = species_richness_macrofungi_norm, color = "obs_age"),
              method = "lm", linetype = "dashed") +
  # 1x1 line (continuous) - optional, if you want a comparison to the "1:1" relationship
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black") +
  
  labs(x = "Normalized Tree Ages (GAM Models and Observed)", y = "Normalized Species Richness (Macrofungi)", 
       title = "Macrofungi") +
  theme_minimal() +
  scale_color_manual(name = "Age Models",
                     values = c("Age25percentClosestTreesToDs" = "blue",
                                "Age20percentOldestTrees" = "green",
                                "obs_age" = "red"))

# BEETLES
tab1$species_richness_beetles_norm <- normalize(tab1$`Non-flying beetles (0.053)`)
beetles <- ggplot(tab1) +
  geom_point(aes(x = Age25percentClosestTreesToDs_norm, y = species_richness_beetles_norm, color = "Age25percentClosestTreesToDs")) +
  geom_point(aes(x = Age20percentOldestTrees_norm, y = species_richness_beetles_norm, color = "Age20percentOldestTrees")) +
  geom_point(aes(x = obs_age_norm, y = species_richness_beetles_norm, color = "obs_age")) +
  geom_smooth(aes(x = Age25percentClosestTreesToDs_norm, y = species_richness_beetles_norm, color = "Age25percentClosestTreesToDs"),
              method = "lm", linetype = "dashed") +
  geom_smooth(aes(x = Age20percentOldestTrees_norm, y = species_richness_beetles_norm, color = "Age20percentOldestTrees"),
              method = "lm", linetype = "dashed") +
  geom_smooth(aes(x = obs_age_norm, y = species_richness_beetles_norm, color = "obs_age"),
              method = "lm", linetype = "dashed") +
  # 1x1 line (continuous) - optional, if you want a comparison to the "1:1" relationship
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black") +
  
  labs(x = "Normalized Tree Ages (GAM Models and Observed)", y = "Normalized Species Richness (Beetles)", 
       title = "Beetles") +
  theme_minimal() +
  scale_color_manual(name = "Age Models",
                     values = c("Age25percentClosestTreesToDs" = "blue",
                                "Age20percentOldestTrees" = "green",
                                "obs_age" = "red"))

# Arrange the plots in a 2x2 grid
grid.arrange(bryophytes, lichens, macrofungi, beetles, ncol = 2, nrow = 2)




#-------------------------------------------------------------------------------
ggplot(tab1) +
  
  # Points for Age25percentClosestTreesToDs vs. species richness, colored by management_type
  geom_point(aes(x = Age25percentClosestTreesToDs, y = tab1$`Lichens (0.137)`, color = management_type)) +
  
  # Points for Age20percentOldestTrees vs. species richness, colored by management_type
  geom_point(aes(x = Age20percentOldestTrees, y = tab1$`Lichens (0.137)`, color = management_type)) +
  
  # Points for obs_age vs. species richness, colored by management_type
  geom_point(aes(x = age, y = tab1$`Lichens (0.137)`, color = management_type)) +
  
  # Linear model fit for Age25percentClosestTreesToDs vs. species richness with dashed line
  geom_smooth(aes(x = Age25percentClosestTreesToDs, y = tab1$`Lichens (0.137)`),
              method = "gam", linetype = "dashed", color = "blue") +
  
  # Linear model fit for Age20percentOldestTrees vs. species richness with dashed line
  geom_smooth(aes(x = Age20percentOldestTrees, y = tab1$`Lichens (0.137)`),
              method = "gam", linetype = "dashed", color = "green") +
  
  # Linear model fit for obs_age vs. species richness with dashed line
  geom_smooth(aes(x = age, y = tab1$`Lichens (0.137)`),
              method = "gam", linetype = "dashed", color = "red") +
  
  # Labels and title
  labs(x = "Tree Ages (GAM Models and Observed)", y = "Species Richness (Lichens)", 
       title = "Comparison of Observed Age and Tree Ages (GAM Models) vs. Lichens Species Richness") +
  
  # Minimal theme
  theme_minimal() +
  
  # Adding color legend for management_type
  scale_color_discrete(name = "Management Type")






#

































































#-------------------------------------------------------------------------------
# Let's create a box plot for the deadwood pools in unmanaged stands
library(ggplot2)
library(broom)
library(tidyr)
library(dplyr)

tab1 <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/08_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood_and_prio.xlsx")

tab1_unmng <- tab1%>%
  filter(Managed == "0")

# Melt the data frame for ggplot

tab1_unmng_melt <- tab1_unmng %>%
  select(SNAG_sim , AG_DW_C_sim ) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

#----------------------------
# Create the boxplot
p <- ggplot(tab1_unmng_melt, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white", color = "black")) +
  labs(title = "Boxplot of SNAG_sim and AG_DW_C_sim", x = "Variable", y = "Deadwood Carbon [kg/ha]")

print(p)

#----------------------------
# Perform linear regression
lm_model <- lm(`Macrofungi (2.118)` ~ AG_DW_C_sim, data = tab1)

# Get regression summary
lm_summary <- summary(lm_model)
lm_text <- paste("R-squared:", round(lm_summary$r.squared, 2), 
                 "\nIntercept:", round(lm_summary$coefficients[1, 1], 2), 
                 "\np-value:", format.pval(lm_summary$coefficients[2, 4]))

# Add regression results to the plot
p <- p + annotate("text", x = 1.5, y = max(tab1_unmng_melt$value), label = lm_text, hjust = 0.5, size = 4, color = "black")

print(p)


