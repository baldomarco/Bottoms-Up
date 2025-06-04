library(RSQLite)
library(readxl)
library(writexl)

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


# Create the LAI time series

LAI <- landscape_4scen %>% 
  group_by(year) %>%
  summarise(LAI = sum(LAI))%>%
  select(year, LAI)
  
# EDIT IT FOR THE PREDICTORS BA BROADLEAF - TREE_10_40 - BROAD>40

#-------------------------------------------------------------------------------
# Calculate the number of trees between 10 and 40 cm

# Specify the range for DBH
dbh_min <- 10
dbh_max <- 40

# Calculate the number of trees in each year with DBH between 10 and 40 cm
tree_10_40 <- tree_4scen %>%
  filter(dbh >= dbh_min, dbh <= dbh_max) %>%
  group_by(year) %>%
  summarise(tree_10_40 = n())


#-------------------------------------------------------------------------------
# To define the species to be removed
unique_sp <- unique(landscape_4scen$species)  # alternative unique_plots <- unique(CZ_JH1[,"plotID"])
print(unique_sp)

species_to_remove <- c("piab", "pisy", "abal",
                       "lade")

# Use subset to filter the dataframe
filtered_broadl <- subset(landscape_4scen, !species %in% species_to_remove)

# Print the filtered dataframe
print(filtered_broadl)

# Sum BA for every broadleaf species in every year
summed_broadl_ba <- filtered_broadl %>%
  group_by(year) %>%
  summarize(basal_area_m2 = sum(basal_area_m2))%>%
  select(year,basal_area_m2)

View(summed_broadl_ba)

#-------------------------------------------------------------------------------
# Calculate the basal area only of the broadleave with a dbh > 40cm

# Create a data frame with all unique years
all_years <- data.frame(year = unique(tree_4scen$year))

# Perform left join with the summarization result
broadl_40 <- all_years %>%
  left_join(
    tree_4scen %>%
      group_by(year) %>%
      filter(dbh > 40 & !species %in% species_to_remove) %>%
      summarise(broadl_40 = n()),
    by = "year"
  ) %>%
  replace(is.na(.), 0)


#-------------------------------------------------------------------------------
# Age - comes from the avarage age

stand_age <- tree_4scen %>%
  group_by(year) %>%
  summarize(age = mean(age))
stand_age <- round(stand_age)

View(stand_age)

#-------------------------------------------------------------------------------
# DW volume - USE snags_c divided by 4 and converted into volume 
# Alternative use the Carbon instead that volume

#-----------------------------------------------------------------------------
# CREATE THE DATA FRAME FOR ADD VARIABLES ABOUT CARBON IN THE FINAL DATA FRAME

totalC_kgha_iland <- data.frame(carbon_4scen %>% 
                                  group_by(year) %>% 
                                  summarise(tot_carbon=sum(stem_c, branch_c, foliage_c, coarseRoot_c, fineRoot_c, regeneration_c, snags_c, snagsOther_c, downedWood_c, litter_c, soil_c)))


# Good one
totalC_kgha_DW_iland <- data.frame(carbon_4scen %>% 
                                     group_by(year) %>% 
                                     summarise(DW_C = sum(snags_c, snagsOther_c))) 

# Create a new row with manually specified values
new_row <- c(0, 4759.779)  # Add your values accordingly

# Add the new row at the beginning of the data frame
totalC_kgha_DW_iland <- rbind(new_row, totalC_kgha_DW_iland)

# Print the modified data frame
print(totalC_kgha_DW_iland)

#-------------------------------------------------------------------------------
# Merge the data frames Plot L1_10
plot_L1_10_df_simul <- bind_cols(stand_age, 
                                 DW_carbon = totalC_kgha_DW_iland$DW_C,
                                 lai_sim = LAI$LAI,
                                 ba_broadl = summed_broadl_ba$basal_area_m2,
                                 trees_10_40 = tree_10_40$tree_10_40,
                                 broadl_40 = broadl_40$broadl_40)


write_xlsx(plot_L1_10_df_simul, "C:/iLand/2023/20230901_Bottoms_Up/plot_init/Jenik/final_table_imp/tables_for_stat/plot_L1_10_df_simul.xlsx")
