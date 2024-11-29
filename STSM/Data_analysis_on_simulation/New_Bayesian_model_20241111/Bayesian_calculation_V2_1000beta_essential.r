# Marco Baldo 25-11-2024 
# To replicate the code be sure to use the same data, right directory and clean the R environment before to start
# Create iLand simulation results from the Bottoms-Up model plots to analyze the multi-taxa biodiversity indicators in the forest ecosystems of Czechia
# Refer the results and visualization to the pdf within this same GitHub folder: OK_final_20231214_BDV_mng_plot_L1_10_300.pdf
# Computer path -> C:/iLand/2023/20230901_Bottoms_Up/outputs/20231129/OK_final_20231214_BDV_mng_plot_L1_10_300.pdf

# Jenik grid just to control if needed
# Load required libraries

library(readxl)
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(RSQLite)
library(vegan)
library(fields)


# source data table:
t1<-read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/Bdv_predictors_v2.xlsx")
t2<-read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/Jenyk_BDV_predictors_ENG.xlsx")


head(t1) # how many sheets are there and their name in the excel file f.
head(t2)

# Uploading all the database in one


# BDV predictors by Jenik
BDV_predictors <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/20_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood.xlsx")

BDV_predictors <- BDV_predictors%>%
  group_by(plotID)%>%
  mutate(deadwood50 = (deadwood/4))


# Define file paths for all taxa
file_paths <- list(
  beetles = "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bayesian_model_results_V4_x1000/Beetles_bayesian_beta_x1000.csv",
  bryophytes = "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bayesian_model_results_V4_x1000/Bryophytes_bayesian_beta_x1000.csv",
  lichens = "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bayesian_model_results_V4_x1000/Lichens_bayesian_beta_x1000.csv",
  macrofungi = "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bayesian_model_results_V4_x1000/Macrofungi_bayesian_beta_x1000.csv",
  macrofungi_red = "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bayesian_model_results_V4_x1000/Macrofungi_RED_bayesian_beta_x1000.csv",
  moths = "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bayesian_model_results_V4_x1000/Moths_bayesian_beta_x1000.csv",
  moths_red = "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bayesian_model_results_V4_x1000/Moths_RED_bayesian_beta_x1000.csv"
)


#-------------------------------------------------------------------------------

setwd("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/")

# Path to the directory containing your SQLite databases
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"

# Get a list of all SQLite databases in the directory
# database_files <- list.files(path = dataroot, pattern = ".sqlite", full.names = TRUE)

{# Create an empty list to store data frames
  dfs <- list() # not working for several subset, only one
}


# Create the list of dataframes I want to save

abeStand_scen <- c()

abeStandDetail_scen <- c()

abeStandRemoval_scen <- c()

abeUnit_scen <- c()

#bb_scen <- c()

landscape_removed_scen <- c()

tree_scen <- c()

stand_scen <- c()

lnd_scen <-c()

dys_scen <- c()

carbon_scen <- c()

carbonflow_scen <- c()

removals <- c()

#damage.all <- c()

H_BA_heterogenity_scen <- c()

landscape_removed_scen_natmor <- c()

# damage.all<-c()

# landscape_removed <- c()

# management <- ()

variables.all <- c()

plot_variables_all <- c()

Bayesian_BDV_model_V3_multi <- c()

bayesian_results_all <- c()

summary_bayesian_results <- c()

#-------------------------------------------------------------------------------
# import the list of files within the folder in dataroot with .sqlite extension

database_files <- list.files(path = dataroot, pattern = ".sqlite", full.names = FALSE)                              # alternative way to select all the databases within a folder select all the file with sqlite format
database_files <- list.files(dataroot, ".sqlite")  

for (i in (1:length(database_files)))  {    # We read in the files in the loop. The "i" is for the x from 1 to i lenght of the dataset of files 
  
  
  # i<-1 # to test but remember to don't run also the }
  
  
  # Name of the database
  file <-paste0(database_files[i])                                    # File to read here the case is always the actual case in the loop
  # "file"= name of the object, "paste0"+ function to create a NAME for a computer path of selection of data/objects
  
  # case<- strsplit(all_v[i],".s")[[1]][1]      # Why we used it in the paper analysis? # explanation of this function is needed
  case<-database_files[i] 
  
  
  # Control
  print(file)
  
  # connect to the database 
  sqlite.driver <- dbDriver("SQLite")
  db <- dbConnect(sqlite.driver, dbname = file)  # connect to the file
  tables.in.the.file<-dbListTables(db)           # explore the tables in the file
  print(tables.in.the.file)
  
  
  #-----------------------------------------------------------------------------
  # LOAD THE DATABASE # Read tables from the database
  
  abeStand <- dbReadTable(db, "abeStand")
  abeStandDetail <- dbReadTable(db, "abeStandDetail")
  abeStandRemoval <- dbReadTable(db, "abeStandRemoval")
  abeUnit <- dbReadTable(db, "abeUnit")
  #barkbeetle <- dbReadTable(db,"barkbeetle")
  carbon <- dbReadTable(db,"carbon")
  carbonflow <- dbReadTable(db, "carbonflow")
  dynamicstand <- dbReadTable(db, "dynamicstand")
  landscape <- dbReadTable(db,"landscape")
  landscape_removed <- dbReadTable(db,"landscape_removed")
  stand <- dbReadTable(db, "stand")
  tree <- dbReadTable(db, "tree")
  
  dbDisconnect(db)    # close the file
  
  # TO UNDERSTAND THE OPERATORS %>% AND %IN% HAVE TO STUDY THEM IN DATACAMP AND IN DPLYR CRAN PACKAGES
  
  #-----------------------------------------------------------------------------
  # CREATE SHANNON VARIABLE
  
  annual.data<-landscape %>% group_by(year) %>% filter(year>0) %>% summarize(VOL.tot=sum(volume_m3), BA.tot=sum(basal_area_m2), count.tot=sum(count_ha))
  annual.spec.data<-landscape %>% group_by(year, species) %>%  filter(year>0) %>% summarize(VOL=(volume_m3), BA=(basal_area_m2), count=sum(count_ha))
  
  print(head(annual.data))
  print(head(annual.spec.data))
  
  S<-landscape %>% group_by(year) %>% filter(volume_m3>0 & year>0) %>% summarise(n=n())   # number of species in each year  (added the filter to count non-zero volumes, now it is okay)
  print(S$n)
  
  
  t<-annual.spec.data %>% right_join(annual.data, by="year")  %>% 
    mutate(prop.VOL=VOL/VOL.tot, prop.BA=BA/BA.tot, prop.count=count/count.tot) %>% 
    filter(prop.VOL>0)  # here I also filtering them to have only records with m3>0
  
  #https://www.statology.org/shannon-diversity-index/
  #https://www.statology.org/shannon-diversity-index-calculator/
  
  # Shannon diversity index (SDI): this is already Shannon.... that extra step by dividing the by S$n is making equitability index based on the link above.
  # so maybe we can make shannon based on BA, VOL and number of trees.... (? can discuss or save all 3 and will see...)
  
  H.BA<-t %>% group_by(year) %>% summarize(H=-1*sum(prop.BA*log(prop.BA)))
  H.VOL<-t %>% group_by(year) %>% summarize(H=-1*sum(prop.VOL*log(prop.VOL)))
  H.count<-t %>% group_by(year) %>% summarize(H=-1*sum(prop.count*log(prop.count)))   # here I just put the proportion of number of trees
  set.panel(3,1)
  plot(H.BA)
  plot(H.VOL)
  plot(H.count)
  
  #The higher the value of H, the higher the diversity of species in a particular community. 
  #The lower the value of H, the lower the diversity. 
  #A value of H = 0 indicates a community that only has one species.
  
  
  # Shannon Equitability Index (SEI).... maybe we do not need this at all.....
  SEI<-H.BA$H/log(S$n)
  SEI[which(is.na(SEI)==T)]<-0
  
  # Pioneer species proportion based on BA
  
  earlyspecs<-c("lade","acps","frex","cabe","bepe","alin","algl","acca","acpl","soau","soar","coav","alvi","potr","poni","ulgl","saca","rops")
  
  early.spec.prop<-t %>% filter(species %in% earlyspecs) %>% summarize(BA=sum(prop.BA))
  
  esp<-data.frame(year=c(1:max(landscape$year)))
  esp<-esp %>% left_join(early.spec.prop, by="year")
  esp$BA[which(is.na(esp$BA)==T)]<-0
  
  #-----------------------------------------------------------------------------
  # Create Shannon on BA heterogeneity
  
  # Calculate the Shannon diversity index
  H_BA_heterogenity <- tree %>%
    group_by(year, species) %>%
    summarize(shannon_ba_heterog = diversity(basalArea, base = exp(1)))
  
  # Print the resulting dataframe
  print(H_BA_heterogenity)
  
  #-----------------------------------------------------------------------------
  # CREATE THE DATA FRAME FOR ADD VARIABLES ABOUT CARBON IN THE FINAL DATA FRAME
  
  ab.tot.c<- data.frame(carbon %>% 
                          group_by(year) %>% 
                          summarise(tot_carbon=sum(stem_c, branch_c, foliage_c, coarseRoot_c, fineRoot_c, regeneration_c, snags_c, snagsOther_c, downedWood_c, litter_c, soil_c)))
  
  
  #-----------------------------------------------------------------------------
  # CREATE A NEW DATA FRAME TO WORK ON THE SPECIFIC VARIABLES NEEDED 
  
  head(landscape)
  head(dynamicstand)
  
  # TO UNDERSTAND THE OPERATORS %>% AND %IN% HAVE TO STUDY THEM IN DATACAMP AND IN DPLER CRAN PACKAGES
  
  ab.lnd.v<- data.frame(landscape %>% 
                          group_by(year) %>% 
                          filter(year>0) %>%
                          summarise(tot_volume=sum(volume_m3),living_c=sum(total_carbon_kg), count_ha=sum(count_ha), tot_ba=sum(basal_area_m2),npp=sum(NPP_kg), LAI=sum(LAI), sapling=sum(cohort_count_ha),growth_m3=sum(gwl_m3)))
  
  
  dynamicstand_1 <-dynamicstand %>% filter(year>0)                            # THIS IS NEEDED FOR MAKE THE DATA FRAME dynamicstand OF THE SAME SIZE OF THE carbon AND carbonflow. WE DID THE SAME FOR THE SELECTED VARIABLES IN LANDSCAPE
  
  # CREATE THE NEW DATA FRAME FOR VARIABLES 
  
  variables <- data.frame(case=case,
                          year=dynamicstand_1$year,
                          h=dynamicstand_1$height_mean,
                          dbh=dynamicstand_1$dbh_mean,
                          age_mean=dynamicstand_1$age_mean,
                          Rh=carbonflow$Rh,
                          NPP=carbonflow$NPP,
                          GPP=carbonflow$GPP,
                          NEP=carbonflow$NEP,
                          H.count=H.count$H,
                          H.VOL=H.VOL$H,
                          H.BA=H.BA$H,
                          SEI=SEI,
                          esp_BA_prop=esp$BA)
  
  # ADD LANDSCAPE VARIABLES AT V DATA FRAME
  
  variables = inner_join(variables, ab.lnd.v, by="year")
  variables = inner_join(variables, ab.tot.c, by="year")
  
  # variables[which(is.na(damage$wind)==TRUE)] <-0                              # FOR MAKE THE na = 0 !!!! "
  
  head(variables)
  
  #-------------------------------------------------------------------------------
  # Create tables for all variables of plot bdv predictors
  # Create the LAI time series
  
  LAI <- landscape %>% 
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
  # Create a data frame with all unique years to not having missing years
  all_years <- data.frame(year = unique(tree$year))
  
  # Perform left join with the summarization result
  tree_10_40 <- all_years %>%
    left_join(
      tree %>%
        group_by(year) %>%
        filter(dbh >= dbh_min, dbh <= dbh_max) %>%
        summarise(tree_10_40 = n()/4),
      by = "year"
    ) %>%
    replace(is.na(.), 0)
  
  # Make the squared one
  tree_10_40_2 <- tree_10_40 %>% 
    group_by(year)%>%
    mutate(tree_10_40_2 = tree_10_40^2)
  
  #-------------------------------------------------------------------------------
  # To define the species to be removed
  unique_sp <- unique(landscape$species)  # alternative unique_plots <- unique(CZ_JH1[,"plotID"])
  
  
  species_to_remove <- c("piab", "pisy", "abal",
                         "lade", "psme", "pini", 
                         "pice")
  
  # Use subset to filter the dataframe
  filtered_broadl <- subset(landscape, !species %in% species_to_remove)
  
  # Sum BA for every broadleaf species in every year
  ba_broadl <- filtered_broadl %>%
    group_by(year) %>%
    summarize(ba_broadl = sum(basal_area_m2)/4)%>%
    select(year,ba_broadl)
  
  #-------------------------------------------------------------------------------
  # Calculate the basal area only of the broadleave with a dbh > 40cm
  
  # Create a data frame with all unique years
  all_years <- data.frame(year = unique(tree$year))
  
  # Perform left join with the summarization result
  broadl_40 <- all_years %>%
    left_join(
      tree %>%
        group_by(year) %>%
        filter(dbh > 40 & !species %in% species_to_remove) %>%
        summarise(broadl_40 = n()/4),
      by = "year"
    ) %>%
    replace(is.na(.), 0)
  
  # Make the squared one
  broadl_40_2 <- broadl_40 %>% 
    group_by(year)%>%
    mutate(broadl_40_2 = broadl_40^2)
  
  #-------------------------------------------------------------------------------
  # Age - comes from the 20% oldest tree avg age
  age <- tree %>%
    group_by(year) %>%
    arrange(desc(age)) %>%                     
    slice_head(prop = 0.2) %>%                 
    summarize(age = mean(age, na.rm = TRUE))   
  
  # Round the age result
  age <- round(age)
  
  # if wants to standardize by 50x50 size 
  #age <- tree %>%
  # group_by(year) %>%
  #sample_frac(0.25) %>%                      # Randomly sample 1/4 of the trees each year
  #arrange(desc(age)) %>%                     # Sort trees by age in descending order within each year
  #slice_head(prop = 0.2) %>%                 # Select the top 20% oldest trees from the sampled subset
  #summarize(age = mean(age, na.rm = TRUE))   # Calculate mean age for the selected trees
  #age <- round(age)
  
  # Age - comes from the avarage age
  stand_age <- tree %>%
    group_by(year) %>%
    summarize(age = mean(age))
  stand_age <- round(stand_age)
  
  # Create a complete sequence of years and fill missing ages with 0
  stand_age <- stand_age %>%
    complete(year = seq(min(year), max(year), by = 1), fill = list(age = 0))
  
  #-------------------------------------------------------------------------------
  # DW volume - USE snags_c divided by 4 and converted into volume 
  # Alternative use the Carbon instead that volume
  
  #-----------------------------------------------------------------------------
  # CREATE THE DATA FRAME FOR ADD VARIABLES ABOUT CARBON IN THE FINAL DATA FRAME
  
  totalC_kgha_iland <- data.frame(carbon %>% 
                                    group_by(year) %>% 
                                    summarise(totalC_kgha_iland=sum(stem_c, branch_c, foliage_c, coarseRoot_c, fineRoot_c, regeneration_c, snags_c, snagsOther_c, downedWood_c, litter_c, soil_c)))
  
  # DW carbon total
  total_DW_C_kgha <- data.frame(carbon %>% 
                                  group_by(year) %>% 
                                  summarise(total_DW_C_kgha=sum(snags_c, snagsOther_c, downedWood_c)))
  
  # DW carbon total without others snags
  total_AGDW_C_kgha <- data.frame(carbon %>% 
                                    group_by(year) %>% 
                                    summarise(total_AGDW_C_kgha=sum(snags_c, snagsOther_c_ag, downedWood_c_ag)))
  
  # Good one
  standing_DW_C <- data.frame(carbon %>% 
                                group_by(year) %>% 
                                summarise(standing_DW_C = sum(snags_c))) 
  
  # Create a new row with manually specified values
  #  new_row_1 <- c(0, 87975.51948)  # Add your values accordingly swdC
  #  new_row_2 <- c(0, 247559.9415)         # Add your values accordingly : original 749+233+225 swdC+other_swdC+yrC
  #  new_row_3 <- c(0, 489678.8983)     # Add your values accordingly to the year 1
  #  new_row_4 <- c(0, 114798.0941)
  # Create new rows with the values from the first row (year 1) of each data frame
  new_row_1 <- c(0, standing_DW_C[1, 2])     # Year 0 row for standing_DW_C
  new_row_2 <- c(0, total_DW_C_kgha[1, 2])   # Year 0 row for total_DW_C_kgha
  new_row_3 <- c(0, totalC_kgha_iland[1, 2]) # Year 0 row for totalC_kgha_iland
  new_row_4 <- c(0, total_AGDW_C_kgha[1, 2]) # Year 0 row for total_AGDW_C_kgha
  
  # Add the new rows at the beginning of each data frame
  standing_DW_C <- rbind(new_row_1, standing_DW_C)
  total_DW_C_kgha <- rbind(new_row_2, total_DW_C_kgha)
  totalC_kgha_iland <- rbind(new_row_3, totalC_kgha_iland)
  total_AGDW_C_kgha <- rbind(new_row_4, total_AGDW_C_kgha)
  
  # Optional: View the modified data frames to confirm
  print(standing_DW_C)
  print(total_DW_C_kgha)
  print(totalC_kgha_iland)
  print(total_AGDW_C_kgha)
  
  #-------------------------------------------------------------------------------
  # Merge the data frames Plot L1_10
  plot_L1_10_df_simul <- bind_cols(age, 
                                   standing_DW_C = standing_DW_C$standing_DW_C,
                                   totalC_kgha_iland = totalC_kgha_iland$totalC_kgha_iland,
                                   total_DW_C_kgha = total_DW_C_kgha$total_DW_C_kgha,
                                   total_AGDW_C_kgha = total_AGDW_C_kgha$total_AGDW_C_kgha,
                                   lai_sim = LAI$LAI,
                                   ba_broadl = ba_broadl$ba_broadl,
                                   tree_10_40 = tree_10_40$tree_10_40,
                                   tree_10_40_2 = tree_10_40_2$tree_10_40_2,
                                   broadl_40 = broadl_40$broadl_40,
                                   broadl_40_2 = broadl_40_2$broadl_40_2)
  
  
  #-------------------------------------------------------------------------------
  # CREATE FUNCTION FOR THE BAYESIAN STATISTICAL MODEL 
  Bayesian_BDV_model_V3 <- plot_L1_10_df_simul %>%
    group_by(year) %>%
    mutate(deadwood = total_AGDW_C_kgha/4)
  
  # SELECT VARIABLES NEEDED
  Bayesian_BDV_model_V3 <- Bayesian_BDV_model_V3 %>%
    group_by(year) %>%
    select(
      age,
      deadwood,
      lai_sim,
      ba_broadl,
      tree_10_40,
      tree_10_40_2,
      broadl_40,
      broadl_40_2)
  
  
  # Function to calculate predictions for each taxon based on the existing data frame
  calculate_predictions <- function(taxon_name, file_path, data_frame) {
    # Read the CSV file for the taxon
    beta_data <- read_csv(file_path)
    
    # Initialize a list to store the prediction results
    fun_list <- vector("list", length = nrow(beta_data))
    
    # Loop through each row of beta_data (for each simulation iteration)
    for (i in 1:nrow(beta_data)) {
      # Extract the coefficients for the current case
      beta0 <- beta_data$beta0[i]
      beta1 <- beta_data$beta1[i]
      beta2 <- beta_data$beta2[i]
      
      # Compute predictions based on the specific taxon
      if (taxon_name == "beetles") {
        fun_list[[i]] <- data_frame %>%
          mutate(
            PRED_RICH_BEETLES = beta0 + beta1 * deadwood + beta2 * ba_broadl
          )
      } else if (taxon_name == "bryophytes") {
        fun_list[[i]] <- data_frame %>%
          mutate(
            PRED_RICH_BRYOPHYTES = beta0 + beta1 * age + beta2 * deadwood
          )
      } else if (taxon_name == "lichens") {
        beta3 <- beta_data$beta3[i]  # For broadl_40 (lichens)
        fun_list[[i]] <- data_frame %>%
          mutate(
            PRED_RICH_LICHENS = beta0 + beta1 * age + beta2 * lai_sim + beta3 * broadl_40
          )
      } else if (taxon_name == "macrofungi") {
        beta3 <- beta_data$beta3[i]  # For ba_broadl
        beta4 <- beta_data$beta4[i]  # For tree_10_40
        fun_list[[i]] <- data_frame %>%
          mutate(
            PRED_RICH_MACROFUNGI = beta0 + beta1 * age + beta2 * deadwood + beta3 * ba_broadl + beta4 * tree_10_40
          )
      } else if (taxon_name == "macrofungi_red") {
        fun_list[[i]] <- data_frame %>%
          mutate(
            PRED_RICH_MACROFUNGI_RED = beta0 + beta1 * deadwood + beta2 * ba_broadl
          )
      } else if (taxon_name == "moths") {
        fun_list[[i]] <- data_frame %>%
          mutate(
            PRED_RICH_MOTHS = beta0 + beta1 * tree_10_40_2 + beta2 * broadl_40
          )
      } else if (taxon_name == "moths_red") {
        fun_list[[i]] <- data_frame %>%
          mutate(
            PRED_RICH_MOTHS_RED = beta0 + beta1 * tree_10_40_2 + beta2 * broadl_40
          )
      }
    }
    
    # Combine results from all iterations into a single dataframe
    all_predictions <- bind_rows(fun_list)
    
    return(all_predictions)
  }
  
  # Apply the function to each taxon and combine the results
  all_results <- list()
  
  for (taxon_name in names(file_paths)) {
    file_path <- file_paths[[taxon_name]]
    
    # Use the existing Bayesian_BDV_model_V3 as the data frame for each taxon
    taxon_results <- calculate_predictions(taxon_name, file_path, Bayesian_BDV_model_V3)
    
    # Append the results to the all_results list
    all_results[[taxon_name]] <- taxon_results
  }
  
  # Combine all taxon results into a single dataframe
  bayesian_results <- bind_cols(all_results)
  
  # SECOND PART OF THE 1000 BETA FUNCTION ASSEMBLAGE
  
  # Function to calculate predictions for each taxon based on the existing data frame
  calculate_predictions <- function(taxon_name, file_path, data_frame) {
    # Read the CSV file for the taxon
    beta_data <- read_csv(file_path)
    
    # Initialize a list to store the prediction results
    fun_list <- vector("list", length = nrow(beta_data))
    
    # Loop through each row of beta_data (for each simulation iteration)
    for (i in 1:nrow(beta_data)) {
      # Extract the coefficients for the current case
      beta0 <- beta_data$beta0[i]
      beta1 <- beta_data$beta1[i]
      beta2 <- beta_data$beta2[i]
      
      # Compute predictions based on the specific taxon
      if (taxon_name == "beetles") {
        fun_list[[i]] <- data_frame %>%
          mutate(
            PRED_RICH_BEETLES = beta0 + beta1 * deadwood + beta2 * ba_broadl
          )
      } else if (taxon_name == "bryophytes") {
        fun_list[[i]] <- data_frame %>%
          mutate(
            PRED_RICH_BRYOPHYTES = beta0 + beta1 * age + beta2 * deadwood
          )
      } else if (taxon_name == "lichens") {
        beta3 <- beta_data$beta3[i]  # For broadl_40 (lichens)
        fun_list[[i]] <- data_frame %>%
          mutate(
            PRED_RICH_LICHENS = beta0 + beta1 * age + beta2 * lai_sim + beta3 * broadl_40
          )
      } else if (taxon_name == "macrofungi") {
        beta3 <- beta_data$beta3[i]  # For ba_broadl
        beta4 <- beta_data$beta4[i]  # For tree_10_40
        fun_list[[i]] <- data_frame %>%
          mutate(
            PRED_RICH_MACROFUNGI = beta0 + beta1 * age + beta2 * deadwood + beta3 * ba_broadl + beta4 * tree_10_40
          )
      } else if (taxon_name == "macrofungi_red") {
        fun_list[[i]] <- data_frame %>%
          mutate(
            PRED_RICH_MACROFUNGI_RED = beta0 + beta1 * deadwood + beta2 * ba_broadl
          )
      } else if (taxon_name == "moths") {
        fun_list[[i]] <- data_frame %>%
          mutate(
            PRED_RICH_MOTHS = beta0 + beta1 * tree_10_40_2 + beta2 * broadl_40
          )
      } else if (taxon_name == "moths_red") {
        fun_list[[i]] <- data_frame %>%
          mutate(
            PRED_RICH_MOTHS_RED = beta0 + beta1 * tree_10_40_2 + beta2 * broadl_40
          )
      }
    }
    
    # Combine results from all iterations into a single dataframe
    all_predictions <- bind_rows(fun_list)
    
    return(all_predictions)
  }
  
  # Apply the function to each taxon and combine the results
  all_results <- list()
  
  for (taxon_name in names(file_paths)) {
    file_path <- file_paths[[taxon_name]]
    
    # Use the existing Bayesian_BDV_model_V3 as the data frame for each taxon
    taxon_results <- calculate_predictions(taxon_name, file_path, Bayesian_BDV_model_V3)
    
    # Append the results to the all_results list
    all_results[[taxon_name]] <- taxon_results
  }
  
  # Combine all taxon results into a single dataframe
  bayesian_results <- bind_cols(all_results)
  
  # --- NEW FUNCTIONALITY FOR MIN/MAX/MEAN ---
  # Calculate summary statistics for each taxon and year
  summary_bayesian_results <- bayesian_results %>%
    pivot_longer(
      cols = starts_with("PRED_RICH_"),  # Include all richness prediction columns
      names_to = "taxon_richness",       # Create a column identifying the taxon
      values_to = "richness_value"       # The corresponding richness value
    ) %>%
    group_by(year...1, taxon_richness) %>%  # Group by year and taxon
    summarize(
      min_value = quantile(richness_value, 0.05, na.rm = TRUE),  # Minimum (5th percentile)
      mean_value = mean(richness_value, na.rm = TRUE),           # Mean
      max_value = quantile(richness_value, 0.95, na.rm = TRUE),  # Maximum (95th percentile)
      .groups = "drop"                                           # Ungroup after summarization
    )
  
  # CREATE THE Y BDV VALUE PREDICTED
  # SELECT VARIABLES NEEDED
  Bayesian_BDV_model_V3 <- Bayesian_BDV_model_V3 %>%
    group_by(year) %>%
    mutate( 
      BRYO_PRED_RICH_97.5 = 11.67784 + 0.02123*age + 0.00006*deadwood, # Updated with beta0 97.5%, beta[1] 97.5%, beta[2] 97.5%
      BRYO_PRED_RICH_50 = 9.71337 + 0.01584*age + 0.00004*deadwood,    # Updated with beta0 50%, beta[1] 50%, beta[2] 50%
      BRYO_PRED_RICH_2.5 = 7.49491 + 0.00269*age + 0.00001*deadwood,   # Updated with beta0 2.5%, beta[1] 2.5%, beta[2] 2.5%
      LICHEN_PRED_RICH_97.5 = 14.39949 + 0.07614*age + (-0.37808)*lai_sim + 0.28592*broadl_40,
      LICHEN_PRED_RICH_50 = 9.82144 + 0.04723*age + (-1.64795)*lai_sim + 0.11608*broadl_40,
      LICHEN_PRED_RICH_2.5 = 5.17480 + 0.01805*age + (-2.98086)*lai_sim + 0.01149*broadl_40,
      MACROFUNGI_PRED_RICH_97.5 = 103.14266 + 0.34920*age + 0.00054*deadwood + 5.67190*ba_broadl + 0.17916*tree_10_40,
      MACROFUNGI_PRED_RICH_50 = 83.52201 + 0.17438*age + 0.00035*deadwood + 3.29810*ba_broadl + 0.08110*tree_10_40,
      MACROFUNGI_RICH_2.5 = 62.10863 + 0.03120*age + 0.00014*deadwood + 0.82859*ba_broadl + 0.01157*tree_10_40,
      MACROFUNGI_RED_PRED_RICH_97.5 = 134.47920 + 0.00116*deadwood + 9.44456*ba_broadl,
      MACROFUNGI_RED_PRED_RICH_50 = 115.57863 + 0.00088*deadwood + 6.18950*ba_broadl,
      MACROFUNGI_RED_RICH_2.5 = 97.41300 + 0.00061*deadwood + 3.06694*ba_broadl,
      BEETLES_PRED_RICH_97.5 = 9.32954 + 0.00003*deadwood + 0.25618*ba_broadl,
      BEETLES_PRED_RICH_50 = 8.55743 + 0.00002*deadwood + 0.11495*ba_broadl,
      BEETLES_RICH_2.5 = 7.78440 + 0.00000*deadwood + 0.01439*ba_broadl,
      MOTHS_PRED_RICH_97.5 = 64.98348 + (-0.00004)*tree_10_40_2 + 0.89673*broadl_40,
      MOTHS_PRED_RICH_50 = 61.44840 + (-0.00018)*tree_10_40_2 + 0.62675*broadl_40,
      MOTHS_RICH_2.5 = 57.88974 + (-0.00033)*tree_10_40_2 + 0.35542*broadl_40,
      MOTHS_RED_PRED_RICH_97.5 = 66.44195 + (-0.00005)*tree_10_40_2 + 0.94178*broadl_40,
      MOTHS_RED_PRED_RICH_50 = 62.49055 + (-0.00019)*tree_10_40_2 + 0.65890*broadl_40,
      MOTHS_RED_RICH_2.5 = 58.73980 + (-0.00035)*tree_10_40_2 + 0.38171*broadl_40,
      BRYO_PRED_RICH_50_beta1 = 0.01584*age,                          # beta[1] 50% for age
      BRYO_PRED_RICH_50_beta2 = 0.00004*deadwood,                      # beta[2] 50% for deadwood
      LICHEN_PRED_RICH_50_beta1 = 0.04723*age,
      LICHEN_PRED_RICH_50_beta2 = -1.64795*lai_sim,
      LICHEN_PRED_RICH_50_beta3 = 0.11608*broadl_40,
      MACROFUNGI_PRED_RICH_50_beta1 = 0.17438*age,
      MACROFUNGI_PRED_RICH_50_beta2 = 0.00035*deadwood,
      MACROFUNGI_PRED_RICH_50_beta3 = 3.29810*ba_broadl,
      MACROFUNGI_PRED_RICH_50_beta4 = 0.08110*tree_10_40,
      MACROFUNGI_RED_PRED_RICH_50_beta1 = 0.00088*deadwood,
      MACROFUNGI_RED_PRED_RICH_50_beta2 = 6.18950*ba_broadl,
      BEETLES_PRED_RICH_50_beta1 = 0.00002*deadwood,
      BEETLES_PRED_RICH_50_beta2 = 0.11495*ba_broadl,
      MOTHS_PRED_RICH_50_beta1 = -0.00018*tree_10_40_2,
      MOTHS_PRED_RICH_50_beta2 = 0.62675*broadl_40,
      MOTHS_RED_PRED_RICH_50_beta1 = -0.00019*tree_10_40_2,
      MOTHS_RED_PRED_RICH_50_beta2 = 0.65890*broadl_40
    )
  
  #-----------------------------------------------------------------------------
  # CREATE A COLUMN WITH CUMULATIVE MORTALITY IN VOLUME
  
  landscape_removed_natmor <- landscape_removed %>%
    filter(reason == "N") %>%
    group_by(species) %>%
    arrange(year) %>%  # Ensure the data is ordered by year before calculating cumulative sum
    mutate(cumm_mortality_total_carbon = cumsum(total_carbon)) %>%
    ungroup()
  
  
  #-----------------------------------------------------------------------------
  # CREATE THE CALCULATION FOR DAMAGES LOOK LINE 370
  
  landscape.area<-landscape$area[1]                                             # CREATE THE VARIABLE FOR LANDSCAPE AREA          
  
  lnd_volume = landscape %>% group_by(year)  %>%                                # CREATE THE SUMMARIZATION OF THE SPECIES VOLUME PROPORTION TO CREATE A TOTAL LANDSCAPE VOLUME
    summarise(tot_vol = sum(volume_m3),
              .groups = 'drop')
  
  head(lnd_volume)                                                              
  
  # wind and barkbeetle merging:
  
  #head(barkbeetle)
  # head(wind)
  
  
  #damage <- data.frame(year=barkbeetle$year,                                      # CREATE THE DATA FRAME FOR FOR DAMAGE OF BARKBEETLE
  #                 barkbeetle=barkbeetle$killedVolume, 
  #                case=case)
  
  # ADD WIND IMPACT IN THE DAMAGE DATA FRAME
  # damage<-left_join(damage,wind[,c(1,8)],by=("year"))                           # LEFT_JOIN IS A FUNCTION TO JOIN A VARIABLE IN THIS CASE COLUMN 1 AND 2 AND MANY ROWS BASE ON YEAR VARIABLE NUMBER OF ROWS
  # damage<-left_join(damage,lnd_volume,by=("year"))                              # ADD THE LANDSCAPE VOLUME IN THE DAMAGE DATA FRAME
  # colnames(damage)<-c("year","barkbeetle","case","wind","volume")               # GIVE THE NAME AT EVERY VARIABLE
  
  # damage$wind[which(is.na(damage$wind)==TRUE)] <-0                            # FOR MAKE THE na = 0 !!!! "
  
  # head(damage)
  
  #-----------------------------------------------------------------------------
  # Make the 3 categories of removals:
  
  activity.names<-unique(abeStandRemoval$activity)                              # here I list all different type of activities
  
  swcuts<- grepl("sw",activity.names)                                           # I look for only which has "sw" this grepl gives TRUE/FALSE
  activity.names.sw<-activity.names[swcuts]                                     # collect the activity names with sw
  activity.names.notsw<-activity.names[!swcuts]                                 # collect the activity names withOUT sw
  
  #print(activity.names.sw)
  #print(activity.names.notsw)
  
  # Here I filter only the listed activity names and calculate thinning/finalcut values for every year 
  # (each line is per ha for a stand, so I scale with the area, sum up all the harvest on the landscape and then divide it with the whole area to get again per ha)
  
  # TO UNDERSTAND THE OPERATORS %>% AND %IN% HAVE TO STUDY THEM IN DATACAMP AND IN DPLER CRAN PACKAGES
  
  ab.regcuts<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.sw)    %>% 
                            group_by(year)   %>%   summarise(volume=sum(volumeThinning*area)/landscape.area, type="regcut", run=case))
  
  ab.finalcuts<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.sw)    %>% 
                              group_by(year)   %>%   summarise(volume=sum(volumeFinal*area)/landscape.area, type="finalcut", run=case))
  
  ab.thinnig<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.notsw)    %>% 
                            group_by(year)   %>%   summarise(volume=sum(volumeThinning*area)/landscape.area, type="thinning", run=case))
  
  ab.salvaged<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.sw)    %>% 
                             group_by(year)   %>%   summarise(volume=sum(volumeSalvaged*area)/landscape.area, type="salvager", run=case))
  
  # CREATE A FOR CYCLE INTO THE FOR CYCLE
  if (length(activity.names[swcuts])==0) {
    
    ab.finalcuts<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.notsw)    %>% 
                                group_by(year)   %>%   summarise(volume=sum(volumeFinal*area)/landscape.area, type="finalcut", run=case))
    
  }                                                                             # CLOSE THE litte FOR CYCLE
  
  
  # Collect abeUnit data FOR CREATE THE VARIABLE ABEUNIT FOR ALL THE RUNS
  abeStand<-(abeStand %>% mutate(run=case))
  abeStand_scen<-rbind(abeStand_scen, abeStand)
  
  # Collect abeUnit data FOR CREATE THE VARIABLE ABEUNIT FOR ALL THE RUNS
  abeStandDetail<-(abeStandDetail %>% mutate(run=case))
  abeStandDetail_scen<-rbind(abeStandDetail_scen, abeStandDetail)
  
  # Collect abeUnit data FOR CREATE THE VARIABLE ABEUNIT FOR ALL THE RUNS
  abeStandRemoval <-(abeStandRemoval %>% mutate(run=case))
  abeStandRemoval_scen<-rbind(abeStandRemoval_scen, abeStandRemoval)
  
  # Collect abeUnit data FOR CREATE THE VARIABLE ABEUNIT FOR ALL THE RUNS
  abeUnit<-(abeUnit %>% mutate(run=case))
  abeUnit_scen<-rbind(abeUnit_scen, abeUnit)
  
  # Collect abeUnit data FOR CREATE THE VARIABLE ABEUNIT FOR ALL THE RUNS
  landscape_removed <-(landscape_removed %>% mutate(run=case))
  landscape_removed_scen<-rbind(landscape_removed_scen, landscape_removed)
  
  # Collect landscape data FOR CREATE THE VARIABLE LND FOR ALL THE RUNS
  landscape <- (landscape %>% mutate(run=case))
  lnd_scen <- rbind(lnd_scen, landscape)
  
  # Collect dynamicstand data FOR CREATE THE VARIABLE WIND FOR ALL THE RUNS
  dynamicstand <-(dynamicstand %>% mutate(run=case))
  dys_scen <-rbind(dys_scen, dynamicstand)
  
  # Collect dynamicstand data FOR CREATE THE VARIABLE WIND FOR ALL THE RUNS
  tree <-(tree %>% mutate(run=case))
  tree_scen <-rbind(tree_scen, tree)
  
  # Collect dynamicstand data FOR CREATE THE VARIABLE WIND FOR ALL THE RUNS
  stand <-(stand %>% mutate(run=case))
  stand_scen <-rbind(stand_scen, stand)
  
  # Collect dynamicstand data FOR CREATE THE VARIABLE WIND FOR ALL THE RUNS
  carbon <-(carbon %>% mutate(run=case))
  carbon_scen <-rbind(carbon_scen, carbon)
  
  # Collect dynamicstand data FOR CREATE THE VARIABLE WIND FOR ALL THE RUNS
  carbonflow <-(carbonflow %>% mutate(run=case))
  carbonflow_scen <-rbind(carbonflow_scen, carbonflow)
  
  # Collect barkbeetle data FOR CREATE THE VARIABLE BB FOR ALL THE RUNS
  #  barkbeetle <-(barkbeetle %>% mutate(run=case))
  # bb_scen <-rbind(bb_scen, barkbeetle)
  
  # Collect wind data FOR CREATE THE VARIABLE WIND FOR ALL THE RUNS
  # wind <-(wind %>% mutate(run=case))
  # w <-rbind(w, wind)
  
  # CREATE THE VARIABLE FOR THE DIFFERENT WOOD REMOVAL ACTIVITY
  # HERE IT IS NOT NEEDED TO CHANGE CASE TO RUN BECAUSE ALREADY DONE
  removals<-rbind(removals,ab.regcuts,ab.finalcuts,ab.thinnig,ab.salvaged)
  
  # CREATE THE VARIABLE DAMAGE FOR ALL THE RUNS
  # damage <-(damage %>% mutate(run=case))
  #  damage.all<-rbind(damage.all, damage)                                         # PUT ALL THE DAMAGE RUNS INTO A SINGLE DATAFRAME WITH DIFF CASES TO BE PLOT ALL TOGETHER IN LINE 370
  
  # CREATE THE VARIABLES DF FOR ALL THE RUNS
  variables <-(variables %>% mutate(run=case))
  variables.all<-rbind(variables.all, variables) 
  
  # CREATE THE VARIABLE H of BA per species FOR ALL THE RUNS
  H_BA_heterogenity <-(H_BA_heterogenity %>% mutate(run=case))
  H_BA_heterogenity_scen <- rbind(H_BA_heterogenity_scen, H_BA_heterogenity)
  
  # CREATE THE VARIABLE NATURAL MORTALITY FOR ALL THE RUNS
  landscape_removed_natmor <-(landscape_removed_natmor %>% mutate(run=case))
  landscape_removed_scen_natmor <- rbind(landscape_removed_scen_natmor, landscape_removed_natmor)
  
  # CREATE THE VARIABLES NEEDED IN THE BDV STUDY
  plot_L1_10_df_simul <-(plot_L1_10_df_simul %>% mutate(run=case))
  plot_variables_all <- rbind(plot_variables_all, plot_L1_10_df_simul)
  
  # CREATE THE VARIABLES NEEDED IN THE BDV STUDY - Bayesian_BDV_model_bryophytes_V2
  Bayesian_BDV_model_V3 <-(Bayesian_BDV_model_V3 %>% mutate(run=case))
  Bayesian_BDV_model_V3_multi <- rbind(Bayesian_BDV_model_V3_multi, Bayesian_BDV_model_V3)
  
  # CREATE THE VARIABLES NEEDED FOR PLOT THE BAYESIAN FUNCTION
  bayesian_results <-(bayesian_results %>% mutate(run=case))
  bayesian_results_all <- rbind(bayesian_results_all, bayesian_results)
  
  # CREATE THE VARIABLES FOR SUMMARY THE BAYESIAN FUNCTION
  summary_bayesian_results <-(summary_bayesian_results %>% mutate(run=case))
  summary_bayesian_results <- rbind(summary_bayesian_results, summary_bayesian_results)
  
} 
