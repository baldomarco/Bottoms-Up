################          MARCO BALDO - 2025.07.04        ######################
################       DATA UPLOADING AND FIRST ANALYSIS   #####################
################       BDV RECOVERY ANALYSIS SESSION 1    ######################
################          ARTICLE BALDO ET AL. 2025       ######################
################################################################################


# Load required packages
#library(ggplot2)
#library(ggpubr)    # For stat_pvalue_manual
library(tidyr)
library(dplyr)
#library(rstatix)   # For Kruskal-Wallis and correlation tests
library(readxl)    # For reading Excel files
library(scales)

## SECOND PART OF THE ANALYSIS WHERE WE CALCULATE THE YEAR OF BDV RECOVERY - 10% OF THE VALUES >= OF THE Q1,Q2 OR Q3 THRESHOLD.
library(readxl)
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(stringr)

# TO UPLOAD THE DATA REQUIRED COMING FROM THE SCRIPT: Output_Visualization_extention_included_Bayesian_calculation_V2_1000beta_mng_missing_values.r
bayesian_results_all <- readRDS("C:/iLand/2023/20230901_Bottoms_Up/outputs/20250116_official/20250116_official/20250704/DB_R_Graph_1961_1990_umng_mng/DB_R_1961_1990_unmng_mng/bayesian_results_all.rds")

# Remove rows where the "run" column contains substring "_mng"
bayesian_results_all <- bayesian_results_all[!grepl("_mng", bayesian_results_all$run), ]

# CREATE A TAXA LIST
# Lista dei taxa da analizzare
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")

# Load the data
BDV_predictors <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/20_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood_tes - Copy.xlsx")

# Remake the names in the table for a better understanding and coding
BDV_predictors <- BDV_predictors %>%
  mutate(forest_cat = recode(forest_cat,
                             "Beech-Oak" = "Native Broadleaves",
                             "Conifer" = "Non-Native Coniferous",
                             "Old-Growth" = "Old-Growth")) %>%
  rename(
    Bryophytes = `Epiphytic / epixilic bryophytes (0.212)`,
    Lichens = `Lichens (0.137)`,
    Macrofungi = `Macrofungi (2.118)`,
    Beetles = `Non-flying beetles (0.053)`,
    Moths = `Moths (0.566)`
  )

# Check
table(BDV_predictors$forest_cat)
glimpse(BDV_predictors)
BDV_predictors

#-------------------------------------------------------------------------------
# Make Old growth forest quartiles for select the 25%, 50% , 75% BDV threshold per Taxa sp richness

# Filter only Old-growth category
BDV_old_growth <- BDV_predictors %>% 
  filter(forest_cat == "Old-Growth")

# Compute quartiles for the 5 taxa in Old-growth
taxa_quartiles_thrashold <- BDV_old_growth %>%
  summarise(across(c(Bryophytes, Lichens, Macrofungi, Beetles, Moths), quantile, probs = c(0.25, 0.5, 0.75), na.rm = TRUE))

# Transpose the result for better readability
taxa_quartiles_thrashold <- as.data.frame(t(taxa_quartiles_thrashold))

# Rename columns for clarity
colnames(taxa_quartiles_thrashold) <- c("Q1", "Median", "Q3")

# Print the quartiles
print(taxa_quartiles_thrashold)

#-------------------------------------------------------------------------------
# Now assign to them the forest_cat

# Extract plotID from run names and detect managed versions
bayesian_results_all <- bayesian_results_all %>%
  mutate(
    plotID = gsub(".*(L\\d+_\\d+).*", "\\1", run), # Extract Lx_xx code
    is_mng = grepl("_mng", run) # Check if "_mng" is in the run name
  )

# Merge with BDV_predictors to get forest_cat
bayesian_results_all <- bayesian_results_all %>%
  left_join(select(BDV_predictors, plotID, forest_cat,management_type), by = "plotID") %>%
  mutate(
    forest_cat = if_else(is_mng, paste0(forest_cat, "_mng"), forest_cat) # if it gives errors no worry is the mutate and mng cases
  )


# Check unique Forest Categorys after update
unique(bayesian_results_all$forest_cat)

#-------------------------------------------------------------------------------
# NOW THE MAIN PART OF THE WORK - EVALUATE THE PERCENTAGE (PROBABILITY) THAT OUR
# SAMPLING PLOTS (FORESTS) REACH OR EXCEED THE OLD GROWTH FOREST TAXA SPECIES RICHNESS
#-------------------------------------------------------------------------------

# Define quartiles from Old-Growth Forests
taxa_quartiles <- list(
  BRYOPHYTES = c(Q1 = 10.5, Median = 17.5, Q3 = 22.0),
  LICHENS = c(Q1 = 16.25, Median = 18.5, Q3 = 23.75),
  MACROFUNGI = c(Q1 = 162, Median = 192, Q3 = 225),
  BEETLES = c(Q1 = 8.5, Median = 11.5, Q3 = 13.75),
  MOTHS = c(Q1 = 54.25, Median = 62, Q3 = 72.75)
)

# Select only relevant taxa columns
data_filtered <- bayesian_results_all %>%
  select(plotID, year = year, StandAge = StandAge, forest_cat,
         BRYOPHYTES = PRED_RICH_BRYOPHYTES, 
         LICHENS = PRED_RICH_LICHENS, 
         MACROFUNGI = PRED_RICH_MACROFUNGI, 
         BEETLES = PRED_RICH_BEETLES, 
         MOTHS = PRED_RICH_MOTHS)  

# Compute percentages above each quartile
BDV_recovery <- data_filtered %>%
  group_by(plotID, year,forest_cat, StandAge) %>%
  summarise(
    across(c(BRYOPHYTES, LICHENS, MACROFUNGI, BEETLES, MOTHS), 
           list(
             Q1 = ~mean(. >= taxa_quartiles[[cur_column()]]["Q1"], na.rm = TRUE) * 100,
             Median = ~mean(. >= taxa_quartiles[[cur_column()]]["Median"], na.rm = TRUE) * 100,
             Q3 = ~mean(. >= taxa_quartiles[[cur_column()]]["Q3"], na.rm = TRUE) * 100
           ),
           .names = "%_above_{fn}_{col}")
  ) %>%
  ungroup()  # Ensure ungrouped output

# View result
BDV_recovery


#-------------------------------------------------------------------------------
# Let's remove the zero
BDV_recovery_filtered <- BDV_recovery %>% filter(year >= 0 & year <= 266)

BDV_recovery_filtered <- BDV_recovery_filtered %>%
  filter(year >= 0 &  forest_cat %in% c("Native Broadleaves"  ,  "Non-Native Coniferous")) # "Old-Growth"

# For saving (C:\iLand\2023\20230901_Bottoms_Up\outputs\20250116_official\20250116_official\20250704\DB_R_Graph_1961_1990_umng_mng\DB_R_1961_1990_unmng_mng)
saveRDS(BDV_recovery_filtered, file.path(dataroot, "BDV_recovery_filtered.rds")) # VARIABLES NEEDED IN THE BDV STUDY - Bayesian_BDV_model_bryophytes_V2


################################################################################
######################            THE END                 ######################
################################################################################


