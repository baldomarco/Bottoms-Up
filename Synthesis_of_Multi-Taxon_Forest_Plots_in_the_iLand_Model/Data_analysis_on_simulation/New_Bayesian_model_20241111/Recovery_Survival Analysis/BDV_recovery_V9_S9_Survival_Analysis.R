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
bayesian_results_all_1961_1990 <- readRDS("C:/iLand/2023/20230901_Bottoms_Up/outputs/20250116_official/20250116_official/20250704/DB_R_Graph_1961_1990_umng_mng/DB_R_1961_1990_unmng_mng/bayesian_results_all.rds")
bayesian_results_all_1991_2020 <- readRDS("C:/iLand/2023/20230901_Bottoms_Up/outputs/20250116_official/20250620_clim_1991_2020/DB_R_Graph_1991_2020_umng_mng/DB_R_1991_2020_unmng_mng/bayesian_results_all_1991_2020.rds")

# Remove rows where the "run" column contains substring "_mng"
bayesian_results_all_1961_1990 <- bayesian_results_all_1961_1990[!grepl("_mng", bayesian_results_all_1961_1990$run), ]
bayesian_results_all_1991_2020 <- bayesian_results_all_1991_2020[!grepl("_mng", bayesian_results_all_1991_2020$run), ]


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
bayesian_results_all_1961_1990 <- bayesian_results_all_1961_1990 %>%
  mutate(
    plotID = gsub(".*(L\\d+_\\d+).*", "\\1", run), # Extract Lx_xx code
    is_mng = grepl("_mng", run) # Check if "_mng" is in the run name
  )

# Merge with BDV_predictors to get forest_cat
bayesian_results_all_1961_1990 <- bayesian_results_all_1961_1990 %>%
  left_join(select(BDV_predictors, plotID, forest_cat,management_type), by = "plotID") %>%
  mutate(
    forest_cat = if_else(is_mng, paste0(forest_cat, "_mng"), forest_cat) # if it gives errors no worry is the mutate and mng cases
  )


# Check unique Forest Categorys after update
unique(bayesian_results_all_1961_1990$forest_cat)



# Extract plotID from run names and detect managed versions
bayesian_results_all_1991_2020 <- bayesian_results_all_1991_2020 %>%
  mutate(
    plotID = gsub(".*(L\\d+_\\d+).*", "\\1", run), # Extract Lx_xx code
    is_mng = grepl("_mng", run) # Check if "_mng" is in the run name
  )

# Merge with BDV_predictors to get forest_cat
bayesian_results_all_1991_2020 <- bayesian_results_all_1991_2020 %>%
  left_join(select(BDV_predictors, plotID, forest_cat,management_type), by = "plotID") %>%
  mutate(
    forest_cat = if_else(is_mng, paste0(forest_cat, "_mng"), forest_cat) # if it gives errors no worry is the mutate and mng cases
  )


# Check unique Forest Categorys after update
unique(bayesian_results_all_1991_2020$forest_cat)

################################################################################
######################            SECOND SECTION         #######################
################################################################################

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
data_filtered <- bayesian_results_all_1961_1990 %>%
  select(plotID, year = year, StandAge = StandAge, forest_cat,
         BRYOPHYTES = PRED_RICH_BRYOPHYTES, 
         LICHENS = PRED_RICH_LICHENS, 
         MACROFUNGI = PRED_RICH_MACROFUNGI, 
         BEETLES = PRED_RICH_BEETLES, 
         MOTHS = PRED_RICH_MOTHS)  

# Compute percentages above each quartile
BDV_recovery_1961_1990 <- data_filtered %>%
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
BDV_recovery_1961_1990

#-------------
############## CLIMATE SERIES 1991 - 2020

# Select only relevant taxa columns
data_filtered <- bayesian_results_all_1991_2020 %>%
  select(plotID, year = year, StandAge = StandAge, forest_cat,
         BRYOPHYTES = PRED_RICH_BRYOPHYTES, 
         LICHENS = PRED_RICH_LICHENS, 
         MACROFUNGI = PRED_RICH_MACROFUNGI, 
         BEETLES = PRED_RICH_BEETLES, 
         MOTHS = PRED_RICH_MOTHS)  

# Compute percentages above each quartile
BDV_recovery_1991_2020 <- data_filtered %>%
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
BDV_recovery_1991_2020


#-------------------------------------------------------------------------------
# Let's remove the not recovered values = 0
BDV_recovery_1961_1990 <- BDV_recovery_1961_1990 %>% filter(year >= 0 & year <= 350)

BDV_recovery_1961_1990 <- BDV_recovery_1961_1990 %>%
  filter(year >= 0 &  forest_cat %in% c("Native Broadleaves"  ,  "Non-Native Coniferous")) # "Old-Growth"


############## CLIMATE SERIES 1991 - 2020

BDV_recovery_1991_2020 <- BDV_recovery_1991_2020 %>% filter(year >= 0 & year <= 350)

BDV_recovery_1991_2020 <- BDV_recovery_1991_2020 %>%
  filter(year >= 0 &  forest_cat %in% c("Native Broadleaves"  ,  "Non-Native Coniferous")) # "Old-Growth"


# For saving (C:\iLand\2023\20230901_Bottoms_Up\outputs\20250116_official\20250116_official\20250704\DB_R_Graph_1961_1990_umng_mng\DB_R_1961_1990_unmng_mng)
saveRDS(BDV_recovery_1961_1990, file.path(dataroot, "BDV_recovery_1961_1990_350Y.rds")) # VARIABLES NEEDED IN THE BDV STUDY - Bayesian_BDV_model_bryophytes_V2
saveRDS(BDV_recovery_1991_2020, file.path(dataroot, "BDV_recovery_1991_2020_350Y.rds")) # VARIABLES NEEDED IN THE BDV STUDY - Bayesian_BDV_model_bryophytes_V2

# write excel % csv
writexl::write_xlsx(BDV_recovery_1961_1990, "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/BDV_recovery_1961_1990_350Y.xlsx")
readr::write_csv(BDV_recovery_1961_1990, "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/BDV_recovery_1961_1990_350Y.csv")
# write excel % csv
writexl::write_xlsx(BDV_recovery_1991_2020, "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/BDV_recovery_1991_2020_350Y.xlsx")
readr::write_csv(BDV_recovery_1991_2020, "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/BDV_recovery_1991_2020_350Y.csv")


################################################################################
######################            THIRD SECTION         #######################
################################################################################

# ANALYSIS
#------------------
BDV_recovery_data_1961_1990 <- read.csv("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/BDV_recovery_1961_1990_350Y.csv")
BDV_recovery_data_1991_2020 <- read.csv("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/BDV_recovery_1991_2020_350Y.csv")

BDV_recovery_data_1961_1990
BDV_recovery_data_1991_2020

################################################################################
######################      CLIMATE 1990 - 2020           ######################
################################################################################

#------------------
# Initial recovery stage: first row (year == 0) per plot
initial_recovery_stage_1961_1990 <- BDV_recovery_data_1961_1990 %>%
  filter(year == 0) %>%
  group_by(plotID) %>%
  slice(1) %>%
  ungroup()

initial_recovery_stage_1961_1990

# Recovery years (YoR) per plot and taxa-threshold. The YoR will be assigned at every % threshold Taxa richness, so instead then the % reaching the threshold now you will have the year in which at least some recovered.
recovery_years_1961_1990 <- BDV_recovery_data_1961_1990 %>%
  group_by(plotID) %>%
  summarise(
    forest_cat = first(forest_cat),
    StandAge = first(StandAge),
    across(contains("above"), ~ { # Safer: handles any variation in column prefix
      recovery_year <- year[which(. > 0)[1]] # Find first year OF RECOVERY. HERE JUST MAJOR OF 0
      ifelse(is.na(recovery_year), NA, recovery_year) # Assign NA if not found
    }),
    .groups = "drop"
  )

recovery_years_1961_1990
#------------------

recovery_long_1961_1990 <- recovery_years_1961_1990 %>%
  # Rename columns to remove "X._" and replace with "Per_"
  rename_with(~ gsub("^X\\._", "Per_", .x), contains("above")) %>%
  # Pivot to long format
  pivot_longer(
    cols = starts_with("Per_"),
    names_to = "taxa_threshold",
    values_to = "YoR"
  ) %>%
  # Join StandAge from initial_recovery_stage (if needed)
  left_join(initial_recovery_stage_1961_1990 %>% select(plotID, StandAge), by = "plotID") %>%
  # Create unified 'Stand age' column and drop redundant columns if they exist
  mutate(StandAge = coalesce(StandAge.x, StandAge.y)) %>%
  select(-StandAge.x, -StandAge.y)


# write excel needed for the next script
write.csv(recovery_long_1961_1990, 
          "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/recovery_long_1961_1990_350Y.csv", 
          row.names = FALSE)

writexl::write_xlsx(recovery_long_1961_1990, "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/recovery_long_1961_1990_350Y.xlsx")


################################################################################
######################      CLIMATE 1990 - 2020           ######################
################################################################################

#------------------
# Initial recovery stage: first row (year == 0) per plot
initial_recovery_stage_1991_2020 <- BDV_recovery_data_1991_2020 %>%
  filter(year == 0) %>%
  group_by(plotID) %>%
  slice(1) %>%
  ungroup()

initial_recovery_stage_1991_2020

# Recovery years (YoR) per plot and taxa-threshold. The YoR will be assigned at every % threshold Taxa richness, so instead then the % reaching the threshold now you will have the year in which at least some recovered.
recovery_years_1991_2020 <- BDV_recovery_data_1991_2020 %>%
  group_by(plotID) %>%
  summarise(
    forest_cat = first(forest_cat),
    StandAge = first(StandAge),
    across(contains("above"), ~ { # Safer: handles any variation in column prefix
      recovery_year <- year[which(. > 0)[1]] # Find first year OF RECOVERY. HERE JUST MAJOR OF 0
      ifelse(is.na(recovery_year), NA, recovery_year) # Assign NA if not found
    }),
    .groups = "drop"
  )

recovery_years_1991_2020
#------------------

recovery_long_1991_2020 <- recovery_years_1991_2020 %>%
  # Rename columns to remove "X._" and replace with "Per_"
  rename_with(~ gsub("^X\\._", "Per_", .x), contains("above")) %>%
  # Pivot to long format
  pivot_longer(
    cols = starts_with("Per_"),
    names_to = "taxa_threshold",
    values_to = "YoR"
  ) %>%
  # Join StandAge from initial_recovery_stage (if needed)
  left_join(initial_recovery_stage_1991_2020 %>% select(plotID, StandAge), by = "plotID") %>%
  # Create unified 'Stand age' column and drop redundant columns if they exist
  mutate(StandAge = coalesce(StandAge.x, StandAge.y)) %>%
  select(-StandAge.x, -StandAge.y)


# write excel needed for the next script
write.csv(recovery_long_1991_2020, 
          "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/recovery_long_1991_2020_350Y.csv", 
          row.names = FALSE)

writexl::write_xlsx(recovery_long_1991_2020, "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/recovery_long_1991_2020_350Y.xlsx")



################################################################################
######################         FOURTH SECTION            #######################
################################################################################


# Add climate period to both datasets
rec1 <- recovery_long_1961_1990 %>%
  mutate(climate_period = "1961_1990")

rec2 <- recovery_long_1991_2020 %>%
  mutate(climate_period = "1991_2020")

# Combine datasets
recovery_all <- bind_rows(rec1, rec2)

# Filter relevant taxa_thresholds
recovery_filtered <- recovery_all %>%
  filter(
    (str_detect(taxa_threshold, "Median") & !str_detect(taxa_threshold, "MOTHS")) |
      (str_detect(taxa_threshold, "Q3") & str_detect(taxa_threshold, "MOTHS"))
  )

# Create recovery category
recovery_processed <- recovery_filtered %>%
  mutate(
    recovery_status = case_when(
      YoR == 0 ~ "Recovered_before_267",
      YoR < 267 ~ "Recovered_before_267",
      YoR >= 267 & YoR < 350 ~ "Recovered_after_267",
      YoR >= 350 ~ "Not_recovered_350",
      is.na(YoR) ~ "Not_recovered_350"
    )
  )

recovery_wide <- recovery_processed %>%
  mutate(recovery_YoR = ifelse(recovery_status == "Not_recovered_350", NA, YoR)) %>%
  select(plotID, forest_cat, StandAge, taxa_threshold, climate_period, recovery_status, recovery_YoR) %>%
  pivot_wider(
    names_from = c(taxa_threshold, climate_period, recovery_status),
    values_from = recovery_YoR,
    names_sep = "__"
  )



summary_table <- recovery_processed %>%
  group_by(taxa_threshold, climate_period, forest_cat, recovery_status) %>%
  summarise(n_plots = n_distinct(plotID), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = recovery_status,
    values_from = n_plots,
    values_fill = 0
  )


# Step 1: Summarize as before
summary_table_long <- recovery_processed %>%
  group_by(taxa_threshold, forest_cat, climate_period, recovery_status) %>%
  summarise(n_plots = n_distinct(plotID), .groups = "drop")

# Step 2: Pivot to wider format with climate period side-by-side
summary_table_paired <- summary_table_long %>%
  pivot_wider(
    names_from = c(climate_period, recovery_status),
    values_from = n_plots,
    names_sep = "__",
    values_fill = 0
  ) %>%
  arrange(taxa_threshold, forest_cat)


# write excel needed for the next script
write.csv(summary_table_paired, 
          "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/summary_table_paired.csv", 
          row.names = FALSE)

writexl::write_xlsx(summary_table_paired, "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/summary_table_paired.xlsx")

################################################################################
######################      VISUALIZATION SECTION        #######################
################################################################################

################################################################################
# LET'S CLEAN THE NAMES AND MAKE SOME GRAPHICS

summary_cleaned <- summary_table_paired %>%
  mutate(
    taxon = str_extract(taxa_threshold, "[A-Z]+$")  # extract the last all-caps word
  ) %>%
  select(taxon, forest_cat, everything(), -taxa_threshold)

# Pivot longer to make plotting easier
summary_long <- summary_cleaned %>%
  pivot_longer(
    cols = -c(taxon, forest_cat),
    names_to = c("climate_period", "recovery_status"),
    names_sep = "__",
    values_to = "n_plots"
  )

################################################################################
# FIRST GRAPHIC TRY

# Plot
ggplot(summary_long, aes(x = taxon, y = n_plots, fill = recovery_status)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(forest_cat ~ climate_period) +
  labs(
    title = "Recovery by Taxon, Forest Type, and Climate Period",
    x = "Taxon",
    y = "Number of Plots",
    fill = "Recovery Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


################################################################################
# SECOND GRAPHIC TRY

library(patchwork)

# Custom palettes
palette_broad <- c("Recovered_before_267" = "#FDB813",
                   "Recovered_after_267" = "#F7821B",
                   "Not_recovered_350"   = "#D7263D")

palette_conif <- c("Recovered_before_267" = "#A8E6CF",
                   "Recovered_after_267" = "#56C596",
                   "Not_recovered_350"   = "#379683")

# Split the data
broad_df <- summary_long %>% filter(forest_cat == "Native Broadleaves")
conif_df <- summary_long %>% filter(forest_cat == "Non-Native Coniferous")

# Plot for Broadleaves
p1 <- ggplot(broad_df, aes(x = taxon, y = n_plots, fill = recovery_status)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ climate_period) +
  scale_fill_manual(values = palette_broad) +
  coord_cartesian(ylim = c(0, 28)) +
  labs(
    title = "Native Broadleaves",
    x = "Taxon",
    y = "Number of Plots",
    fill = "Recovery Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Plot for Conifers
p2 <- ggplot(conif_df, aes(x = taxon, y = n_plots, fill = recovery_status)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ climate_period) +
  scale_fill_manual(values = palette_conif) +
  coord_cartesian(ylim = c(0, 52)) +
  labs(
    title = "Non-Native Coniferous",
    x = "Taxon",
    y = "Number of Plots",
    fill = "Recovery Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Combine the two plots
p1 / p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")


################################################################################
# THIRD GRAPHIC TRY

# Custom palettes
palette_broad <- c("Recovered_before_267" = "#FDB813",
                   "Recovered_after_267" = "#F7821B",
                   "Not_recovered_350"   = "#D7263D")

palette_conif <- c("Recovered_before_267" = "#A8E6CF",
                   "Recovered_after_267" = "#56C596",
                   "Not_recovered_350"   = "#379683")

# Combine palette (you can just pick one, assuming recovery_status values are shared across both types)
custom_palette <- palette_broad  # Or palette_conif – depending on what you prefer

# Rename climate periods
summary_long <- summary_long %>%
  mutate(climate_period = recode(climate_period,
                                 "1961_1990" = "Climate Series 1961–1990",
                                 "1991_2020" = "Climate Series 1991–2020"
  ))

# Plot
ggplot(summary_long, aes(x = taxon, y = n_plots, fill = recovery_status)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(forest_cat ~ climate_period) +
  scale_fill_manual(values = custom_palette) +
  coord_cartesian(ylim = c(0, 52)) +  # Max needed for conifers
  labs(
    title = "Recovery by Taxon, Forest Type, and Climate Period",
    x = "Taxon",
    y = "Number of Plots",
    fill = "Recovery Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

################################################################################
# 4TH GRAPHIC TRY

library(patchwork)

# Custom palettes
palette_broad <- c("Recovered_before_267" = "#FDB813",
                   "Recovered_after_267" = "#F7821B",
                   "Not_recovered_350"   = "#D7263D")

palette_conif <- c("Recovered_before_267" = "#A8E6CF",
                   "Recovered_after_267" = "#C0FF3E",
                   "Not_recovered_350"   = "#556B2F")

# Split the data
broad_df <- summary_long %>% filter(forest_cat == "Native Broadleaves")
conif_df <- summary_long %>% filter(forest_cat == "Non-Native Coniferous")

# Plot for Broadleaves
p1 <- ggplot(broad_df, aes(x = taxon, y = n_plots, fill = recovery_status)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ climate_period) +
  scale_fill_manual(values = palette_broad) +
  coord_cartesian(ylim = c(0, 28)) +
  labs(
    title = "Native Broadleaves",
    x = "Taxon",
    y = "Number of Plots",
    fill = "Recovery Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Plot for Conifers
p2 <- ggplot(conif_df, aes(x = taxon, y = n_plots, fill = recovery_status)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ climate_period) +
  scale_fill_manual(values = palette_conif) +
  coord_cartesian(ylim = c(0, 52)) +
  labs(
    title = "Non-Native Coniferous",
    x = "Taxon",
    y = "Number of Plots",
    fill = "Recovery Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Combine the two plots
p1 / p2 + plot_layout(guides = "collect") & theme(legend.position = "right")



################################################################################
# 5TH GRAPHIC TRY

# Assumendo che il dataframe 'summary_long' sia già caricato e processato.
# Se non lo è, assicurati che contenga le colonne:
# 'climate_period', 'forest_cat', 'taxon', 'n_plots', e 'recovery_status'.

# Rinomina i periodi climatici per una migliore leggibilità
summary_long <- summary_long %>%
  mutate(climate_period = recode(climate_period,
                                 "1961_1990" = "Climate Series 1961–1990",
                                 "1991_2020" = "Climate Series 1991–2020"
  ))

# --- Definisci le palette di colori specifiche ---
# Palette per le latifoglie (Broadleaves)
palette_broad <- c("Recovered_before_267" = "#FDB813",
                   "Recovered_after_267" = "#F7821B",
                   "Not_recovered_350"   = "#D7263D")

# Palette per le conifere (Coniferous)
palette_conif <- c("Recovered_before_267" = "#A8E6CF",
                   "Recovered_after_267" = "#C0FF3E",
                   "Not_recovered_350"   = "#556B2F")

# --- Crea una nuova variabile per l'estetica 'fill' che combini recovery_status e forest_cat ---
summary_long <- summary_long %>%
  mutate(fill_group = paste(recovery_status, forest_cat, sep = "_"))

# --- Crea una palette master per la nuova variabile 'fill_group' ---
master_palette <- c(
  "Recovered_before_267_Native Broadleaves" = "#FDB813",
  "Recovered_after_267_Native Broadleaves" = "#F7821B",
  "Not_recovered_350_Native Broadleaves"   = "#D7263D",
  "Recovered_before_267_Non-Native Coniferous" = "#A8E6CF",
  "Recovered_after_267_Non-Native Coniferous" = "#C0FF3E",
  "Not_recovered_350_Non-Native Coniferous"   = "#556B2F"
)

# --- Definisci l'ordine dei livelli di recovery_status per uno stacking consistente nelle barre ---
# MODIFICA: Invertito l'ordine per correggere lo stacking se appariva invertito.
recovery_status_levels <- c("Not_recovered_350", "Recovered_after_267", "Recovered_before_267")
summary_long$recovery_status <- factor(summary_long$recovery_status, levels = recovery_status_levels)

# Definisci l'ordine dei livelli di fill_group per assicurare un ordine consistente nella legenda
# Questo ordine deve riflettere l'ordine di stacking desiderato per ciascuna categoria forestale.
fill_group_levels_ordered <- c(
  "Not_recovered_350_Native Broadleaves",
  "Recovered_after_267_Native Broadleaves",
  "Recovered_before_267_Native Broadleaves",
  "Not_recovered_350_Non-Native Coniferous",
  "Recovered_after_267_Non-Native Coniferous",
  "Recovered_before_267_Non-Native Coniferous"
)
summary_long$fill_group <- factor(summary_long$fill_group, levels = fill_group_levels_ordered)

# --- Crea un vettore di etichette nominato per la legenda in inglese ---
legend_labels_named <- c(
  "Recovered_before_267_Native Broadleaves" = "Broadleaves: Recovered before 267",
  "Recovered_after_267_Native Broadleaves" = "Broadleaves: Recovered after 267",
  "Not_recovered_350_Native Broadleaves"   = "Broadleaves: Not recovered 350",
  "Recovered_before_267_Non-Native Coniferous" = "Coniferous: Recovered before 267",
  "Recovered_after_267_Non-Native Coniferous" = "Coniferous: Recovered after 267",
  "Not_recovered_350_Non-Native Coniferous"   = "Coniferous: Not recovered 350"
)

# --- Crea il grafico combinato ---
ggplot(summary_long, aes(x = taxon, y = n_plots, fill = fill_group)) +
  geom_bar(stat = "identity", position = "stack") +
  # Usa facet_grid per creare righe per forest_cat e colonne per climate_period
  # MODIFICA: Aggiunto scales = "free_y" per limiti Y indipendenti per riga
  facet_grid(forest_cat ~ climate_period, scales = "free_y") +
  # MODIFICA: Aggiunto expand_limits per garantire che l'asse Y raggiunga i massimi desiderati
  # Questo aggiunge implicitamente punti per estendere gli assi y se i dati reali non li raggiungono
  scale_fill_manual(
    values = master_palette,
    name = "Recovery Status", # Titolo della legenda in inglese
    labels = legend_labels_named # Etichette della legenda in inglese
  ) +
  labs(
    title = "Recovery by Taxon, Forest Type, and Climate Period",
    x = "Taxon",
    y = "Number of Plots"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(face = "bold"), # I nomi dei pannelli saranno in inglese dai dati stessi
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

################################################################################
# 6TH GRAPHIC TRY

# Assumendo che il dataframe 'summary_long' sia già caricato e processato.
# Se non lo è, assicurati che contenga le colonne:
# 'climate_period', 'forest_cat', 'taxon', 'n_plots', e 'recovery_status'.

# Rinomina i periodi climatici per una migliore leggibilità
summary_long <- summary_long %>%
  mutate(climate_period = recode(climate_period,
                                 "1961_1990" = "Climate Series 1961–1990",
                                 "1991_2020" = "Climate Series 1991–2020"
  ))

# --- Definisci le palette di colori specifiche ---
# Useremo solo la palette per le latifoglie (Broadleaves)
palette_broad <- c("Recovered_before_267" = "#FDB813",
                   "Recovered_after_267" = "#F7821B",
                   "Not_recovered_350"   = "#D7263D")

# Useremo solo la palette per le latifoglie (Broadleaves)
palette_broad <- c("Recovered_before_267" = "#FDB813",
                   "Recovered_after_267" = "#F7821B",
                   "Not_recovered_350"   = "#D7263D")
# Palette per le conifere (Coniferous)
palette_broad <- c("Recovered_before_267" = "#556B2F",
                   "Recovered_after_267" = "#C0FF3E",
                   "Not_recovered_350"   = "#D7263D")

palette_broad <- c("Recovered_before_267" = "#4CAF50", # A vibrant green for "Recovered_before_267" (complete/early recovery)
                   "Recovered_after_267" = "#FFC107", # A warm yellow for "Recovered_after_267" (later recovery)
                   "Not_recovered_350"   = "#E53935") # A distinctive red for "Not_recovered_350" (not recovered)


## COLOR BLIND PEOPLE

palette_broad <- c("Recovered_before_267" = "#0072B2", # A strong blue (often good for green/red deficiencies)
                   "Recovered_after_267" = "#E69F00", # A muted orange/gold
                   "Not_recovered_350"   = "#D55E00") # A reddish-orange (distinct from the blue/gold)

# Dark Purple/Magenta for "Not_recovered_350"
palette_broad <- c("Recovered_before_267" = "#0077B6", # A clear, vibrant blue
                   "Recovered_after_267" = "#00B4D8", # A lighter, distinct teal/cyan
                   "Not_recovered_350"   = "#7B2C8B") # A deep purple/magenta, clearly distinct from blues

# Palette per le conifere (Coniferous)
palette_broad <- c("Recovered_before_267" = "#556B2F",
                   "Recovered_after_267" = "#C0FF3E",
                   "Not_recovered_350"   = "#D7263D")

# --- Definisci l'ordine dei livelli di recovery_status per uno stacking consistente nelle barre ---
# Questo ordine determina come le sezioni delle barre impilate appariranno
recovery_status_levels <- c("Not_recovered_350", "Recovered_after_267", "Recovered_before_267")
summary_long$recovery_status <- factor(summary_long$recovery_status, levels = recovery_status_levels)

# --- Crea il grafico combinato ---
ggplot(summary_long, aes(x = taxon, y = n_plots, fill = recovery_status)) + # Fill by recovery_status directly
  geom_bar(stat = "identity", position = "stack") +
  # Usa facet_grid per creare righe per forest_cat e colonne per climate_period
  # 'scales = "free_y"' permette a ciascuna riga di avere il proprio limite Y
  facet_grid(forest_cat ~ climate_period, scales = "free_y") +
  # Imposta i limiti dell'asse Y per ciascuna categoria forestale
  # Broadleaves avrà un max di 28, Coniferous un max di 52.
  # Poiché scales="free_y", ggplot imposterà automaticamente il max per ogni facet
  # basandosi sui dati e su qualsiasi expand_limits o coord_cartesian applicato.
  # Per forzare i limiti specifici, possiamo usare `after_stat` in geom_bar
  # o manipolare i dati per aggiungere righe fittizie per i limiti,
  # ma con 'free_y' e i tuoi dati, i limiti si adatteranno.
  # Se vuoi forzare un limite superiore di 28 per Broadleaves e 52 per Coniferous,
  # pur mantenendo 'free_y', è più robusto usare `coord_cartesian` condizionalmente
  # o assicurarsi che i dati non superino questi valori.
  # Per semplicità e dato che hai specificato i massimi, useremo `expand_limits`
# che assicura che il range sia coperto, e `free_y` lo adatterà al massimo di ogni facet.
  scale_fill_manual(
    values = palette_broad, # Usa solo la palette_broad per tutti i colori
    name = "Recovery Status" # Titolo della legenda in inglese
    # Non è necessario 'labels' se i nomi di recovery_status sono già descrittivi
  ) +
  labs(
    title = "Recovery by Taxon, Forest Type, and Climate Period",
    x = "Taxon",
    y = "Number of Plots"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(face = "bold"), # I nomi dei pannelli saranno in inglese dai dati stessi
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    legend.position = "right" # MODIFICA: Rimuove la legenda
  )


################################################################################
######################            THE END                 ######################
################################################################################


################################################################################
######################     NEW FOURTH SECTION            #######################
################################################################################

# --- Add climate period to both datasets and rename for readability ---
rec1 <- recovery_long_1961_1990 %>%
  mutate(climate_period = "Climate Series 1961-1990")

rec2 <- recovery_long_1991_2020 %>%
  mutate(climate_period = "Climate Series 1991-2020")

# --- Combine datasets ---
recovery_all <- bind_rows(rec1, rec2)

# --- Filter relevant taxa_thresholds and assign clean taxon names directly ---
recovery_filtered <- recovery_all %>%
  filter(
    (str_detect(taxa_threshold, "Median") & !str_detect(taxa_threshold, "MOTHS")) |
      (str_detect(taxa_threshold, "Q3") & str_detect(taxa_threshold, "MOTHS"))
  ) %>%
  mutate(
    taxon = case_when(
      str_detect(taxa_threshold, "BEETLES") ~ "BEETLES",
      str_detect(taxa_threshold, "BRYOPHYTES") ~ "BRYOPHYTES",
      str_detect(taxa_threshold, "LICHENS") ~ "LICHENS",
      str_detect(taxa_threshold, "MACROFUNGI") ~ "MACROFUNGI",
      str_detect(taxa_threshold, "MOTHS") ~ "MOTHS",
      TRUE ~ taxa_threshold
    )
  )

# --- Create recovery category ---
recovery_processed <- recovery_filtered %>%
  mutate(
    recovery_status = case_when(
      YoR == 0 ~ "Converged before windthrow",
      YoR < 267 ~ "Converged before windthrow",
      YoR >= 267 & YoR < 350 ~ "Converged after windthrow",
      YoR >= 350 ~ "Not Converged",
      is.na(YoR) ~ "Not Converged"
    )
  )

summary_long <- recovery_processed %>%
  group_by(taxon, forest_cat, climate_period, recovery_status) %>%
  summarise(n_plots = n_distinct(plotID), .groups = "drop") %>%
  mutate(
    forest_cat = case_when(
      str_detect(forest_cat, regex("native broadleaves", ignore_case = TRUE)) ~ "Mng broadleaves",
      str_detect(forest_cat, regex("non-native coniferous", ignore_case = TRUE)) ~ "Mng coniferous",
      TRUE ~ forest_cat
    )
  )

# --- Define the order of recovery_status for consistent stacking ---
recovery_status_levels <- c("Not Converged", "Converged after windthrow", "Converged before windthrow")
summary_long$recovery_status <- factor(summary_long$recovery_status, levels = recovery_status_levels)

# --- Define the color palette for the recovery stages ---
palette_recovery <- c(
  "Converged before windthrow" = "#556B2F",
  "Converged after windthrow" = "#C0FF3E",
  "Not Converged" = "#D7263D"
)

# --- Create the combined plot ---
ggplot(summary_long, aes(x = taxon, y = n_plots, fill = recovery_status)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(forest_cat ~ climate_period, scales = "free_y") +
  scale_fill_manual(
    values = palette_recovery,
    name = "Restoration Stage"
  ) +
  labs(
    #title = "Biodiversity Convergence to Old-Growth Forests by Taxon, Climate, and Forest Types",
    x = "Taxon",
    y = "Number of Plots"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    legend.position = "right"
  )
