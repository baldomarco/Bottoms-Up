################          MARCO BALDO - 2025.07.04        ######################
################       DATA ANALYSIS AND VISUALIZATION     #####################
################   PLAUSIBILITY TEST ANALYSIS SESSION 6   ######################
################          ARTICLE BALDO ET AL. 2025       ######################
################################################################################


# 12' graph
#-------------------------------------------------------------------------------

#--------------------
# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
pdf(paste0(dataroot, "BDV_Plausibility_Test_v15.pdf"), height=9, width=16)


# Standardize column names in BDV_predictors to match data_filtered
BDV_predictors <- BDV_predictors
colnames(BDV_predictors) <- recode(colnames(BDV_predictors),
                                   "Bryophytes" = "BRYOPHYTES",
                                   "Lichens" = "LICHENS",
                                   "Macrofungi" = "MACROFUNGI",
                                   "Beetles" = "BEETLES",
                                   "Moths" = "MOTHS")

# Assign dataset labels
data_simulated_filtered <- data_filtered 

data_simulated_filtered$Source <- "Simulated"
BDV_predictors$Source <- "Observed"

# Select only necessary columns
data_simulated_filtered <- data_simulated_filtered %>% select(forest_cat, all_of(taxa_list), Source)
BDV_predictors <- BDV_predictors %>% select(forest_cat, all_of(taxa_list), Source)

#Alternative for the plausibility differences compare the observations with simulation filtering out the not comparable forests categories
#data_simulated_filtered <- data_simulated_filtered %>%
#  filter(!forest_cat %in% c("Native Broadleaves", "Non-Native Coniferous", "Old-Growth_mng"))

#data_simulated_filtered <- data_simulated_filtered %>%
#  mutate(forest_cat = recode(forest_cat,
#                             "Native Broadleaves_mng" = "Native Broadleaves",
#                             "Non-Native Coniferous_mng" = "Non-Native Coniferous"))

# REDUCE THE NUMBER BY 10%
set.seed(123)  # Ensures reproducibility
data_simulated_filtered <- data_simulated_filtered %>%
  sample_frac(0.1)

# Replace negative values with 0 in all numerical columns (excluding non-numeric ones)
#data_simulated_filtered <- data_simulated_filtered %>%
#  mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))

# Efficient merging into long format
Observed_vs_Simulated <- bind_rows(
  data_simulated_filtered %>% pivot_longer(cols = all_of(taxa_list), names_to = "Taxa", values_to = "Richness"),
  BDV_predictors %>% pivot_longer(cols = all_of(taxa_list), names_to = "Taxa", values_to = "Richness")
)

saveRDS(Observed_vs_Simulated, file.path(dataroot, "Observed_vs_Simulated_reduced_V15.rds")) 

# Define management colors
management_colors <- c(
  "Old-Growth" = "#3B9AB2",  # Elegant teal ??? Stability, conservation  
  "Native Broadleaves" = "#FF8247",  # Warm golden amber ??? Deciduous richness  
  "Non-Native Coniferous" = "darkolivegreen" 
)

#------------------------------------------------------------------
# This should work to reduce the data samples
set.seed(123)  # Set seed for reproducibility

# Sample 10% of the "Simulated" data
simulated_sample <- Observed_vs_Simulated %>%
  filter(Source == "Simulated") %>%
  sample_frac(0.99)  # Sample 10% of the Simulated data

# Keep all "Observed" data
observed_data <- Observed_vs_Simulated %>%
  filter(Source == "Observed")

# Combine both datasets
final_data <- bind_rows(observed_data, simulated_sample)

# Check the result
head(final_data)
tail(final_data)
#-------------------------------

# update without points

# Generate Boxplots for each taxa (faceted by forest category) without points
ggplot(final_data, aes(x = Source, y = Richness, fill = forest_cat)) +
  geom_boxplot() +  # Boxplot without jitter points
  facet_grid(forest_cat ~ Taxa, scales = "free_y") +  # Rows: Forest category, Columns: Taxa
  labs(title = "Comparison of Observed vs Simulated Species Richness",
       x = "Data Source",
       y = "Species Richness") +
  scale_fill_manual(values = management_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Separated

# Filter out negative richness values
df_filtered <- final_data #%>% filter(Richness >= 0)

# Generate a list of plots, one for each taxa
plots <- lapply(unique(df_filtered$Taxa), function(taxa) {
  ggplot(df_filtered %>% filter(Taxa == taxa), aes(x = Source, y = Richness, fill = forest_cat)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Only boxplot, no points, no outliers shown
    facet_grid(forest_cat ~ ., scales = "free_y") +  # Facet by forest category
    labs(title = paste("Observed vs Simulated Species Richness -", taxa),
         x = "Data Source",
         y = "Species Richness") +
    scale_fill_manual(values = management_colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

# Display all the plots
plots

#-------------------------------------------------------------------------------
# Compute Summary Statistics
stats_summary <- Observed_vs_Simulated %>%
  group_by(Taxa, forest_cat, Source) %>%
  summarise(
    Mean = mean(Richness, na.rm = TRUE),
    Median = median(Richness, na.rm = TRUE),
    Variance = var(Richness, na.rm = TRUE),
    Std_Dev = sd(Richness, na.rm = TRUE),
    Min = min(Richness, na.rm = TRUE),
    Max = max(Richness, na.rm = TRUE),
    .groups = "drop"
  )

# Save summary statistics as CSV
write.csv(stats_summary, "BDV_Plausibility_Test_Stats_V15.csv", row.names = FALSE)


#--------
dev.off()

#################  Here the section on the differences among the forest categories and observation - simulations

# 1. Filter and calculate summary statistics for observed data
observed_data <- Observed_vs_Simulated %>%
  filter(Source == "Observed" & !forest_cat %in% c("Old-Growth_mng")) %>%
  filter(forest_cat %in% c("Native Broadleaves", "Non-Native Coniferous", "Old-Growth")) %>%
  group_by(forest_cat, Taxa) %>%
  summarise(
    median_richness = median(Richness, na.rm = TRUE),
    q1_richness = quantile(Richness, 0.25, na.rm = TRUE),
    q3_richness = quantile(Richness, 0.75, na.rm = TRUE),
    min_richness = min(Richness, na.rm = TRUE),
    max_richness = max(Richness, na.rm = TRUE),
    mean_richness = mean(Richness, na.rm = TRUE)
  ) %>%
  ungroup()

# 2. Pivot the data to wide format (one row per Taxa, with columns for each statistic and forest category)
observed_wide <- observed_data %>%
  pivot_wider(
    names_from = forest_cat,
    values_from = c(median_richness, q1_richness, q3_richness, min_richness, max_richness, mean_richness),
    names_glue = "{forest_cat}_{.value}"
  )

# 3. Calculate the percentage differences between forest categories based on the median
observed_diff <- observed_wide %>%
  mutate(
    diff_mng_broad_mng_conf_obs = (`Native Broadleaves_median_richness` - `Non-Native Coniferous_median_richness`) / `Non-Native Coniferous_median_richness` * 100,
    diff_old_growth_mng_broad_obs = (`Native Broadleaves_median_richness` - `Old-Growth_median_richness` ) / `Old-Growth_median_richness`  * 100,
    diff_old_growth_mng_conf_obs = (`Non-Native Coniferous_median_richness` - `Old-Growth_median_richness` ) / `Old-Growth_median_richness`  * 100
  ) %>%
  select(Taxa, starts_with("diff_"))  # Select only the percentage differences for clarity

# Print the calculated percentage differences
print(observed_diff)

# 4. Calculate the percentage differences for Q1
observed_q1_diff <- observed_wide %>%
  mutate(
    diff_mng_broad_mng_conf_q1_obs = (`Native Broadleaves_q1_richness` - `Non-Native Coniferous_q1_richness`) / `Non-Native Coniferous_q1_richness` * 100,
    diff_old_growth_mng_broad_q1_obs = (`Native Broadleaves_q1_richness` - `Old-Growth_q1_richness`) / `Old-Growth_q1_richness` * 100,
    diff_old_growth_mng_conf_q1_obs = (`Non-Native Coniferous_q1_richness` - `Old-Growth_q1_richness`) / `Old-Growth_q1_richness` * 100
  ) %>%
  select(Taxa, starts_with("diff_"))

# Print the Q1 percentage differences
print(observed_q1_diff)

# 5. Calculate the percentage differences for Q3
observed_q3_diff <- observed_wide %>%
  mutate(
    diff_mng_broad_mng_conf_q3_obs = (`Native Broadleaves_q3_richness` - `Non-Native Coniferous_q3_richness`) / `Non-Native Coniferous_q3_richness` * 100,
    diff_old_growth_mng_broad_q3_obs = (`Native Broadleaves_q3_richness` - `Old-Growth_q3_richness`) / `Old-Growth_q3_richness` * 100,
    diff_old_growth_mng_conf_q3_obs = (`Non-Native Coniferous_q3_richness` - `Old-Growth_q3_richness`) / `Old-Growth_q3_richness` * 100
  ) %>%
  select(Taxa, starts_with("diff_"))

# Print the Q3 percentage differences
print(observed_q3_diff)

############################# SIMULATED

# 1. Filter and calculate summary statistics for simulated data
simulated_data <- Observed_vs_Simulated %>%
  filter(Source == "Simulated" & !forest_cat %in% c("Old-Growth_mng")) %>%
  filter(forest_cat %in% c("Native Broadleaves", "Non-Native Coniferous", "Old-Growth")) %>% # NB the coniferous and broadleaves stands in the simulations are related to the managed simmulations data
  group_by(forest_cat, Taxa) %>%
  summarise(
    median_richness = median(Richness, na.rm = TRUE),
    q1_richness = quantile(Richness, 0.25, na.rm = TRUE),
    q3_richness = quantile(Richness, 0.75, na.rm = TRUE),
    min_richness = min(Richness, na.rm = TRUE),
    max_richness = max(Richness, na.rm = TRUE),
    mean_richness = mean(Richness, na.rm = TRUE)
  ) %>%
  ungroup()

# 2. Pivot the data to wide format for simulated data (one row per Taxa, with columns for each statistic and forest category)
simulated_wide <- simulated_data %>%
  pivot_wider(
    names_from = forest_cat,
    values_from = c(median_richness, q1_richness, q3_richness, min_richness, max_richness, mean_richness),
    names_glue = "{forest_cat}_{.value}"
  )

# 3. Calculate the percentage differences for simulated data based on the median
simulated_diff <- simulated_wide %>%
  mutate(
    diff_mng_broad_mng_conf_sim = (`Native Broadleaves_median_richness` - `Non-Native Coniferous_median_richness` ) / `Non-Native Coniferous_median_richness` * 100,
    diff_old_growth_mng_broad_sim = ( `Native Broadleaves_median_richness` - `Old-Growth_median_richness`) / `Old-Growth_median_richness` * 100,
    diff_old_growth_mng_conf_sim = (`Non-Native Coniferous_median_richness` - `Old-Growth_median_richness`) / `Old-Growth_median_richness` * 100
  ) %>%
  select(Taxa, starts_with("diff_"))  # Select only the percentage differences for clarity

# Print the calculated percentage differences for simulated data
print(simulated_diff)

# Similarly, calculate the percentage differences for Q1 and Q3 (just like we did for the observed data)
# For Q1
simulated_q1_diff <- simulated_wide %>%
  mutate(
    diff_mng_broad_mng_conf_q1_sim = (`Native Broadleaves_q1_richness` - `Non-Native Coniferous_q1_richness`) / `Non-Native Coniferous_q1_richness` * 100,
    diff_old_growth_mng_broad_q1_sim = (`Native Broadleaves_q1_richness` - `Old-Growth_q1_richness` ) / `Old-Growth_q1_richness`  * 100,
    diff_old_growth_mng_conf_q1_sim = (`Non-Native Coniferous_q1_richness` - `Old-Growth_q1_richness` ) / `Old-Growth_q1_richness`  * 100
  ) %>%
  select(Taxa, starts_with("diff_"))

# Print the Q1 percentage differences for simulated data
print(simulated_q1_diff)

# Similarly, for Q3
simulated_q3_diff <- simulated_wide %>%
  mutate(
    diff_mng_broad_mng_conf_q3_sim = (`Native Broadleaves_q3_richness` - `Non-Native Coniferous_q3_richness`) / `Non-Native Coniferous_q3_richness` * 100,
    diff_old_growth_mng_broad_q3_sim = (`Native Broadleaves_q3_richness` - `Old-Growth_q3_richness` ) / `Old-Growth_q3_richness`  * 100,
    diff_old_growth_mng_conf_q3_sim = (`Non-Native Coniferous_q3_richness` - `Old-Growth_q3_richness` ) / `Old-Growth_q3_richness`  * 100
  ) %>%
  select(Taxa, starts_with("diff_"))

# Print the Q3 percentage differences for simulated data
print(simulated_q3_diff)

# THIS IS THE SECTION WITH ONLY THE IMPORTANT GRAPHS

# VISUALIZATION
#-------------------------------------------------------------------------------
# Merging and plot the results
# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(officer)
library(writexl)

# --- Define the color palette --- #
management_colors <- c(
  "Old-Growth" = "#3B9AB2",  # Elegant teal ??? Stability, conservation  
  "Native Broadleaves" = "#FF8247",  # Warm golden amber ??? Deciduous richness  
  "Non-Native Coniferous" = "darkolivegreen"  # Earthy green ??? Non-native conifers
)

# --- MERGE OBSERVED AND SIMULATED DATA --- #

# 1. Merge observed and simulated summary data for each statistic
merged_data <- observed_wide %>%
  left_join(simulated_wide, by = "Taxa", suffix = c("_obs", "_sim"))

# Save the filtered and transformed data to an Excel file
write_xlsx(merged_data, path = "merged_data_v15.xlsx")

# 2. Merge observed and simulated percentage differences
merged_diff <- observed_diff %>%
  left_join(simulated_diff, by = "Taxa", suffix = c("_obs", "_sim"))

write_xlsx(merged_diff, path = "merged_diff_v15.xlsx")

# 3. Merge the two together and only for the medians
# --- MERGE OBSERVED & SIMULATED MEDIAN RICHNESS DATA ---
merged_data <- observed_wide %>%
  left_join(simulated_wide, by = "Taxa", suffix = c("_obs", "_sim"))

# --- MERGE OBSERVED & SIMULATED PERCENTAGE DIFFERENCES (ONLY MEDIAN) ---
merged_diff <- observed_diff %>%
  left_join(simulated_diff, by = "Taxa", suffix = c("_obs", "_sim"))

# --- COMBINE BOTH MERGED TABLES INTO ONE FINAL DATAFRAME ---
final_merged_data <- merged_data %>%
  left_join(merged_diff, by = "Taxa")

# --- SAVE THE FINAL MERGED DATA TO EXCEL ---
write_xlsx(final_merged_data, path = "final_merged_data_V15.xlsx")

#-----------------------------------------------
# --- CREATE BOXPLOTS FOR SPECIES RICHNESS --- #

# Filter data for observation and simulation for boxplots
obs_sim_data <- Observed_vs_Simulated %>%
  filter(forest_cat %in% c("Native Broadleaves", "Non-Native Coniferous", "Old-Growth")) %>%
  mutate(forest_cat = case_when(
    forest_cat == "Non-Native Coniferous_mng" ~ "Non-Native Coniferous",
    forest_cat == "Native Broadleaves_mng" ~ "Native Broadleaves",
    TRUE ~ as.character(forest_cat)
  ))

saveRDS(obs_sim_data, file.path(dataroot, "obs_sim_data_v15.rds")) 

#-------------------------------------------------------------------------------
# NORMAL PLOTS WITHOUT SAVING

# --- CREATE BOXPLOTS FOR SPECIES RICHNESS --- #

# Create a plot for each Taxa
obs_sim_data %>%
  distinct(Taxa) %>% # Get unique Taxa
  pull(Taxa) %>%     # Pull them out as a vector
  lapply(function(taxa_name) {
    
    # Filter data for the current taxa
    taxa_data <- obs_sim_data %>%
      filter(Taxa == taxa_name)
    
    # Create the boxplot
    ggplot(taxa_data, aes(x = interaction(forest_cat, Source), y = Richness, fill = forest_cat)) +
      geom_boxplot() +
      scale_fill_manual(values = management_colors) +  # Apply custom color palette
      labs(title = paste("Species Richness -", taxa_name),
           x = "Forest Category & Source",
           y = "Species Richness") +
      theme_minimal() +
      theme(
        legend.title = element_blank(),  # Remove legend title for aesthetics
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for clarity
      )
  })

#-------------------------------------------------------------------------------
# THIS VERSION WILL SAVE PLOTS AND TABLES

# Create boxplot of species richness for all taxa with the defined color palette
ggplot(obs_sim_data, aes(x = interaction(forest_cat, Source), y = Richness, fill = forest_cat)) +
  geom_boxplot() +
  scale_fill_manual(values = management_colors) +  # Apply custom color palette
  facet_wrap(~Taxa) +
  theme_minimal() +
  labs(title = "Species Richness by Forest Category and Taxa",
       x = "Forest Category & Source",
       y = "Species Richness") +
  theme(legend.title = element_blank())  # Remove legend title for aesthetics

# Save the boxplot as an image
ggsave("species_richness_boxplot_V2.png", width = 10, height = 6)


# --- CREATE DIFF TABLE FOR WORD DOCUMENT --- #

# Combine the observed and simulated percentage differences for each quartile (Q1, Q2, Q3)
merged_diff_full <- merged_data %>%
  left_join(observed_q1_diff, by = "Taxa") %>%
  left_join(simulated_q1_diff, by = "Taxa", suffix = c("_obs_q1", "_sim_q1")) %>%
  left_join(observed_diff, by = "Taxa") %>%
  left_join(simulated_diff, by = "Taxa", suffix = c("_obs_q1", "_sim_q1")) %>%
  left_join(observed_q3_diff, by = "Taxa") %>%
  left_join(simulated_q3_diff, by = "Taxa", suffix = c("_obs_q3", "_sim_q3")) %>%
  select(Taxa,
         starts_with("diff_"))  # Include all diff columns for easier readability

# Print merged differences
print(merged_diff_full)

# Save the filtered and transformed data to an Excel file
write_xlsx(merged_diff_full, path = "merged_diff_full_V15.xlsx")

# Create Word document with the differences table
doc <- read_docx()

# Add a title
doc <- doc %>%
  body_add_par("Species Richness Differences Between Observed and Simulated Data", style = "heading 1")

# Add table of percentage differences (diff by quantile)
doc <- doc %>%
  body_add_table(merged_diff_full, style = "table_template")

# Save Word document
print(doc, target = "species_richness_differences_V2.docx")

################################################################################
######################            THE END                 ######################
################################################################################