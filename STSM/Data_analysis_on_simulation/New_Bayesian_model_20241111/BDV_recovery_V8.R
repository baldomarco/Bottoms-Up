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
  select(-c(Lichens_RD1, Macrofungi_RD1, Moths_RD1, `Macrofungi x red-listed (2.652)`, `Moths x red-listed (0.574)`)) %>%
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
    forest_cat = if_else(is_mng, paste0(forest_cat, "_mng"), forest_cat)
  )


# Check unique Forest Categorys after update
unique(bayesian_results_all$forest_cat)

#-------------------------------------------------------------------------------
# NOW THE MAIN PART OF THE WORK - EVALUATE THE PERCENTAGE (PROBABILITY) THAT OUR
# SAMPLING PLOTS (FORESTS) REACH OT EXCEED THE OLD GROWTH FOREST TAXA SPECIES RICHNESS
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
  select(plotID, year = year...1, age = age...2, forest_cat,
         BRYOPHYTES = PRED_RICH_BRYOPHYTES, 
         LICHENS = PRED_RICH_LICHENS, 
         MACROFUNGI = PRED_RICH_MACROFUNGI, 
         BEETLES = PRED_RICH_BEETLES, 
         MOTHS = PRED_RICH_MOTHS)  

# Compute percentages above each quartile
BDV_recovery <- data_filtered %>%
  group_by(plotID, year,forest_cat, age) %>%
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
# Let's remove the management runs

BDV_recovery_filtered <- BDV_recovery %>%
  filter(!grepl("_mng", forest_cat))

#-------------------------------------------------------------------------------
# Let's remove the zero
#BDV_recovery_filtered <- BDV_recovery %>% filter(year >= 0 & year <= 267)

BDV_recovery_filtered <- BDV_recovery_filtered %>%
  filter(year >= 0 & year <= 300 & forest_cat %in% c("Native Broadleaves"  ,  "Non-Native Coniferous")) # "Old-Growth"

#-------------------------------------------------------------------------------
# Example Data (assuming the data is already loaded as 'data_processed')
# You would replace the below example data with your actual data
# data_processed <- your_data

library(ggplot2)
library(gridExtra)

# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
pdf(paste0(dataroot, "BDV_recovery_all_V14.pdf"), height=9, width=16)


# 1' graph
# Funzione per generare e visualizzare i grafici per ogni taxa
plot_taxa <- function(df, taxa) {
  
  # Controllo se il taxa deve usare il terzo quartile invece della mediana
  if (taxa == "MOTHS") {  
    col_raw <- paste0("%_above_Q3_", taxa)
    threshold_label <- "Q3 (Third Quartile)"
  } else {
    col_raw <- paste0("%_above_Median_", taxa)
    threshold_label <- "Median"
  }
  
  # 1️⃣ Calcolare la probabilità normalizzata di superamento della soglia per categoria
  G1 <- df %>%
    # Calcolare per ogni plot e anno se il valore supera la soglia (1 = supera la soglia, 0 = non supera)
    mutate(exceeds_threshold = ifelse(!!sym(col_raw) > 0, 1, 0)) %>%
    group_by(forest_cat, plotID) %>%
    summarise(
      total_years = n_distinct(year),  # Numero di anni distinti per ogni plot
      exceeds_threshold_count = sum(exceeds_threshold, na.rm = TRUE),  # Somma dei superamenti della soglia per plot
      .groups = "drop"
    ) %>%
    # Calcolare la percentuale di anni in cui il plot supera la soglia
    mutate(percent_above_threshold = exceeds_threshold_count / total_years) %>%
    group_by(forest_cat) %>%
    summarise(
      total_plots = n_distinct(plotID),  # Numero di plot distinti per categoria
      average_percent_above_threshold = mean(percent_above_threshold, na.rm = TRUE),  # Media della percentuale sopra la soglia per categoria
      .groups = "drop"
    ) %>%
    # Creare il grafico a barre con probabilità normalizzata
    ggplot(aes(x = forest_cat, y = average_percent_above_threshold, fill = forest_cat)) +
    geom_bar(stat = "identity") +
    labs(title = paste(taxa, "- Normalized Probability of Exceeding", threshold_label),
         x = "Forest Category",
         y = paste("Normalized Probability of Exceeding", threshold_label)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Formattare come %
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 2️⃣ Serie temporale con ribbon (mean, min, max per anno e categoria)
  summary_df <- df %>%
    group_by(year, forest_cat) %>%
    summarise(
      mean_value = mean(!!sym(col_raw), na.rm = TRUE),
      min_value = min(!!sym(col_raw), na.rm = TRUE),
      max_value = max(!!sym(col_raw), na.rm = TRUE),
      .groups = "drop"
    )
  
  G2 <- ggplot(summary_df, aes(x = year, y = mean_value, color = forest_cat, fill = forest_cat)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = min_value, ymax = max_value), alpha = 0.2) +
    labs(title = paste(taxa, "- Predictions Above", threshold_label, "Over Time"),
         x = "Year",
         y = paste("% of Predictions Above", threshold_label)) +
    theme_minimal()
  
  # Mostra i grafici per il taxa corrente
  gridExtra::grid.arrange(G1, G2, ncol = 2)
}

# Lista dei taxa da analizzare
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")

# Generazione e visualizzazione dei grafici separati per ogni taxa
for (taxa in taxa_list) {
  plot_taxa(BDV_recovery_filtered, taxa)
}


# 2' graph
#-------------------------------------------------------------------------------
# Bar plot of `%_above_Median_BRYOPHYTES` by Forest Category

# Function to create bar plots for each threshold (Q1, Median, Q3)
plot_taxa_thresholds <- function(df, taxa) {
  
  # List of thresholds and corresponding labels
  thresholds <- c("Q1", "Median", "Q3")
  
  # Create an empty list to store plots
  plots <- list()
  
  # Loop through each threshold type (Q1, Median, Q3)
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    
    # Define column dynamically
    col_raw <- paste0("%_above_", threshold, "_", taxa)
    
    # Compute the normalized index
    summary_df <- df %>%
      mutate(exceeds_threshold = ifelse(!!sym(col_raw) > 0, 1, 0)) %>%
      group_by(forest_cat, plotID) %>%
      summarise(
        total_years = n_distinct(year),
        exceeds_threshold_count = sum(exceeds_threshold, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(percent_above_threshold = exceeds_threshold_count / total_years) %>%
      group_by(forest_cat) %>%
      summarise(
        total_plots = n_distinct(plotID),
        average_percent_above_threshold = mean(percent_above_threshold, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Create the bar plot
    plots[[i]] <- ggplot(summary_df, aes(x = forest_cat, y = average_percent_above_threshold, fill = forest_cat)) +
      geom_bar(stat = "identity") +
      labs(title = paste(taxa, "-", threshold, "Normalized Probability of Exceeding Threshold"),
           x = "Forest Category",
           y = paste("% Above", threshold, "Normalized")) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Arrange Q1, Median, Q3 plots side by side
  grid.arrange(grobs = plots, ncol = 3)
}

# List of taxa to analyze
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")

# Loop through each taxa and generate plots
for (taxa in taxa_list) {
  plot_taxa_thresholds(BDV_recovery_filtered, taxa)
}

# 3' graph
#-------------------------------------------------------------------------------
# MAKE THE GRAPHS FOR ALL THE TAXA TOGETHER #

# Function to prepare G1 data for all taxa
prepare_G1_data <- function(df, taxa_list) {
  G1_data <- list()
  
  for (taxa in taxa_list) {
    col_raw <- ifelse(taxa == "MOTHS", paste0("%_above_Q3_", taxa), paste0("%_above_Median_", taxa))
    
    data <- df %>%
      mutate(exceeds_threshold = ifelse(!!sym(col_raw) > 0, 1, 0)) %>%
      group_by(forest_cat, plotID) %>%
      summarise(
        total_years = n_distinct(year),
        exceeds_threshold_count = sum(exceeds_threshold, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(percent_above_threshold = exceeds_threshold_count / total_years) %>%
      group_by(forest_cat) %>%
      summarise(
        total_plots = n_distinct(plotID),
        average_percent_above_threshold = mean(percent_above_threshold, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(taxa = taxa)  # Add taxa column
    
    G1_data[[taxa]] <- data
  }
  
  return(bind_rows(G1_data))
}

# Function to prepare G2 data for all taxa
prepare_G2_data <- function(df, taxa_list) {
  G2_data <- list()
  
  for (taxa in taxa_list) {
    col_raw <- ifelse(taxa == "MOTHS", paste0("%_above_Q3_", taxa), paste0("%_above_Median_", taxa))
    
    data <- df %>%
      group_by(year, forest_cat) %>%
      summarise(
        mean_value = mean(!!sym(col_raw), na.rm = TRUE),
        min_value = min(!!sym(col_raw), na.rm = TRUE),
        max_value = max(!!sym(col_raw), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(taxa = taxa)  # Add taxa column
    
    G2_data[[taxa]] <- data
  }
  
  return(bind_rows(G2_data))
}

# List of taxa
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")

# Prepare data
G1_data <- prepare_G1_data(BDV_recovery_filtered, taxa_list)
G2_data <- prepare_G2_data(BDV_recovery_filtered, taxa_list)

# 1️⃣ Plot G1: Bar plots for all taxa together
G1_plot <- ggplot(G1_data, aes(x = forest_cat, y = average_percent_above_threshold, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Normalized Probability of Exceeding Threshold (All Taxa)", 
       x = "Forest Category", 
       y = "Normalized Probability of Exceeding") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom") +
  facet_wrap(~ taxa, nrow = 1)

# 2️⃣ Plot G2: Time series for all taxa together
G2_plot <- ggplot(G2_data, aes(x = year, y = mean_value, color = forest_cat, fill = forest_cat)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = min_value, ymax = max_value), alpha = 0.2) +
  labs(title = "Predictions Above Threshold Over Time (All Taxa)",
       x = "Year",
       y = "% of Predictions Above Threshold") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~ taxa, nrow = 1)

# Display plots separately
print(G1_plot)
print(G2_plot)

# 4' graph
# second version of the 3'

# Function to prepare G1 data for all taxa
prepare_G1_data <- function(df, taxa_list) {
  G1_data <- list()
  
  for (taxa in taxa_list) {
    col_raw <- ifelse(taxa == "MOTHS", paste0("%_above_Q3_", taxa), paste0("%_above_Median_", taxa))
    
    data <- df %>%
      mutate(exceeds_threshold = ifelse(!!sym(col_raw) > 0, 1, 0)) %>%
      group_by(forest_cat, plotID) %>%
      summarise(
        total_years = n_distinct(year),
        exceeds_threshold_count = sum(exceeds_threshold, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(percent_above_threshold = exceeds_threshold_count / total_years) %>%
      group_by(forest_cat) %>%
      summarise(
        total_plots = n_distinct(plotID),
        average_percent_above_threshold = mean(percent_above_threshold, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(taxa = taxa)  # Add taxa column
    
    G1_data[[taxa]] <- data
  }
  
  return(bind_rows(G1_data))
}

# Function to prepare G2 data for all taxa
prepare_G2_data <- function(df, taxa_list) {
  G2_data <- list()
  
  for (taxa in taxa_list) {
    col_raw <- ifelse(taxa == "MOTHS", paste0("%_above_Q3_", taxa), paste0("%_above_Median_", taxa))
    
    data <- df %>%
      group_by(year, forest_cat) %>%
      summarise(
        mean_value = mean(!!sym(col_raw), na.rm = TRUE),
        min_value = min(!!sym(col_raw), na.rm = TRUE),
        max_value = max(!!sym(col_raw), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(taxa = taxa)  # Add taxa column
    
    G2_data[[taxa]] <- data
  }
  
  return(bind_rows(G2_data))
}

# List of taxa
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")

# Prepare data
G1_data <- prepare_G1_data(BDV_recovery_filtered, taxa_list)
G2_data <- prepare_G2_data(BDV_recovery_filtered, taxa_list)

# 1️⃣ Plot G1: Bar plots for all taxa together
G1_plot <- ggplot(G1_data, aes(x = forest_cat, y = average_percent_above_threshold, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Normalized Probability of Exceeding Threshold (All Taxa)", 
       x = "Forest Category", 
       y = "Normalized Probability of Exceeding") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom") +  # Move legend to bottom
  facet_wrap(~ taxa, nrow = 1)

# 2️⃣ Plot G2: Time series for all taxa together, separated by forest category
G2_plot <- ggplot(G2_data, aes(x = year, y = mean_value, color = forest_cat, fill = forest_cat)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = min_value, ymax = max_value), alpha = 0.2) +
  labs(title = "Predictions Above Threshold Over Time (All Taxa)", 
       x = "Year", 
       y = "% of Predictions Above Threshold") +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Move legend to bottom
  facet_grid(taxa ~ forest_cat)  # Separate by taxa (rows) and forest_cat (columns)

# Display plots separately
print(G1_plot)
print(G2_plot)


#-------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gridExtra)


# 🔹 Define fixed colors for Forest Categorys
management_colors <- c(
  "Old-Growth"   = "#1f77b4",  # Blue → Represents conservation and stability
  "Native Broadleaves" = "#ff7f0e",  # Orange → Associated with deciduous trees
  "Non-Native Coniferous"  = "#2ca02c")

management_colors <- c(
  "Old-Growth"   = "#3B9AB2",  # Elegant teal → Stability, conservation  
  "Beech-Oak" = "#E69F00",  # Warm golden amber → Deciduous richness 
  "Conifer"  = "#009E73"  # Burnt orange → Evergreen dominance 
)

management_colors <- c(
  "Old-Growth"   = "#3B9AB2",  # Elegant teal → Stability, conservation  
  "Beech-Oak" = "#D55E00",  # Warm golden amber → Deciduous richness 
  "Conifer"  = "darkolivegreen" 
)

management_colors <- c(
  "Old-Growth"   = "#3B9AB2",  # Elegant teal → Stability, conservation  
  "Native Broadleaves" = "#FF8247",  # Warm golden amber → Deciduous richness  
  "Non-Native Coniferous"  = "darkolivegreen" 
)

# 5' graph
#-------------------------------------------------------------------------------
# FUNZIONE per generare i grafici per un dato taxa e quartile
plot_taxa_quartile <- function(df, taxa, quartile) {
  
  # Nome della colonna da analizzare
  col_name <- paste0("%_above_", quartile, "_", taxa)
  
  # 🔹 Filtra solo gli anni in cui la % è > 0 (tranne per G1)
  df_filtered <- df %>%
    filter(!!sym(col_name) > 0)
  
  # 1️⃣ (G1) Bar plot: Relative % of Years Above 0 per Forest Category
  G1 <- df %>%
    group_by(forest_cat) %>%
    summarise(
      total_years = n(),  # Total possible years in each category
      years_above_0 = sum(!!sym(col_name) > 0)  # Years where % > 0
    ) %>%
    mutate(relative_years_above_0 = years_above_0 / total_years) %>%  # Normalize
    ggplot(aes(x = forest_cat, y = relative_years_above_0, fill = forest_cat)) +
    geom_bar(stat = "identity") +
    labs(title = paste(taxa, quartile, "- Relative Years Above 0"),
         x = "Forest Category",
         y = "Relative % of Years Above 0") +
    scale_fill_manual(values = management_colors) +  # 🔹 Fixed colors
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Show % format
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  # 2️⃣ (G2) Scatter plot: Year vs. % sopra la soglia, colorato per Forest Category
  G2 <- ggplot(df_filtered, aes(x = year, y = !!sym(col_name), color = forest_cat)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "loess", se = FALSE, size = 1.5, span = 0.3) + # Lower span = more wiggly curve
    labs(title = paste(taxa, quartile, "- % Above Threshold per Year"),
         x = "Year",
         y = paste("% Above", quartile)) +
    scale_color_manual(values = management_colors) +  # 🔹 Fixed colors
    theme_minimal()
  
  # 3️⃣ (G3) Boxplot: Distribuzione delle % sopra la soglia per Forest Category
  G3 <- ggplot(df_filtered, aes(x = forest_cat, y = !!sym(col_name), fill = forest_cat)) +
    geom_boxplot(outlier.color = "black", outlier.alpha = 0.1) +
    labs(title = paste(taxa, quartile, "- Distribution of % Above"),
         x = "Forest Category",
         y = paste("% Above", quartile)) +
    scale_fill_manual(values = management_colors) +  # 🔹 Fixed colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 4️⃣ (G4) Boxplot per età in cui si supera la soglia
  G4 <- ggplot(df_filtered, aes(x = forest_cat, y = age, fill = forest_cat)) +
    geom_boxplot(outlier.shape = 1, outlier.color = "gray40", outlier.size = 1, outlier.alpha = 0.3) +
    labs(title = paste(taxa, quartile, "- Age Distribution Above Threshold"),
         x = "Forest Category",
         y = "Age") +
    scale_fill_manual(values = management_colors) +  # 🔹 Fixed colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Mostra i 4 grafici insieme
  grid.arrange(G1, G2, G3, G4, ncol = 2)
}

# 🚀 Esempio per Bryophytes e quartile Median
# plot_taxa_quartile(BDV_recovery_filtered, "BRYOPHYTES", "Median")

# Per generare tutti i grafici per ogni taxa e quartile:
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")
quartiles <- c("Q1", "Median", "Q3")

for (taxa in taxa_list) {
  for (quartile in quartiles) {
    plot_taxa_quartile(BDV_recovery_filtered, taxa, quartile)
  }
}

# 6' graph
#-------------------------------------------------------------------------------
# SECOND VERSION

# Function to generate plots for each taxa (only "Median" or "Q3" for moths)
plot_taxa_filtered <- function(df, taxa) {
  
  # Select correct quartile
  quartile <- ifelse(taxa == "MOTHS", "Q3", "Median")
  
  # Column name
  col_name <- paste0("%_above_", quartile, "_", taxa)
  
  # 🔹 Filter only years where % > 0 (except for G1)
  df_filtered <- df %>%
    filter(!!sym(col_name) > 0)
  
  # 1️⃣ (G1) Bar plot: Relative % of Years Above 0 per Forest Category
  G1 <- df %>%
    group_by(forest_cat) %>%
    summarise(
      total_years = n(),  
      years_above_0 = sum(!!sym(col_name) > 0, na.rm = TRUE)  
    ) %>%
    mutate(relative_years_above_0 = years_above_0 / total_years) %>%  
    ggplot(aes(x = forest_cat, y = relative_years_above_0, fill = forest_cat)) +
    geom_bar(stat = "identity") +
    labs(title = paste(taxa, "-", quartile, "- Relative Years Above 0"),
         x = "Forest Category",
         y = "Relative % of Years Above 0") +
    scale_fill_manual(values = management_colors) +  
    scale_y_continuous(labels = percent_format(accuracy = 1)) +  
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")  # Move legend to bottom
  
  # 2️⃣ (G2) Scatter plot: Year vs. % Above Threshold
  G2 <- ggplot(df_filtered, aes(x = year, y = !!sym(col_name), color = forest_cat)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "loess", se = FALSE, size = 1.5, span = 0.3) +  
    labs(title = paste(taxa, "-", quartile, "- % Above Threshold per Year"),
         x = "Year",
         y = paste("% Above", quartile)) +
    scale_color_manual(values = management_colors) +  
    theme_minimal() +
    theme(legend.position = "none")  # Hide legend to avoid duplication
  
  # 3️⃣ (G3) Boxplot: Distribution of % Above by Forest Category
  G3 <- ggplot(df_filtered, aes(x = forest_cat, y = !!sym(col_name), fill = forest_cat)) +
    geom_boxplot(outlier.color = "black", outlier.alpha = 0.1) +
    labs(title = paste(taxa, "-", quartile, "- Distribution of % Above"),
         x = "Forest Category",
         y = paste("% Above", quartile)) +
    scale_fill_manual(values = management_colors) +  
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")  # Hide legend
  
  # 4️⃣ (G4) Boxplot: Age Distribution Above Threshold
  G4 <- ggplot(df_filtered, aes(x = forest_cat, y = age, fill = forest_cat)) +
    geom_boxplot(outlier.shape = 1, outlier.color = "gray40", outlier.size = 1, outlier.alpha = 0.3) +
    labs(title = paste(taxa, "-", quartile, "- Age Distribution Above Threshold"),
         x = "Forest Category",
         y = "Age") +
    scale_fill_manual(values = management_colors) +  
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")  # Hide legend
  
  # Arrange all 4 plots in a single row
  grid.arrange(G1, G2, G3, G4, ncol = 4)
}

# 🚀 Generate graphs for each taxa (only "Median" or "Q3" for moths)
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")

for (taxa in taxa_list) {
  plot_taxa_filtered(BDV_recovery_filtered, taxa)
}


# 7' graph
#-------------------------------------------------------------------------------

# Funzione per generare un tipo di grafico per tutti i taxa (e visualizzarli su una singola riga)
plot_single_type_all_taxa <- function(df, plot_type) {
  
  taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")
  
  # Lista per raccogliere i grafici
  plot_list <- list()
  
  # Ciclo su tutti i taxa
  for (taxa in taxa_list) {
    
    # Selezioniamo il quartile corretto per i Moths
    quartile <- ifelse(taxa == "MOTHS", "Q3", "Median")
    col_name <- paste0("%_above_", quartile, "_", taxa)
    
    # 🔹 Filtra solo gli anni in cui la % è > 0 (tranne per G1)
    df_filtered <- df %>%
      filter(!!sym(col_name) > 0)
    
    if (plot_type == "bar") {
      # 1️⃣ (G1) Bar plot: Relative % of Years Above 0 per Forest Category
      p <- df %>%
        group_by(forest_cat) %>%
        summarise(
          total_years = n(),  
          years_above_0 = sum(!!sym(col_name) > 0, na.rm = TRUE)  
        ) %>%
        mutate(relative_years_above_0 = years_above_0 / total_years) %>%  
        ggplot(aes(x = forest_cat, y = relative_years_above_0, fill = forest_cat)) +
        geom_bar(stat = "identity") +
        labs(title = paste(taxa), # "-", quartile, "- Relative Years Above 0"),
             x = "Forest Category",
             y = "Relative % of Years Above 0") +
        scale_fill_manual(values = management_colors) +  
        scale_y_continuous(labels = percent_format(accuracy = 1)) +  
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")  # Legenda in basso
      
    } else if (plot_type == "scatter") {
      # 2️⃣ (G2) Scatter plot: Year vs. % Above Threshold
      p <- ggplot(df_filtered, aes(x = year, y = !!sym(col_name), color = forest_cat)) +
        geom_point(alpha = 0.1) +
        geom_smooth(method = "loess", se = FALSE, size = 1.5, span = 0.3) +  
        labs(title = paste(taxa), # "-", quartile, "- % Above Threshold per Year"),
             x = "Year",
             y = paste("% Above", quartile)) +
        scale_color_manual(values = management_colors) +  
        theme_minimal() +
        theme(legend.position = "bottom")  # Legenda in basso
      
    } else if (plot_type == "box") {
      # 3️⃣ (G3) Boxplot: Distribution of % Above by Forest Category
      p <- ggplot(df_filtered, aes(x = forest_cat, y = !!sym(col_name), fill = forest_cat)) +
        geom_boxplot(outlier.color = "black", outlier.alpha = 0.1) +
        labs(title = paste(taxa), # "-", quartile, "- Distribution of % Above"),
             x = "Forest Category",
             y = paste("% Above", quartile)) +
        scale_fill_manual(values = management_colors) +  
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")  # Legenda in basso
      
    } else if (plot_type == "age") {
      # 4️⃣ (G4) Boxplot: Age Distribution Above Threshold
      p <- ggplot(df_filtered, aes(x = forest_cat, y = age, fill = forest_cat)) +
        geom_boxplot(outlier.shape = 1, outlier.color = "gray40", outlier.size = 1, outlier.alpha = 0.3) +
        labs(title = paste(taxa), # "-", quartile, "- Age Distribution Above Threshold"),
             x = "Forest Category",
             y = "Age") +
        scale_fill_manual(values = management_colors) +  
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")  # Legenda in basso
    }
    
    plot_list[[taxa]] <- p
  }
  
  # Uniamo tutti i grafici in un singolo layout a singola riga
  grid.arrange(grobs = plot_list, ncol = 5)  # 5 colonne per 5 taxa
}

# 🚀 Genera un grafico per ogni tipo (bar, scatter, box, age) per tutti i taxa
plot_single_type_all_taxa(BDV_recovery_filtered, "bar")   # Bar plot
plot_single_type_all_taxa(BDV_recovery_filtered, "scatter")  # Scatter plot
plot_single_type_all_taxa(BDV_recovery_filtered, "box")    # Boxplot
plot_single_type_all_taxa(BDV_recovery_filtered, "age")    # Age Distribution

#-------

dev.off()

# write excel
writexl::write_xlsx(BDV_recovery_filtered, "C:/iLand/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/BDV_recovery_data_all_V13_no_old_growth.xlsx")


## SECOND PART OF THE ANALYSIS WHERE WE CALCULATE THE YEAR OF BDV RECOVERY - 10% OF THE VALUES >= OF THE Q1,Q2 OR Q3 THRESHOLD.
library(readxl)
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(stringr)

# ANALYSIS
#------------------
BDV_recovery_data <- read_xlsx("C:/iLand/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/BDV_recovery_data_all_V13_no_old_growth.xlsx")
BDV_recovery_data

#------------------
# 1️⃣ Initial recovery stage: first row (year == 0) per plot
initial_recovery_stage <- BDV_recovery_data %>%
  filter(year == 0) %>%
  group_by(plotID) %>%
  slice(1) %>%
  ungroup()

initial_recovery_stage

# 2️⃣ Recovery years (YoR) per plot and taxa-threshold. The YoR will be assigned at every % threshold Taxa richness, so instead then the % reaching the threshold now you will have the year in which at least 10% recovered.
recovery_years <- BDV_recovery_data %>%
  group_by(plotID) %>%
  summarise(
    forest_cat = first(forest_cat),
    age = first(age),
    across(starts_with("%_above_"), ~ {
      recovery_year <- year[which(. > 0)[1]]  # Find first year OF RECOVERY. HERE JUST MAJOR OF 0
      ifelse(is.na(recovery_year), NA, recovery_year)  # Assign NA if not found
    }),
    .groups = "drop"
  )

recovery_years
#------------------

recovery_long <- recovery_years %>%
  pivot_longer(
    cols = starts_with("%_above_"),
    names_to = "taxa_threshold",
    values_to = "YoR"
  ) %>%
  left_join(initial_recovery_stage %>% select(plotID, age), by = "plotID") %>%
  mutate(age = coalesce(age.x, age.y)) %>%
  select(-age.x, -age.y)

#--------------------
# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
pdf(paste0(dataroot, "BDV_recovery_V8_no_old_growth.pdf"), height=9, width=16)

#--------------------
management_colors <- c(
  "Old-Growth"   = "#3B9AB2",  # Elegant teal → Stability, conservation  
  "Native Broadleaves" = "#FF8247",  # Warm golden amber → Deciduous richness  
  "Non-Native Coniferous"  = "darkolivegreen" 
)

# 8' graph
#-------------------------------------------------------------------------------
# 🏗️ Function to generate 2x3 plots for a given taxa
plot_taxa_overview <- function(df, taxa_name) {
  df_taxa <- df %>%
    filter(str_detect(taxa_threshold, taxa_name))
  
  quartiles <- c("Q1", "Median", "Q3")
  
  # 🔁 Generate plots per quartile
  plots <- lapply(quartiles, function(q) {
    df_q <- df_taxa %>%
      filter(str_detect(taxa_threshold, q))
    
    # 📈 Scatter plot
    scatter <- ggplot(df_q, aes(x = age, y = YoR , color = forest_cat)) +
      geom_point(alpha = 0.7, size = 2.5) +
      ylim(0,NA)+
      geom_smooth(method = "lm", se = FALSE, size = 1.5, span = 0.3) +
      labs(title = paste(taxa_name, "-", q, "Scatter"), x = "age", y = "YoR") +
      scale_color_manual(values = management_colors) +
      theme_minimal() +
      theme(legend.position = "none")
    
    # 📊 Boxplot
    box <- ggplot(df_q, aes(x = forest_cat, y = YoR, fill = forest_cat)) +
      geom_boxplot(outlier.shape = 21, outlier.size = 1.5, outlier.color = "black") +
      labs(title = paste(taxa_name, "-", q, "Boxplot"), x = "Forest Categories", y = "YoR") +
      scale_fill_manual(values = management_colors) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    list(scatter, box)
  })
  
  # 🔗 Arrange plots in a 2x3 grid (scatter on top, box below)
  grid.arrange(
    plots[[1]][[1]], plots[[2]][[1]], plots[[3]][[1]],
    plots[[1]][[2]], plots[[2]][[2]], plots[[3]][[2]],
    ncol = 3,
    top = paste("Taxa Overview:", taxa_name)
  )
}

# 🐛 Generate plots for all taxa
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")
for (taxa in taxa_list) {
  plot_taxa_overview(recovery_long, taxa)
}


# 9' graph
#-------------------------------------------------------------------------------
# LET'S DO THE SAME VISUALIZATION BUT WITH A SCATTER PLOT AND A BOXPLOT OF THE RECOVERY FOR EVERY QUARTILE AND TAXA

# 🏗️ Function to generate 1x2 plots (scatter + boxplot) per taxa and quartile
plot_taxa_quartile_1x2 <- function(df, taxa_name, quartile) {
  df_filtered <- df %>%
    filter(str_detect(taxa_threshold, taxa_name) & str_detect(taxa_threshold, quartile))
  
  # 📈 Scatter plot
  scatter <- ggplot(df_filtered, aes(x = YoR, y = age, color = forest_cat)) +
    geom_point(alpha = 0.7, size = 2.5) +
    labs(title = paste(taxa_name, "-", quartile, "Scatter Plot"), x = "YoR", y = "Age") +
    scale_color_manual(values = management_colors) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # 📊 Boxplot
  box <- ggplot(df_filtered, aes(x = forest_cat, y = YoR, fill = forest_cat)) +
    geom_boxplot(outlier.shape = 21, outlier.size = 1.5, outlier.color = "black") +
    labs(title = paste(taxa_name, "-", quartile, "Boxplot"), x = "Forest Categories", y = "YoR") +
    scale_fill_manual(values = management_colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  # 🔗 Combine scatter + boxplot in 1x2 layout
  grid.arrange(scatter, box, ncol = 2, top = paste("Taxa:", taxa_name, "| Quartile:", quartile))
}

# 🚀 Loop over taxa and quartiles to generate all plots
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")
quartiles <- c("Q1", "Median", "Q3")

for (taxa in taxa_list) {
  for (q in quartiles) {
    plot_taxa_quartile_1x2(recovery_long, taxa, q)
  }
}

# 10' graphs
#-------------------------------------------------------------------------------

# Funzione per generare scatter plot e boxplot separati per tutti i taxa e quartili
plot_taxa_plots <- function(df, taxa_list, quartile) {
  
  # Creare il nome dinamico per il filtro in base al taxa e quartile
  col_name <- paste0("%_above_", quartile, "_", taxa_list)  # Formato: %_above_Q3_MOTHS o %_above_Median_BRYOPHYTES
  
  # Filtro i dati per quartile e taxa
  df_filtered <- df %>%
    filter(taxa_threshold == col_name & !is.na(YoR))  # Filtro per quartile e taxa
  
  # Verifica se ci sono dati dopo il filtro
  if (nrow(df_filtered) == 0) {
    message(paste("No data available for", taxa_list, "in quartile", quartile))
    return(NULL)
  }
  
  # 📈 Scatter plot con modello lineare per ogni categoria forestale
  scatter <- ggplot(df_filtered, aes(x = age, y = YoR, color = forest_cat)) +
    geom_point(alpha = 0.7, size = 2.5) +
    geom_smooth(method = "lm", se = FALSE, aes(group = forest_cat)) +  # Modello lineare per ogni categoria forestale
    labs(title = paste("Scatter Plot - ", taxa_list, " - ", quartile), x = "Age", y = "Year of Recovery (YoR)") +
    scale_color_manual(values = management_colors) +
    facet_wrap(~forest_cat, ncol = 5) +  # Facet per ogni categoria forestale
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # 📊 Boxplot per tutti i taxa e quartile, per categoria forestale
  box <- ggplot(df_filtered, aes(x = forest_cat, y = YoR, fill = forest_cat)) +
    geom_boxplot(outlier.shape = 21, outlier.size = 1.5, outlier.color = "black") +
    labs(title = paste("Boxplot - ", taxa_list, " - ", quartile), x = "Forest Categories", y = "Year of Recovery (YoR)") +
    scale_fill_manual(values = management_colors) +
    facet_wrap(~forest_cat, ncol = 5) +  # Facet per ogni categoria forestale
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
  
  # 🔗 Mostra i due grafici separati
  grid.arrange(scatter, box, ncol = 1)
}

# 🚀 Genera i grafici per tutti i taxa e quartili (solo Q3 per Moths, Median per gli altri taxa)
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")
quartiles <- c("Median", "Q3")  # Median per tutti i taxa tranne Moths, Q3 per Moths

# Loop per generare il grafico per ogni quartile
for (q in quartiles) {
  for (taxa in taxa_list) {
    plot_taxa_plots(recovery_long, taxa, q)
  }
}


# 11' 
#-------------------------------------------------------------------------------

# 🏗️ Function to generate combined plots for selected quartiles and taxa (scatter and boxplot)
plot_combined_taxa <- function(df, taxa_list, quartiles, min_y_value = 0) {
  
  # Generate the list of data for selected quartiles and taxa
  df_filtered <- df %>%
    mutate(
      taxa = str_extract(taxa_threshold, "(?<=Q1_|Median_|Q3_)[A-Za-z]+"),  # Extract taxa (e.g., BRYOPHYTES)
      quartile = str_extract(taxa_threshold, "Q1|Median|Q3")  # Extract quartile (Q1, Median, Q3)
    ) %>%
    filter(
      (quartile == "Median" & taxa %in% taxa_list) |  # Keep Median for all taxa in the list
        (taxa == "MOTHS" & quartile == "Q3")           # Keep Q3 only for Moths
    )
  
  # Create the scatter plot for each taxa
  scatter_plots <- lapply(taxa_list, function(taxa_name) {
    df_taxa <- df_filtered %>% filter(taxa == taxa_name)
    
    ggplot(df_taxa, aes(x = age, y = YoR, color = forest_cat)) +
      geom_point(alpha = 0.7, size = 2.5) +
      geom_smooth(method = "lm", se = FALSE, size = 1.5, span = 0.7) +
      labs(title = paste(taxa_name, "- Scatter"), x = "Age", y = "YoR") +
      scale_color_manual(values = management_colors) +
      scale_y_continuous(limits = c(min_y_value, NA)) +  # Set min y-value
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Create the boxplot for each taxa
  box_plots <- lapply(taxa_list, function(taxa_name) {
    df_taxa <- df_filtered %>% filter(taxa == taxa_name)
    
    ggplot(df_taxa, aes(x = forest_cat, y = YoR, fill = forest_cat)) +
      geom_boxplot(outlier.shape = 21, outlier.size = 1.5, outlier.color = "black") +
      labs(title = paste(taxa_name, "- Boxplot"), x = "Forest Categories", y = "YoR") +
      scale_fill_manual(values = management_colors) +
      scale_y_continuous(limits = c(min_y_value, NA)) +  # Set min y-value
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  })
  
  # Arrange scatter plots in 1 row and 5 columns
  scatter_grid <- do.call(grid.arrange, c(scatter_plots, nrow = 1, ncol = 5))
  
  # Arrange box plots in 1 row and 5 columns
  box_grid <- do.call(grid.arrange, c(box_plots, nrow = 1, ncol = 5))
  
  # Combine the two grids (boxplots above, scatter plots below)
  combined_grid <- grid.arrange(
    box_grid, 
    scatter_grid, 
    nrow = 2,   # Two rows, boxplots in the first row and scatter plots in the second row
    heights = c(1, 1)  # Equal height for both rows
  )
  
  # Return the combined grid
  return(combined_grid)
}

# 🐛 Define taxa and quartiles for filtering
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")
quartiles <- c("Median", "Q3")  # Median for all taxa except Moths, Q3 for Moths

# 🐛 Generate the combined plots for selected taxa and quartiles with a min y-value of 0
combined_taxa_plot <- plot_combined_taxa(recovery_long, taxa_list, quartiles, min_y_value = 0)

# Print the combined plot
print(combined_taxa_plot)


# 12' graph
#-------------------------------------------------------------------------------

#--------------------
# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
pdf(paste0(dataroot, "BDV_Plausibility_Test_v5.pdf"), height=9, width=16)


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
data_simulated_filtered <- data_simulated_filtered %>%
  filter(!forest_cat %in% c("Native Broadleaves", "Non-Native Coniferous", "Old-Growth_mng"))

data_simulated_filtered <- data_simulated_filtered %>%
  mutate(forest_cat = recode(forest_cat,
                             "Native Broadleaves_mng" = "Native Broadleaves",
                             "Non-Native Coniferous_mng" = "Non-Native Coniferous"))

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

saveRDS(Observed_vs_Simulated, file.path(dataroot, "Observed_vs_Simulated_reduced.rds")) 

# Define management colors
management_colors <- c(
  "Old-Growth" = "#3B9AB2",  # Elegant teal → Stability, conservation  
  "Native Broadleaves" = "#FF8247",  # Warm golden amber → Deciduous richness  
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
df_filtered <- final_data %>% filter(Richness >= 0)

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
write.csv(stats_summary, "BDV_Plausibility_Test_Stats_V6.csv", row.names = FALSE)


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
    diff_old_growth_mng_broad_obs = (`Old-Growth_median_richness` - `Native Broadleaves_median_richness`) / `Native Broadleaves_median_richness` * 100,
    diff_old_growth_mng_conf_obs = (`Old-Growth_median_richness` - `Non-Native Coniferous_median_richness`) / `Non-Native Coniferous_median_richness` * 100
  ) %>%
  select(Taxa, starts_with("diff_"))  # Select only the percentage differences for clarity

# Print the calculated percentage differences
print(observed_diff)

# 4. Calculate the percentage differences for Q1
observed_q1_diff <- observed_wide %>%
  mutate(
    diff_mng_broad_mng_conf_q1_obs = (`Native Broadleaves_q1_richness` - `Non-Native Coniferous_q1_richness`) / `Non-Native Coniferous_q1_richness` * 100,
    diff_old_growth_mng_broad_q1_obs = (`Old-Growth_q1_richness` - `Native Broadleaves_q1_richness`) / `Native Broadleaves_q1_richness` * 100,
    diff_old_growth_mng_conf_q1_obs = (`Old-Growth_q1_richness` - `Non-Native Coniferous_q1_richness`) / `Non-Native Coniferous_q1_richness` * 100
  ) %>%
  select(Taxa, starts_with("diff_"))

# Print the Q1 percentage differences
print(observed_q1_diff)

# 5. Calculate the percentage differences for Q3
observed_q3_diff <- observed_wide %>%
  mutate(
    diff_mng_broad_mng_conf_q3_obs = (`Native Broadleaves_q3_richness` - `Non-Native Coniferous_q3_richness`) / `Non-Native Coniferous_q3_richness` * 100,
    diff_old_growth_mng_broad_q3_obs = (`Old-Growth_q3_richness` - `Native Broadleaves_q3_richness`) / `Native Broadleaves_q3_richness` * 100,
    diff_old_growth_mng_conf_q3_obs = (`Old-Growth_q3_richness` - `Non-Native Coniferous_q3_richness`) / `Non-Native Coniferous_q3_richness` * 100
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
    diff_old_growth_mng_broad_sim = ( `Old-Growth_median_richness`- `Native Broadleaves_median_richness`) / `Native Broadleaves_median_richness` * 100,
    diff_old_growth_mng_conf_sim = ( `Old-Growth_median_richness`-`Non-Native Coniferous_median_richness`) / `Non-Native Coniferous_median_richness` * 100
  ) %>%
  select(Taxa, starts_with("diff_"))  # Select only the percentage differences for clarity

# Print the calculated percentage differences for simulated data
print(simulated_diff)

# Similarly, calculate the percentage differences for Q1 and Q3 (just like we did for the observed data)
# For Q1
simulated_q1_diff <- simulated_wide %>%
  mutate(
    diff_mng_broad_mng_conf_q1_sim = (`Native Broadleaves_q1_richness` - `Non-Native Coniferous_q1_richness`) / `Non-Native Coniferous_q1_richness` * 100,
    diff_old_growth_mng_broad_q1_sim = (`Old-Growth_q1_richness` - `Native Broadleaves_q1_richness`) / `Native Broadleaves_q1_richness` * 100,
    diff_old_growth_mng_conf_q1_sim = (`Old-Growth_q1_richness` - `Non-Native Coniferous_q1_richness`) / `Non-Native Coniferous_q1_richness` * 100
  ) %>%
  select(Taxa, starts_with("diff_"))

# Print the Q1 percentage differences for simulated data
print(simulated_q1_diff)

# Similarly, for Q3
simulated_q3_diff <- simulated_wide %>%
  mutate(
    diff_mng_broad_mng_conf_q3_sim = (`Native Broadleaves_q3_richness` - `Non-Native Coniferous_q3_richness`) / `Non-Native Coniferous_q3_richness` * 100,
    diff_old_growth_mng_broad_q3_sim = (`Old-Growth_q3_richness` - `Native Broadleaves_q3_richness`) / `Native Broadleaves_q3_richness` * 100,
    diff_old_growth_mng_conf_q3_sim = (`Old-Growth_q3_richness` - `Non-Native Coniferous_q3_richness`) / `Non-Native Coniferous_q3_richness` * 100
  ) %>%
  select(Taxa, starts_with("diff_"))

# Print the Q3 percentage differences for simulated data
print(simulated_q3_diff)

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
  "Old-Growth" = "#3B9AB2",  # Elegant teal → Stability, conservation  
  "Native Broadleaves" = "#FF8247",  # Warm golden amber → Deciduous richness  
  "Non-Native Coniferous" = "darkolivegreen"  # Earthy green → Non-native conifers
)

# --- MERGE OBSERVED AND SIMULATED DATA --- #

# 1. Merge observed and simulated summary data for each statistic
merged_data <- observed_wide %>%
  left_join(simulated_wide, by = "Taxa", suffix = c("_obs", "_sim"))

# Save the filtered and transformed data to an Excel file
write_xlsx(merged_data, path = "merged_data_v2.xlsx")

# 2. Merge observed and simulated percentage differences
merged_diff <- observed_diff %>%
  left_join(simulated_diff, by = "Taxa", suffix = c("_obs", "_sim"))

write_xlsx(merged_diff, path = "merged_diff_v2.xlsx")

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
write_xlsx(final_merged_data, path = "final_merged_data.xlsx")

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

saveRDS(obs_sim_data, file.path(dataroot, "obs_sim_data_v2.rds")) 





#-------------------------------------------------------------------------------
# SECOND SECTION ON THE OBSERVATION STATISTICS
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
ggsave("species_richness_boxplot.png", width = 10, height = 6)


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
write_xlsx(merged_diff_full, path = "merged_diff_full.xlsx")

# Create Word document with the differences table
doc <- read_docx()

# Add a title
doc <- doc %>%
  body_add_par("Species Richness Differences Between Observed and Simulated Data", style = "heading 1")

# Add table of percentage differences (diff by quantile)
doc <- doc %>%
  body_add_table(merged_diff_full, style = "table_template")

# Save Word document
print(doc, target = "species_richness_differences.docx")

##################    THE END     ################


