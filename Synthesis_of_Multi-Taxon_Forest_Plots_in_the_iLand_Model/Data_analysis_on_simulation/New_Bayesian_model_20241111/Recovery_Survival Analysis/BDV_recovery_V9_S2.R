################          MARCO BALDO - 2025.07.04        ######################
################         GRAPHS AND SECOND ANALYSIS        #####################
################       BDV RECOVERY ANALYSIS SESSION 2    ######################
################          ARTICLE BALDO ET AL. 2025       ######################
################################################################################

# This script is related to the normalized probability for the plot to exceed
# the median level of old-growth forest species richness during the unmanaged 
# development in managed forest categories


#-------------------------------------------------------------------------------
# Example Data (assuming the data is already loaded as data_processed "BDV_recovery_filtered")

library(ggplot2)
library(gridExtra)

# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
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
  
  # Calcolare la probabilitC  normalizzata di superamento della soglia per categoria
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
    # Creare il grafico a barre con probabilitC  normalizzata
    ggplot(aes(x = forest_cat, y = average_percent_above_threshold, fill = forest_cat)) +
    geom_bar(stat = "identity") +
    labs(title = paste(taxa, "- Normalized Probability of Exceeding", threshold_label),
         x = "Forest Category",
         y = paste("Normalized Probability of Exceeding", threshold_label)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Formattare come %
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Serie temporale con ribbon (mean, min, max per anno e categoria)
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

# Plot G1: Bar plots for all taxa together
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

# Plot G2: Time series for all taxa together
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

# Plot G1: Bar plots for all taxa together
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

# Plot G2: Time series for all taxa together, separated by forest category
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

dev.off()

################################################################################
######################            THE END                 ######################
################################################################################
