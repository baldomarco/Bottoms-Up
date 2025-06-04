#-------------------------------------------------------------------------------
#  THIS IS THE CODE TO COMPUTE A SINGLE PLOT BDV TAXA DIFFERENCE CALCULATION
#-------------------------------------------------------------------------------


library(dplyr)
library(tidyr)

# Update taxa columns with the correct names
taxa_columns <- c("BRYO_PRED_RICH_2.5", "BRYO_PRED_RICH_50", "BRYO_PRED_RICH_97.5", 
                  "LICHEN_PRED_RICH_2.5", "LICHEN_PRED_RICH_50", "LICHEN_PRED_RICH_97.5", 
                  "MACROFUNGI_PRED_RICH_2.5", "MACROFUNGI_PRED_RICH_50", "MACROFUNGI_PRED_RICH_97.5", 
                  "BEETLES_PRED_RICH_2.5", "BEETLES_PRED_RICH_50", "BEETLES_PRED_RICH_97.5", 
                  "MOTHS_PRED_RICH_2.5", "MOTHS_PRED_RICH_50", "MOTHS_PRED_RICH_97.5")

# Step 3: Create plotID from the run column and filter the data for the two types of 'run'
filtered_data <- Bayesian_BDV_model_V3_multi %>%
  mutate(plotID = sub(".*(L6_01).*", "\\1", run)) %>%  # Extract 'plotID' from 'run'
  filter(run %in% c("DB_CZ_JH1_L6XL6_01_plot_V6.sqlite", "DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite")) %>%
  select(year, plotID, run, any_of(taxa_columns))  # Ensure correct column names

# Step 4: Reshape the data into wide format
wide_data <- filtered_data %>%
  pivot_wider(names_from = run, values_from = any_of(taxa_columns))

# Step 5: Calculate the differences between the two 'run' types
wide_data <- wide_data %>%
  mutate(
    BRYO_PRED_RICH_2.5_diff = BRYO_PRED_RICH_2.5_DB_CZ_JH1_L6XL6_01_plot_V6.sqlite - BRYO_PRED_RICH_2.5_DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite,
    BRYO_PRED_RICH_50_diff = BRYO_PRED_RICH_50_DB_CZ_JH1_L6XL6_01_plot_V6.sqlite - BRYO_PRED_RICH_50_DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite,
    BRYO_PRED_RICH_97.5_diff = BRYO_PRED_RICH_97.5_DB_CZ_JH1_L6XL6_01_plot_V6.sqlite - BRYO_PRED_RICH_97.5_DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite,
    LICHEN_PRED_RICH_2.5_diff = LICHEN_PRED_RICH_2.5_DB_CZ_JH1_L6XL6_01_plot_V6.sqlite - LICHEN_PRED_RICH_2.5_DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite,
    LICHEN_PRED_RICH_50_diff = LICHEN_PRED_RICH_50_DB_CZ_JH1_L6XL6_01_plot_V6.sqlite - LICHEN_PRED_RICH_50_DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite,
    LICHEN_PRED_RICH_97.5_diff = LICHEN_PRED_RICH_97.5_DB_CZ_JH1_L6XL6_01_plot_V6.sqlite - LICHEN_PRED_RICH_97.5_DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite,
    MACROFUNGI_PRED_RICH_2.5_diff = MACROFUNGI_PRED_RICH_2.5_DB_CZ_JH1_L6XL6_01_plot_V6.sqlite - MACROFUNGI_PRED_RICH_2.5_DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite,
    MACROFUNGI_PRED_RICH_50_diff = MACROFUNGI_PRED_RICH_50_DB_CZ_JH1_L6XL6_01_plot_V6.sqlite - MACROFUNGI_PRED_RICH_50_DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite,
    MACROFUNGI_PRED_RICH_97.5_diff = MACROFUNGI_PRED_RICH_97.5_DB_CZ_JH1_L6XL6_01_plot_V6.sqlite - MACROFUNGI_PRED_RICH_97.5_DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite,
    BEETLES_PRED_RICH_2.5_diff = BEETLES_PRED_RICH_2.5_DB_CZ_JH1_L6XL6_01_plot_V6.sqlite - BEETLES_PRED_RICH_2.5_DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite,
    BEETLES_PRED_RICH_50_diff = BEETLES_PRED_RICH_50_DB_CZ_JH1_L6XL6_01_plot_V6.sqlite - BEETLES_PRED_RICH_50_DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite,
    BEETLES_PRED_RICH_97.5_diff = BEETLES_PRED_RICH_97.5_DB_CZ_JH1_L6XL6_01_plot_V6.sqlite - BEETLES_PRED_RICH_97.5_DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite,
    MOTHS_PRED_RICH_2.5_diff = MOTHS_PRED_RICH_2.5_DB_CZ_JH1_L6XL6_01_plot_V6.sqlite - MOTHS_PRED_RICH_2.5_DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite,
    MOTHS_PRED_RICH_50_diff = MOTHS_PRED_RICH_50_DB_CZ_JH1_L6XL6_01_plot_V6.sqlite - MOTHS_PRED_RICH_50_DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite,
    MOTHS_PRED_RICH_97.5_diff = MOTHS_PRED_RICH_97.5_DB_CZ_JH1_L6XL6_01_plot_V6.sqlite - MOTHS_PRED_RICH_97.5_DB_CZ_JH1_L6XL6_01_plot_V6_mng.sqlite
  )

# Step 6: Select only the necessary columns: year, plotID, and the differences
final_diff_matrix <- wide_data %>%
  select(year, plotID, contains("_diff"))

# Step 7: Display the result
head(final_diff_matrix)















#-------------------------------------------------------------------------------
#  THIS IS THE CODE TO AUTOMATISE THE WHOLE SET OF PLOTS DIFFERENCE CALCULATION
#-------------------------------------------------------------------------------

# L6 SITE

library(dplyr)
library(tidyr)

# Step 1: Definire le colonne taxa
taxa_columns <- c("BRYO_PRED_RICH_2.5", "BRYO_PRED_RICH_50", "BRYO_PRED_RICH_97.5", 
                  "LICHEN_PRED_RICH_2.5", "LICHEN_PRED_RICH_50", "LICHEN_PRED_RICH_97.5", 
                  "MACROFUNGI_PRED_RICH_2.5", "MACROFUNGI_PRED_RICH_50", "MACROFUNGI_PRED_RICH_97.5", 
                  "BEETLES_PRED_RICH_2.5", "BEETLES_PRED_RICH_50", "BEETLES_PRED_RICH_97.5", 
                  "MOTHS_PRED_RICH_2.5", "MOTHS_PRED_RICH_50", "MOTHS_PRED_RICH_97.5")

# Step 2: Creare una lista di coppie (unmanaged - managed)
runs <- unique(Bayesian_BDV_model_V3_multi$run)

# Filtriamo solo i run senza "_mng"
unmanaged_runs <- runs[!grepl("_mng", runs)]

# Creiamo le coppie (ogni unmanaged deve avere un managed corrispondente)
# Creiamo le coppie unmanaged-managed con nomi corretti
run_pairs <- dplyr::tibble(
  unmanaged = unmanaged_runs,
  managed = sub("_plot_V6.sqlite", "_plot_V6_mng.sqlite", unmanaged_runs)  # Creiamo il nome giusto
) %>%
  filter(managed %in% runs)  # Manteniamo solo le coppie esistenti

# Step 3: Creare il dataset finale
final_diff_matrix <- list()

# Iterare sulle coppie di run
for (k in 1:nrow(run_pairs)) {
  unmanaged_run <- run_pairs$unmanaged[k]
  managed_run <- run_pairs$managed[k]
  
  # Step 4: Filtrare i dati per questa coppia
  filtered_data <- Bayesian_BDV_model_V3_multi %>%
    filter(run %in% c(unmanaged_run, managed_run)) %>%
    mutate(plotID = sub(".*(L6XL6_\\d+).*", "\\1", run)) %>%  # Estrarre plot ID
    select(year, plotID, run, any_of(taxa_columns)) 
  
  # Step 5: Reshape in formato wide
  wide_data <- filtered_data %>%
    pivot_wider(names_from = run, values_from = any_of(taxa_columns))
  
  # Step 6: Calcolare le differenze (UNMANAGED - MANAGED)
  for (taxa in taxa_columns) {
    unmanaged_col <- paste0(taxa, "_", unmanaged_run)
    managed_col <- paste0(taxa, "_", managed_run)
    
    if (all(c(unmanaged_col, managed_col) %in% names(wide_data))) {
      diff_column_name <- paste0(taxa, "_diff")
      wide_data[[diff_column_name]] <- wide_data[[unmanaged_col]] - wide_data[[managed_col]]
    }
  }
  
  # Step 7: Selezionare solo year, plotID e differenze
  diff_matrix <- wide_data %>%
    select(year, plotID, contains("_diff"))
  
  # Aggiungere alla lista finale
  final_diff_matrix[[k]] <- diff_matrix
}

# Unire tutte le coppie
final_diff_matrix <- bind_rows(final_diff_matrix)

# Step 8: Visualizzare i primi risultati
head(final_diff_matrix)


#------------------------------------------------------------------------------
# VISUALISE THE DIFFERENCE DITRIBUTION THROUGH BOX/VIOLIN PLOTS
#------------------------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)

# https://www.youtube.com/watch?v=mG_xIr3LMKQ&ab_channel=WakjiraTesfahun
#-------------------------------------------------------------------------------
# Visualize all the three percentile distribution of sp richenss per taxa per plot
#-------------------------------------------------------------------------------

# Reshape the data to long format
long_data <- final_diff_matrix %>%
  pivot_longer(cols = contains("_diff"), 
               names_to = c("taxa", "percentile"), 
               names_pattern = "(.*)_(.*)_diff", 
               values_to = "value")

# View the reshaped data
head(long_data)


# Violin plot with box plot overlay
Box1 <- ggplot(long_data, aes(x = plotID, y = value, fill = percentile)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +  # Box plot
  facet_wrap(~ taxa, scales = "free_y") +  # Facet by taxa
  scale_fill_manual(values = c("2.5" = "lightblue", "50" = "lightgreen", "97.5" = "lightcoral")) +  # Percentile colors
  theme_minimal() +
  labs(title = "Distribution of Species Richness Differences Between Unmanaged and Managed Plots Across Taxa and PlotIDs",
       x = "PlotID",
       y = expression(Delta * " Species Richness"),
       fill = "Percentile") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Box1

# Violin plot
ggplot(long_data, aes(x = plotID, y = value, fill = percentile)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  facet_wrap(~ taxa, scales = "free_y", ncol=1) +
  scale_fill_manual(values = c("2.5" = "lightblue", "50" = "lightgreen", "97.5" = "lightcoral")) +
  theme_minimal() +
  labs(title = "Distribuzione dei Valori per Taxa e PlotID (Violin Plot)",
       x = "PlotID", y = "Valore", fill = "Percentile") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot
ggplot(long_data, aes(x = plotID, y = value, fill = percentile)) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +
  facet_wrap(~ taxa, scales = "free_y", , ncol=1) +
  scale_fill_manual(values = c("2.5" = "lightblue", "50" = "lightgreen", "97.5" = "lightcoral")) +
  theme_minimal() +
  labs(title = "Distribuzione dei Valori per Taxa e PlotID (Box Plot)",
       x = "PlotID", y = "Valore", fill = "Percentile") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#-------------------------------------------------------------------------------
# Do the same but only for the 50 percentile
#-------------------------------------------------------------------------------

# Reshape the data to long format
long_data <- final_diff_matrix %>%
  pivot_longer(cols = contains("_diff"), 
               names_to = c("taxa", "percentile"), 
               names_pattern = "(.*)_(.*)_diff", 
               values_to = "value")

# Filter the data to include only the 50th percentile
long_data_50 <- long_data %>%
  filter(percentile == "50")

# Violin plot with box plot overlay for the 50th percentile
Box_2_OFF <- ggplot(long_data_50, aes(x = plotID, y = value, fill = percentile)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +  # Box plot
  facet_wrap(~ taxa, scales = "free_y", ncol=1) +  # Facet by taxa
  scale_fill_manual(values = c("50" = "lightblue")) +  # Percentile color
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.5) +  # Horizontal dashed line at y = 0
  theme_minimal() +
  labs(title = "Distribution of Species Richness Differences Between Unmanaged and Managed Plots Across Taxa and PlotIDs",
       x = "PlotID",
       y = expression(Delta * "  Species Richness"),
       fill = "Percentile") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22),  # Increase x-axis text size
    axis.text.y = element_text(size = 22),  # Increase y-axis text size
    axis.title.x = element_text(size = 22),  # Increase x-axis title size
    axis.title.y = element_text(size = 22),  # Increase y-axis title size
    strip.text = element_text(size = 18),  # Increase facet labels size
    plot.title = element_text(size = 30),  # Increase plot title size
    legend.text = element_text(size = 26)  # Increase legend text size
  )

Box_2_OFF


#-------------------------------------------------------------------------------
# ALL THE PERCENTILE PER TAXA
# Violin plot with box plot overlay for all percentiles, faceted by plotID
Box_3 <- ggplot(long_data, aes(x = plotID, y = value, fill = percentile)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +  # Box plot
  facet_wrap(~ interaction(taxa, percentile), scales = "free_y", ncol=3) +  # Facet by taxa and percentile
  scale_fill_manual(values = c("2.5" = "lightblue", "50" = "lightgreen", "97.5" = "lightcoral")) +  # Percentile colors
  theme_minimal() +
  labs(title = "Distribution of Species Richness Differences Between Unmanaged and Managed Plots Across Taxa, Percentiles, and PlotIDs",
       x = "PlotID",
       y = expression(Delta * " Species Richness"),
       fill = "Percentile") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Box_3


# Violin plot with box plot overlay for all percentiles, faceted by taxa and percentile
Box_3_OFF <- ggplot(long_data, aes(x = plotID, y = value, fill = percentile)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +  # Box plot
  facet_grid(taxa ~ percentile, scales = "free_y", switch = "y") +  # Facet by taxa and percentile, switch labels to left
  scale_fill_manual(values = c("2.5" = "lightblue", "50" = "lightgreen", "97.5" = "lightcoral")) +  # Percentile colors
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.5) +  # Horizontal dashed line at y = 0
  theme_minimal() +
  labs(title = "Distribution of Species Richness Differences Between Unmanaged and Managed Plots Across Taxa and PlotIDs",
       x = "PlotID",
       y = expression(Delta * "  Species Richness"),
       fill = "Percentile") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22),  # Increase x-axis text size
    axis.text.y = element_text(size = 22),  # Increase y-axis text size
    axis.title.x = element_text(size = 22),  # Increase x-axis title size
    axis.title.y = element_text(size = 22),  # Increase y-axis title size
    strip.text = element_text(size = 18),  # Increase facet labels size
    plot.title = element_text(size = 30),  # Increase plot title size
    legend.text = element_text(size = 26)  # Increase legend text size
  )

Box_3_OFF


#-------------------------------------------------------------------------------
# Make it for the 1000 beta repetitions 
#-------------------------------------------------------------------------------

# Step 1: Definire le colonne taxa
taxa_columns <- c("PRED_RICH_BEETLES", "PRED_RICH_BRYOPHYTES", "PRED_RICH_LICHENS", "PRED_RICH_MACROFUNGI", "PRED_RICH_MOTHS")

# Step 2: Creare una lista di coppie (unmanaged - managed)
runs <- unique(bayesian_results_all$run)

# Filtrare solo i run senza "_mng"
unmanaged_runs <- runs[!grepl("_mng", runs)]

# Creare le coppie unmanaged-managed con nomi corretti
run_pairs <- tibble(
  unmanaged = unmanaged_runs,
  managed = sub("_plot_V6.sqlite", "_plot_V6_mng.sqlite", unmanaged_runs)  # Creare il nome giusto
) %>%
  filter(managed %in% runs)  # Mantenere solo le coppie esistenti

# Step 3: Creare il dataset finale
final_diff_matrix <- list()

# Iterare sulle coppie di run
for (k in 1:nrow(run_pairs)) {
  unmanaged_run <- run_pairs$unmanaged[k]
  managed_run <- run_pairs$managed[k]
  
  # Step 4: Filtrare i dati per questa coppia
  filtered_data <- bayesian_results_all %>%
    filter(run %in% c(unmanaged_run, managed_run)) %>%
    mutate(plotID = sub(".*(L6XL6_\\d+).*", "\\1", run)) %>%  # Estrarre plot ID
    select(year...1, plotID, run, any_of(taxa_columns)) 
  
  # Step 5: Reshape in formato wide
  wide_data <- filtered_data %>%
    pivot_wider(
      names_from = run,
      values_from = any_of(taxa_columns),
      values_fn = list  # Gestire i duplicati creando liste
    )
  
  # Step 6: Calcolare le differenze (UNMANAGED - MANAGED)
  for (taxa in taxa_columns) {
    unmanaged_col <- paste0(taxa, "_", unmanaged_run)
    managed_col <- paste0(taxa, "_", managed_run)
    
    if (all(c(unmanaged_col, managed_col) %in% names(wide_data))) {
      diff_column_name <- paste0(taxa, "_diff")
      wide_data[[diff_column_name]] <- mapply(function(u, m) {
        u_mean <- mean(as.numeric(u), na.rm = TRUE)
        m_mean <- mean(as.numeric(m), na.rm = TRUE)
        u_mean - m_mean
      }, wide_data[[unmanaged_col]], wide_data[[managed_col]])
    }
  }
  
  # Step 7: Selezionare solo year, plotID e differenze
  diff_matrix <- wide_data %>%
    select(year...1, plotID, contains("_diff"))
  
  # Aggiungere alla lista finale
  final_diff_matrix[[k]] <- diff_matrix
}

# Unire tutte le coppie
final_diff_matrix <- bind_rows(final_diff_matrix)

# Step 8: Visualizzare i primi risultati
head(final_diff_matrix)


#-------------------------------------------------------------------------------
#  LET'S PLOT THE DIFFERENCE DISTRIBUTION USING THE 1000 BETA COEF
#-------------------------------------------------------------------------------
# Supponendo che 'final_diff_matrix' sia il tuo dataframe originale

# Trasforma i dati in formato long
long_data <- final_diff_matrix %>%
  pivot_longer(
    cols = contains("_diff"),
    names_to = "taxa",
    values_to = "value"
  )

# Definisci una palette di colori con almeno 5 colori distinti
color_palette <- c(
  "PRED_RICH_BEETLES_diff" = "#F8766D",
  "PRED_RICH_BRYOPHYTES_diff" = "#00BFC4",
  "PRED_RICH_LICHENS_diff" = "#7CAE00",
  "PRED_RICH_MACROFUNGI_diff" = "#619CFF",
  "PRED_RICH_MOTHS_diff" = "#FF61CC"
)

# Crea il grafico a violino con sovrapposizione di box plot
Box_4_OFF <- ggplot(long_data, aes(x = plotID, y = value, fill = taxa)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Grafico a violino
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +  # Box plot
  facet_wrap(~ taxa, scales = "free_y", ncol = 1) +  # Facet per taxa
  scale_fill_manual(values = color_palette) +  # Colori personalizzati
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.5) +  # Linea orizzontale tratteggiata a y = 0
  theme_minimal() +
  labs(
    title = "Distribution of Species Richness Differences Between Unmanaged and Managed Plots Across Taxa and PlotIDs",
    x = "PlotID",
    y = expression(Delta * " Species Richness"),
    fill = "Taxa"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22),  # Aumenta la dimensione del testo sull'asse x
    axis.text.y = element_text(size = 22),  # Aumenta la dimensione del testo sull'asse y
    axis.title.x = element_text(size = 22),  # Aumenta la dimensione del titolo dell'asse x
    axis.title.y = element_text(size = 22),  # Aumenta la dimensione del titolo dell'asse y
    strip.text = element_text(size = 18),  # Aumenta la dimensione delle etichette dei facet
    plot.title = element_text(size = 30),  # Aumenta la dimensione del titolo del grafico
    legend.text = element_text(size = 26)  # Aumenta la dimensione del testo nella legenda
  )

Box_4_OFF




dev.off()






#-------------------------------------------------------------------------------
# L1 SITE
#-------------------------------------------------------------------------------

# Step 1: Definire le colonne taxa
taxa_columns <- c("BRYO_PRED_RICH_2.5", "BRYO_PRED_RICH_50", "BRYO_PRED_RICH_97.5", 
                  "LICHEN_PRED_RICH_2.5", "LICHEN_PRED_RICH_50", "LICHEN_PRED_RICH_97.5", 
                  "MACROFUNGI_PRED_RICH_2.5", "MACROFUNGI_PRED_RICH_50", "MACROFUNGI_PRED_RICH_97.5", 
                  "BEETLES_PRED_RICH_2.5", "BEETLES_PRED_RICH_50", "BEETLES_PRED_RICH_97.5", 
                  "MOTHS_PRED_RICH_2.5", "MOTHS_PRED_RICH_50", "MOTHS_PRED_RICH_97.5")

# Step 2: Creare una lista di coppie (unmanaged - managed)
runs <- unique(Bayesian_BDV_model_V3_multi$run)

# Filtriamo solo i run senza "_mng"
unmanaged_runs <- runs[!grepl("_mng", runs)]

# Creiamo le coppie (ogni unmanaged deve avere un managed corrispondente)
# Creiamo le coppie unmanaged-managed con nomi corretti
run_pairs <- dplyr::tibble(
  unmanaged = unmanaged_runs,
  managed = sub("_plot_V6.sqlite", "_plot_V6_mng.sqlite", unmanaged_runs)  # Creiamo il nome giusto
) %>%
  filter(managed %in% runs)  # Manteniamo solo le coppie esistenti

# Step 3: Creare il dataset finale
final_diff_matrix <- list()

# Iterare sulle coppie di run
for (k in 1:nrow(run_pairs)) {
  unmanaged_run <- run_pairs$unmanaged[k]
  managed_run <- run_pairs$managed[k]
  
  # Step 4: Filtrare i dati per questa coppia
  filtered_data <- Bayesian_BDV_model_V3_multi %>%
    filter(run %in% c(unmanaged_run, managed_run)) %>%
    mutate(plotID = sub(".*(L1XL1_\\d+).*", "\\1", run)) %>%  # Estrarre plot ID
    select(year, plotID, run, any_of(taxa_columns)) 
  
  # Step 5: Reshape in formato wide
  wide_data <- filtered_data %>%
    pivot_wider(names_from = run, values_from = any_of(taxa_columns))
  
  # Step 6: Calcolare le differenze (UNMANAGED - MANAGED)
  for (taxa in taxa_columns) {
    unmanaged_col <- paste0(taxa, "_", unmanaged_run)
    managed_col <- paste0(taxa, "_", managed_run)
    
    if (all(c(unmanaged_col, managed_col) %in% names(wide_data))) {
      diff_column_name <- paste0(taxa, "_diff")
      wide_data[[diff_column_name]] <- wide_data[[unmanaged_col]] - wide_data[[managed_col]]
    }
  }
  
  # Step 7: Selezionare solo year, plotID e differenze
  diff_matrix <- wide_data %>%
    select(year, plotID, contains("_diff"))
  
  # Aggiungere alla lista finale
  final_diff_matrix[[k]] <- diff_matrix
}

# Unire tutte le coppie
final_diff_matrix <- bind_rows(final_diff_matrix)

# Step 8: Visualizzare i primi risultati
head(final_diff_matrix)


#------------------------------------------------------------------------------
# VISUALISE THE DIFFERENCE DITRIBUTION THROUGH BOX/VIOLIN PLOTS
#------------------------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)

# https://www.youtube.com/watch?v=mG_xIr3LMKQ&ab_channel=WakjiraTesfahun
#-------------------------------------------------------------------------------
# Visualize all the three percentile distribution of sp richenss per taxa per plot
#-------------------------------------------------------------------------------

# Reshape the data to long format
long_data <- final_diff_matrix %>%
  pivot_longer(cols = contains("_diff"), 
               names_to = c("taxa", "percentile"), 
               names_pattern = "(.*)_(.*)_diff", 
               values_to = "value")

# View the reshaped data
head(long_data)


# Violin plot with box plot overlay
Box1 <- ggplot(long_data, aes(x = plotID, y = value, fill = percentile)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +  # Box plot
  facet_wrap(~ taxa, scales = "free_y") +  # Facet by taxa
  scale_fill_manual(values = c("2.5" = "lightblue", "50" = "lightgreen", "97.5" = "lightcoral")) +  # Percentile colors
  theme_minimal() +
  labs(title = "Distribution of Species Richness Differences Between Unmanaged and Managed Plots Across Taxa and PlotIDs",
       x = "PlotID",
       y = expression(Delta * " Species Richness"),
       fill = "Percentile") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Box1

# Violin plot
ggplot(long_data, aes(x = plotID, y = value, fill = percentile)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  facet_wrap(~ taxa, scales = "free_y", ncol=1) +
  scale_fill_manual(values = c("2.5" = "lightblue", "50" = "lightgreen", "97.5" = "lightcoral")) +
  theme_minimal() +
  labs(title = "Distribuzione dei Valori per Taxa e PlotID (Violin Plot)",
       x = "PlotID", y = "Valore", fill = "Percentile") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot
ggplot(long_data, aes(x = plotID, y = value, fill = percentile)) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +
  facet_wrap(~ taxa, scales = "free_y", ncol=1) +
  scale_fill_manual(values = c("2.5" = "lightblue", "50" = "lightgreen", "97.5" = "lightcoral")) +
  theme_minimal() +
  labs(title = "Distribuzione dei Valori per Taxa e PlotID (Box Plot)",
       x = "PlotID", y = "Valore", fill = "Percentile") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#-------------------------------------------------------------------------------
# Do the same but only for the 50 percentile
#-------------------------------------------------------------------------------

# Reshape the data to long format
long_data <- final_diff_matrix %>%
  pivot_longer(cols = contains("_diff"), 
               names_to = c("taxa", "percentile"), 
               names_pattern = "(.*)_(.*)_diff", 
               values_to = "value")

# Filter the data to include only the 50th percentile
long_data_50 <- long_data %>%
  filter(percentile == "50")

# Violin plot with box plot overlay for the 50th percentile
Box_2_OFF <- ggplot(long_data_50, aes(x = plotID, y = value, fill = percentile)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +  # Box plot
  facet_wrap(~ taxa, scales = "free_y", ncol=1) +  # Facet by taxa
  scale_fill_manual(values = c("50" = "lightblue")) +  # Percentile color
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.5) +  # Horizontal dashed line at y = 0
  theme_minimal() +
  labs(title = "Distribution of Species Richness Differences Between Unmanaged and Managed Plots Across Taxa and PlotIDs",
       x = "PlotID",
       y = expression(Delta * "  Species Richness"),
       fill = "Percentile") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22),  # Increase x-axis text size
    axis.text.y = element_text(size = 22),  # Increase y-axis text size
    axis.title.x = element_text(size = 22),  # Increase x-axis title size
    axis.title.y = element_text(size = 22),  # Increase y-axis title size
    strip.text = element_text(size = 18),  # Increase facet labels size
    plot.title = element_text(size = 30),  # Increase plot title size
    legend.text = element_text(size = 26)  # Increase legend text size
  )

Box_2_OFF


#-------------------------------------------------------------------------------
# ALL THE PERCENTILE PER TAXA
# Violin plot with box plot overlay for all percentiles, faceted by plotID
Box_3 <- ggplot(long_data, aes(x = plotID, y = value, fill = percentile)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +  # Box plot
  facet_wrap(~ interaction(taxa, percentile), scales = "free_y", ncol=3) +  # Facet by taxa and percentile
  scale_fill_manual(values = c("2.5" = "lightblue", "50" = "lightgreen", "97.5" = "lightcoral")) +  # Percentile colors
  theme_minimal() +
  labs(title = "Distribution of Species Richness Differences Between Unmanaged and Managed Plots Across Taxa, Percentiles, and PlotIDs",
       x = "PlotID",
       y = expression(Delta * " Species Richness"),
       fill = "Percentile") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Box_3


# Violin plot with box plot overlay for all percentiles, faceted by taxa and percentile
Box_3_OFF <- ggplot(long_data, aes(x = plotID, y = value, fill = percentile)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +  # Box plot
  facet_grid(taxa ~ percentile, scales = "free_y", switch = "y") +  # Facet by taxa and percentile, switch labels to left
  scale_fill_manual(values = c("2.5" = "lightblue", "50" = "lightgreen", "97.5" = "lightcoral")) +  # Percentile colors
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.5) +  # Horizontal dashed line at y = 0
  theme_minimal() +
  labs(title = "Distribution of Species Richness Differences Between Unmanaged and Managed Plots Across Taxa and PlotIDs",
       x = "PlotID",
       y = expression(Delta * "  Species Richness"),
       fill = "Percentile") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22),  # Increase x-axis text size
    axis.text.y = element_text(size = 22),  # Increase y-axis text size
    axis.title.x = element_text(size = 22),  # Increase x-axis title size
    axis.title.y = element_text(size = 22),  # Increase y-axis title size
    strip.text = element_text(size = 18),  # Increase facet labels size
    plot.title = element_text(size = 30),  # Increase plot title size
    legend.text = element_text(size = 26)  # Increase legend text size
  )

Box_3_OFF


#-------------------------------------------------------------------------------
# Make it for the 1000 beta repetitions 
#-------------------------------------------------------------------------------

# Step 1: Definire le colonne taxa
taxa_columns <- c("PRED_RICH_BEETLES", "PRED_RICH_BRYOPHYTES", "PRED_RICH_LICHENS", "PRED_RICH_MACROFUNGI", "PRED_RICH_MOTHS")

# Step 2: Creare una lista di coppie (unmanaged - managed)
runs <- unique(bayesian_results_all$run)

# Filtrare solo i run senza "_mng"
unmanaged_runs <- runs[!grepl("_mng", runs)]

# Creare le coppie unmanaged-managed con nomi corretti
run_pairs <- tibble(
  unmanaged = unmanaged_runs,
  managed = sub("_plot_V6.sqlite", "_plot_V6_mng.sqlite", unmanaged_runs)  # Creare il nome giusto
) %>%
  filter(managed %in% runs)  # Mantenere solo le coppie esistenti

# Step 3: Creare il dataset finale
final_diff_matrix <- list()

# Iterare sulle coppie di run
for (k in 1:nrow(run_pairs)) {
  unmanaged_run <- run_pairs$unmanaged[k]
  managed_run <- run_pairs$managed[k]
  
  # Step 4: Filtrare i dati per questa coppia
  filtered_data <- bayesian_results_all %>%
    filter(run %in% c(unmanaged_run, managed_run)) %>%
    mutate(plotID = sub(".*(L2XL2_\\d+).*", "\\1", run)) %>%  # Estrarre plot ID
    select(year...1, plotID, run, any_of(taxa_columns)) 
  
  # Step 5: Reshape in formato wide
  wide_data <- filtered_data %>%
    pivot_wider(
      names_from = run,
      values_from = any_of(taxa_columns),
      values_fn = list  # Gestire i duplicati creando liste
    )
  
  # Step 6: Calcolare le differenze (UNMANAGED - MANAGED)
  for (taxa in taxa_columns) {
    unmanaged_col <- paste0(taxa, "_", unmanaged_run)
    managed_col <- paste0(taxa, "_", managed_run)
    
    if (all(c(unmanaged_col, managed_col) %in% names(wide_data))) {
      diff_column_name <- paste0(taxa, "_diff")
      wide_data[[diff_column_name]] <- mapply(function(u, m) {
        u_mean <- mean(as.numeric(u), na.rm = TRUE)
        m_mean <- mean(as.numeric(m), na.rm = TRUE)
        u_mean - m_mean
      }, wide_data[[unmanaged_col]], wide_data[[managed_col]])
    }
  }
  
  # Step 7: Selezionare solo year, plotID e differenze
  diff_matrix <- wide_data %>%
    select(year...1, plotID, contains("_diff"))
  
  # Aggiungere alla lista finale
  final_diff_matrix[[k]] <- diff_matrix
}

# Unire tutte le coppie
final_diff_matrix <- bind_rows(final_diff_matrix)

# Step 8: Visualizzare i primi risultati
head(final_diff_matrix)


#-------------------------------------------------------------------------------
#  LET'S PLOT THE DIFFERENCE DISTRIBUTION USING THE 1000 BETA COEF
#-------------------------------------------------------------------------------
# Supponendo che 'final_diff_matrix' sia il tuo dataframe originale

# Trasforma i dati in formato long
long_data <- final_diff_matrix %>%
  pivot_longer(
    cols = contains("_diff"),
    names_to = "taxa",
    values_to = "value"
  )

# Definisci una palette di colori con almeno 5 colori distinti
color_palette <- c(
  "PRED_RICH_BEETLES_diff" = "#F8766D",
  "PRED_RICH_BRYOPHYTES_diff" = "#00BFC4",
  "PRED_RICH_LICHENS_diff" = "#7CAE00",
  "PRED_RICH_MACROFUNGI_diff" = "#619CFF",
  "PRED_RICH_MOTHS_diff" = "#FF61CC"
)

# Crea il grafico a violino con sovrapposizione di box plot
Box_4_OFF <- ggplot(long_data, aes(x = plotID, y = value, fill = taxa)) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Grafico a violino
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +  # Box plot
  facet_wrap(~ taxa, scales = "free_y", ncol = 1) +  # Facet per taxa
  scale_fill_manual(values = color_palette) +  # Colori personalizzati
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.5) +  # Linea orizzontale tratteggiata a y = 0
  theme_minimal() +
  labs(
    title = "L4 - Distribution of Species Richness Differences Between Unmanaged and Managed Plots Across Taxa and PlotIDs",
    x = "PlotID",
    y = expression(Delta * " Species Richness"),
    fill = "Taxa"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22),  # Aumenta la dimensione del testo sull'asse x
    axis.text.y = element_text(size = 22),                         # Aumenta la dimensione del testo sull'asse y
    axis.title.x = element_text(size = 22),                        # Aumenta la dimensione del titolo dell'asse x
    axis.title.y = element_text(size = 22),                        # Aumenta la dimensione del titolo dell'asse y
    strip.text = element_text(size = 18),                          # Aumenta la dimensione delle etichette dei facet
    plot.title = element_text(size = 30),                          # Aumenta la dimensione del titolo del grafico
    legend.text = element_text(size = 26)                          # Aumenta la dimensione del testo nella legenda
  )

Box_4_OFF
