################          MARCO BALDO - 2025.07.04        ######################
################       DATA UPLOADING AND FOURTH ANALYSIS   #####################
################       BDV RECOVERY ANALYSIS SESSION 4    ######################
################          ARTICLE BALDO ET AL. 2025       ######################
################################################################################

## VERY IMPORTANT SESSION ON THE YEAR OR RECOVERY (YoR)
## SECOND PART OF THE ANALYSIS WHERE WE CALCULATE THE YEAR OF BDV RECOVERY - 
## AS SOON SOME VALUES >= OF THE Q1,Q2 OR Q3 THRESHOLD.

library(readxl)
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(stringr)

# ANALYSIS
#------------------
BDV_recovery_data <- read.csv("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/BDV_recovery_data_all_V13_no_old_growth.csv")
BDV_recovery_data

#------------------
# Initial recovery stage: first row (year == 0) per plot
initial_recovery_stage <- BDV_recovery_data %>%
  filter(year == 0) %>%
  group_by(plotID) %>%
  slice(1) %>%
  ungroup()

initial_recovery_stage

# Recovery years (YoR) per plot and taxa-threshold. The YoR will be assigned at every % threshold Taxa richness, so instead then the % reaching the threshold now you will have the year in which at least some recovered.
recovery_years <- BDV_recovery_data %>%
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

recovery_years
#------------------

recovery_long <- recovery_years %>%
  # Rename columns to remove "X._" and replace with "Per_"
  rename_with(~ gsub("^X\\._", "Per_", .x), contains("above")) %>%
  # Pivot to long format
  pivot_longer(
    cols = starts_with("Per_"),
    names_to = "taxa_threshold",
    values_to = "YoR"
  ) %>%
  # Join StandAge from initial_recovery_stage (if needed)
  left_join(initial_recovery_stage %>% select(plotID, StandAge), by = "plotID") %>%
  # Create unified 'Stand age' column and drop redundant columns if they exist
  mutate(StandAge = coalesce(StandAge.x, StandAge.y)) %>%
  select(-StandAge.x, -StandAge.y)


# write excel needed for the next script
write.csv(recovery_long, 
          "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/recovery_long.csv", 
          row.names = FALSE)

#--------------------
# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
pdf(paste0(dataroot, "BDV_recovery_V8_no_old_growth.pdf"), height=9, width=16)

#--------------------
management_colors <- c(
  #"Old-Growth"   = "#3B9AB2",  # Elegant teal Stability, conservation  
  "Native Broadleaves" = "#FF8247",  # Warm golden amber Deciduous richness  
  "Non-Native Coniferous"  = "darkolivegreen" 
)

# 8' graph
#-------------------------------------------------------------------------------
# Function to generate 2x3 plots for a given taxa
plot_taxa_overview <- function(df, taxa_name) {
  df_taxa <- df %>%
    filter(str_detect(taxa_threshold, taxa_name))
  
  quartiles <- c("Q1", "Median", "Q3")
  
  # Generate plots per quartile
  plots <- lapply(quartiles, function(q) {
    df_q <- df_taxa %>%
      filter(str_detect(taxa_threshold, q))
    
    # Scatter plot
    scatter <- ggplot(df_q, aes(x = StandAge, y = YoR , color = forest_cat)) +
      geom_point(alpha = 0.7, size = 2.5) +
      ylim(0,NA)+
      geom_smooth(method = "lm", se = FALSE, size = 1.5, span = 0.3) +
      labs(title = paste(taxa_name, "-", q, "Scatter"), x = "Stand Age", y = "YoR") +
      scale_color_manual(values = management_colors) +
      theme_minimal() +
      theme(legend.position = "none")
    
    # Boxplot
    box <- ggplot(df_q, aes(x = forest_cat, y = YoR, fill = forest_cat)) +
      geom_boxplot(outlier.shape = 21, outlier.size = 1.5, outlier.color = "black") +
      labs(title = paste(taxa_name, "-", q, "Boxplot"), x = "Forest Categories", y = "YoR") +
      scale_fill_manual(values = management_colors) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    list(scatter, box)
  })
  
  # Arrange plots in a 2x3 grid (scatter on top, box below)
  grid.arrange(
    plots[[1]][[1]], plots[[2]][[1]], plots[[3]][[1]],
    plots[[1]][[2]], plots[[2]][[2]], plots[[3]][[2]],
    ncol = 3,
    top = paste("Taxa Overview:", taxa_name)
  )
}

# Generate plots for all taxa
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")
for (taxa in taxa_list) {
  plot_taxa_overview(recovery_long, taxa)
}


# 9' graph
#-------------------------------------------------------------------------------
# LET'S DO THE SAME VISUALIZATION BUT WITH A SCATTER PLOT AND A BOXPLOT OF THE RECOVERY FOR EVERY QUARTILE AND TAXA

# Function to generate 1x2 plots (scatter + boxplot) per taxa and quartile
plot_taxa_quartile_1x2 <- function(df, taxa_name, quartile) {
  df_filtered <- df %>%
    filter(str_detect(taxa_threshold, taxa_name) & str_detect(taxa_threshold, quartile))
  
  # Scatter plot
  scatter <- ggplot(df_filtered, aes(x = YoR, y = StandAge, color = forest_cat)) +
    geom_point(alpha = 0.7, size = 2.5) +
    labs(title = paste(taxa_name, "-", quartile, "Scatter Plot"), x = "YoR", y = "Stand Age") +
    scale_color_manual(values = management_colors) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Boxplot
  box <- ggplot(df_filtered, aes(x = forest_cat, y = YoR, fill = forest_cat)) +
    geom_boxplot(outlier.shape = 21, outlier.size = 1.5, outlier.color = "black") +
    labs(title = paste(taxa_name, "-", quartile, "Boxplot"), x = "Forest Categories", y = "YoR") +
    scale_fill_manual(values = management_colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  # Combine scatter + boxplot in 1x2 layout
  grid.arrange(scatter, box, ncol = 2, top = paste("Taxa:", taxa_name, "| Quartile:", quartile))
}

# Loop over taxa and quartiles to generate all plots
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
  col_name <- paste0("Per_above_", quartile, "_", taxa_list)  # Formato: %_above_Q3_MOTHS o %_above_Median_BRYOPHYTES
  
  # Filtro i dati per quartile e taxa
  df_filtered <- df %>%
    filter(taxa_threshold == col_name & !is.na(YoR))  # Filtro per quartile e taxa
  
  # Verifica se ci sono dati dopo il filtro
  if (nrow(df_filtered) == 0) {
    message(paste("No data available for", taxa_list, "in quartile", quartile))
    return(NULL)
  }
  
  # Scatter plot con modello lineare per ogni categoria forestale
  scatter <- ggplot(df_filtered, aes(x = StandAge, y = YoR, color = forest_cat)) +
    geom_point(alpha = 0.5, size = 2.5) +
    geom_smooth(method = "lm", se = FALSE, aes(group = forest_cat)) +  # Modello lineare per ogni categoria forestale
    labs(title = paste("Scatter Plot - ", taxa_list, " - ", quartile), x = "Stand Age", y = "Year of Recovery (YoR)") +
    scale_color_manual(values = management_colors) +
    facet_wrap(~forest_cat, ncol = 5) +  # Facet per ogni categoria forestale
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Boxplot per tutti i taxa e quartile, per categoria forestale
  box <- ggplot(df_filtered, aes(x = forest_cat, y = YoR, fill = forest_cat)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 1.5, outlier.color = "black") +
    labs(title = paste("Boxplot - ", taxa_list, " - ", quartile), x = "Forest Categories", y = "Year of Recovery (YoR)") +
    scale_fill_manual(values = management_colors) +
    facet_wrap(~forest_cat, ncol = 5) +  # Facet per ogni categoria forestale
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
  
  # Mostra i due grafici separati
  grid.arrange(scatter, box, ncol = 1)
}

# Genera i grafici per tutti i taxa e quartili (solo Q3 per Moths, Median per gli altri taxa)
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")
quartiles <- c("Q1", "Median", "Q3")  # Median per tutti i taxa tranne Moths, Q3 per Moths

# Loop per generare il grafico per ogni quartile
for (q in quartiles) {
  for (taxa in taxa_list) {
    plot_taxa_plots(recovery_long, taxa, q)
  }
}

plot_taxa_plots <- function(df, taxa, quartile) {
  col_name <- paste0("Per_above_", quartile, "_", taxa)
  
  df_filtered <- df %>%
    filter(taxa_threshold == col_name & !is.na(YoR))
  
  if (nrow(df_filtered) == 0) {
    message(paste("No data available for", taxa, "in quartile", quartile))
    
    # Create empty scatter plot with message
    scatter <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6, color = "red") +
      theme_void() +
      ggtitle(paste("Scatter Plot -", taxa, "-", quartile))
    
    # Create empty boxplot with message
    box <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6, color = "red") +
      theme_void() +
      ggtitle(paste("Boxplot -", taxa, "-", quartile))
    
  } else {
    # Your usual plotting code here using df_filtered
    scatter <- ggplot(df_filtered, aes(x = StandAge, y = YoR, color = forest_cat)) +
      geom_point(alpha = 0.5, size = 2.5) +
      geom_smooth(method = "lm", se = FALSE, aes(group = forest_cat)) +
      labs(title = paste("Scatter Plot -", taxa, "-", quartile),
           x = "Stand Age", y = "Year of Recovery (YoR)") +
      scale_color_manual(values = management_colors) +
      facet_wrap(~forest_cat, ncol = 5) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    box <- ggplot(df_filtered, aes(x = forest_cat, y = YoR, fill = forest_cat)) +
      geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 1.5, outlier.color = "black") +
      labs(title = paste("Boxplot -", taxa, "-", quartile),
           x = "Forest Categories", y = "Year of Recovery (YoR)") +
      scale_fill_manual(values = management_colors) +
      facet_wrap(~forest_cat, ncol = 5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
  }
  
  # Arrange the plots vertically
  grid.arrange(scatter, box, ncol = 1)
}


# 11'  MOST IMPORTANT
#-------------------------------------------------------------------------------

# Function to generate combined plots for selected quartiles and taxa (scatter and boxplot)
plot_combined_taxa <- function(df, taxa_list, quartiles, min_y_value = 0) {
  
  # Generate the list of data for selected quartiles and taxa
  df_filtered <- df %>%
    mutate(
      taxa = str_extract(taxa_threshold, "(?<=Q1_|Median_|Q3_)[A-Za-z]+"),  # Extract taxa (e.g., BRYOPHYTES)
      quartile = str_extract(taxa_threshold, "Q1|Median|Q3")  # Extract quartile (Q1, Median, Q3)
    ) %>%
    filter(
      (quartile == "Median" & taxa %in% taxa_list) |  # Keep Median for all taxa in the list
        (taxa == "MOTHS" & quartile == "Q3")          # Keep Q3 only for Moths
    )
  
  # Create the scatter plot for each taxa (StandAge vs YoR)
  scatter_plots <- lapply(taxa_list, function(taxa_name) {
    df_taxa <- df_filtered %>% filter(taxa == taxa_name)
    
    ggplot(df_taxa, aes(x = StandAge, y = YoR, color = forest_cat)) +
      geom_point(alpha = 0.5, size = 2.5) +
      geom_smooth(method = "lm", se = FALSE, size = 1.5, span = 0.7) +
      labs(title = paste(taxa_name, "- Scatter"), x = "Stand Age", y = "YoR") +
      scale_color_manual(values = management_colors) +
      scale_y_continuous(limits = c(min_y_value, NA)) +  # Set min y-value
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Create the scatter plot with inverted axes for each taxa (YoR vs StandAge)
  scatter_plots_inverted <- lapply(taxa_list, function(taxa_name) {
    df_taxa <- df_filtered %>% filter(taxa == taxa_name)
    
    ggplot(df_taxa, aes(x = YoR, y = StandAge, color = forest_cat)) + # Inverted axes
      geom_point(alpha = 0.5, size = 2.5) +
      geom_smooth(method = "lm", se = FALSE, size = 1.5, span = 0.7) +
      labs(title = paste(taxa_name, "- Scatter (Inverted)"), x = "YoR", y = "Stand Age") + # Labels updated
      scale_color_manual(values = management_colors) +
      scale_x_continuous(limits = c(min_y_value, NA)) +  # Set min x-value for YoR
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Create the boxplot for each taxa (YoR on y-axis, with 0.7 shading)
  box_plots_yor <- lapply(taxa_list, function(taxa_name) {
    df_taxa <- df_filtered %>% filter(taxa == taxa_name)
    
    ggplot(df_taxa, aes(x = forest_cat, y = YoR, fill = forest_cat)) +
      geom_boxplot(outlier.shape = 21, outlier.size = 1.5, outlier.color = "black", alpha = 0.7) + # Shading effect 0.7
      labs(title = paste(taxa_name, "- Boxplot (YoR)"), x = "Forest Categories", y = "YoR") +
      scale_fill_manual(values = management_colors) +
      scale_y_continuous(limits = c(min_y_value, NA)) +  # Set min y-value
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  })
  
  # Create the boxplot for each taxa (Stand Age on y-axis, with 0.7 shading) - not combined in final output
  box_plots_standage <- lapply(taxa_list, function(taxa_name) {
    df_taxa <- df_filtered %>% filter(taxa == taxa_name)
    
    ggplot(df_taxa, aes(x = forest_cat, y = StandAge, fill = forest_cat)) + # Stand Age on y-axis
      geom_boxplot(outlier.shape = 21, outlier.size = 1.5, outlier.color = "black", alpha = 0.7) + # Shading effect 0.7
      labs(title = paste(taxa_name, "- Boxplot (Stand Age)"), x = "Forest Categories", y = "Stand Age") + # Labels updated
      scale_fill_manual(values = management_colors) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  })
  
  # Arrange YoR box plots in 1 row and 5 columns
  box_grid_yor <- do.call(grid.arrange, c(box_plots_yor, nrow = 1, ncol = 5))
  
  # Arrange standard scatter plots in 1 row and 5 columns
  scatter_grid <- do.call(grid.arrange, c(scatter_plots, nrow = 1, ncol = 5))
  
  # Arrange inverted scatter plots in 1 row and 5 columns
  scatter_inverted_grid <- do.call(grid.arrange, c(scatter_plots_inverted, nrow = 1, ncol = 5))
  
  # Combine YoR boxplots and standard scatter plots
  combined_panel_1 <- grid.arrange(
    box_grid_yor,
    scatter_grid,
    nrow = 2,
    heights = c(1, 1) # Equal height for both rows
  )
  
  # Combine YoR boxplots and inverted scatter plots
  combined_panel_2 <- grid.arrange(
    box_grid_yor,
    scatter_inverted_grid,
    nrow = 2,
    heights = c(1, 1) # Equal height for both rows
  )
  
  # Return a list of the combined panels and the individual Stand Age boxplots
  return(list(combined_panel_1 = combined_panel_1,
              combined_panel_2 = combined_panel_2,
              box_plots_standage = box_plots_standage)) # Stand Age boxplots returned individually
}

# Define taxa and quartiles for filtering
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")
quartiles <- c("Median", "Q3")  # Median for all taxa except Moths, Q3 for Moths

# Generate the combined plots for selected taxa and quartiles with a min y-value of 0
plot_results <- plot_combined_taxa(recovery_long, taxa_list, quartiles, min_y_value = 0)

# THIS IS TRUE FOR ALL THE TAXA BECAUSE IS BASICALLY SAYING THE MEDIAN AGE OF THE STANDS WHICH RECOVERED IN OUR DATA
# To view individual Stand Age boxplots (e.g., for BRYOPHYTES):
print(plot_results$box_plots_standage[[1]]) 


################################################################################
######################            THE END                 ######################
################################################################################