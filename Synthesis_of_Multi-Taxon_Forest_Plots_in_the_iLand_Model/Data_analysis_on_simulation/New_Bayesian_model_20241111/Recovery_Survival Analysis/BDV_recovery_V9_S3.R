################          MARCO BALDO - 2025.07.04        ######################
################       DATA AND GRAPHS IN THIRD ANALYSIS   #####################
################       BDV RECOVERY ANALYSIS SESSION 3    ######################
################          ARTICLE BALDO ET AL. 2025       ######################
################################################################################

# Important here is calculating of the plots that recover how many years and
# the percentage of the whole set of 1000 fitted function per year
# There are three main types of visualization of the same analysis

#-------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gridExtra)

# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
pdf(paste0(dataroot, "BDV_recovery_all_V14_relative_year_of_recovery.pdf"), height=9, width=16)

# Define fixed colors for Forest Categorys
management_colors <- c(
  "Old-Growth"   = "#1f77b4",  # Blue Represents conservation and stability
  "Native Broadleaves" = "#ff7f0e",  # Orange Associated with deciduous trees
  "Non-Native Coniferous"  = "#2ca02c")

management_colors <- c(
  "Old-Growth"   = "#3B9AB2",  # Elegant teal Stability, conservation  
  "Beech-Oak" = "#E69F00",  # Warm golden amber Deciduous richness 
  "Conifer"  = "#009E73"  # Burnt orange Evergreen dominance 
)

management_colors <- c(
  "Old-Growth"   = "#3B9AB2",  # Elegant teal Stability, conservation  
  "Beech-Oak" = "#D55E00",  # Warm golden amber Deciduous richness 
  "Conifer"  = "darkolivegreen" 
)

management_colors <- c(
  "Old-Growth"   = "#3B9AB2",  # Elegant teal Stability, conservation  
  "Native Broadleaves" = "#FF8247",  # Warm golden amber Deciduous richness  
  "Non-Native Coniferous"  = "darkolivegreen" 
)

# 5' graph
#-------------------------------------------------------------------------------
# FUNZIONE per generare i grafici per un dato taxa e quartile
plot_taxa_quartile <- function(df, taxa, quartile) {
  
  # Nome della colonna da analizzare
  col_name <- paste0("%_above_", quartile, "_", taxa)
  
  # Filtra solo gli anni in cui la % C( > 0 (tranne per G1)
  df_filtered <- df %>%
    filter(!!sym(col_name) > 0)
  
  # (G1) Bar plot: Relative % of Years Above 0 per Forest Category
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
    scale_fill_manual(values = management_colors) +  # Fixed colors
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Show % format
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  # (G2) Scatter plot: Year vs. % sopra la soglia, colorato per Forest Category
  G2 <- ggplot(df_filtered, aes(x = year, y = !!sym(col_name), color = forest_cat)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "loess", se = FALSE, size = 1.5, span = 0.3) + # Lower span = more wiggly curve
    labs(title = paste(taxa, quartile, "- % Above Threshold per Year"),
         x = "Year",
         y = paste("% Above", quartile)) +
    scale_color_manual(values = management_colors) +  # Fixed colors
    theme_minimal()
  
  # (G3) Boxplot: Distribuzione delle % sopra la soglia per Forest Category
  G3 <- ggplot(df_filtered, aes(x = forest_cat, y = !!sym(col_name), fill = forest_cat)) +
    geom_boxplot(outlier.color = "black", outlier.alpha = 0.1) +
    labs(title = paste(taxa, quartile, "- Distribution of % Above"),
         x = "Forest Category",
         y = paste("% Above", quartile)) +
    scale_fill_manual(values = management_colors) +  # Fixed colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # (G4) Boxplot per etc in cui si supera la soglia
  G4 <- ggplot(df_filtered, aes(x = forest_cat, y = StandAge, fill = forest_cat)) +
    geom_boxplot(outlier.shape = 1, outlier.color = "gray40", outlier.size = 1, outlier.alpha = 0.3) +
    labs(title = paste(taxa, quartile, "- Age Distribution Above Threshold"),
         x = "Forest Category",
         y = "StandAge") +
    scale_fill_manual(values = management_colors) +  # Fixed colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Mostra i 4 grafici insieme
  grid.arrange(G1, G2, G3, G4, ncol = 2)
}


# Esempio per Bryophytes e quartile Median
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
  
  # Filter only years where % > 0 (except for G1)
  df_filtered <- df %>%
    filter(!!sym(col_name) > 0)
  
  # (G1) Bar plot: Relative % of Years Above 0 per Forest Category
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
  
  # (G2) Scatter plot: Year vs. % Above Threshold
  G2 <- ggplot(df_filtered, aes(x = year, y = !!sym(col_name), color = forest_cat)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "loess", se = FALSE, size = 1.5, span = 0.3) +  
    labs(title = paste(taxa, "-", quartile, "- % Above Threshold per Year"),
         x = "Year",
         y = paste("% Above", quartile)) +
    scale_color_manual(values = management_colors) +  
    theme_minimal() +
    theme(legend.position = "none")  # Hide legend to avoid duplication
  
  # (G3) Boxplot: Distribution of % Above by Forest Category
  G3 <- ggplot(df_filtered, aes(x = forest_cat, y = !!sym(col_name), fill = forest_cat)) +
    geom_boxplot(outlier.color = "black", outlier.alpha = 0.1) +
    labs(title = paste(taxa, "-", quartile, "- Distribution of % Above"),
         x = "Forest Category",
         y = paste("% Above", quartile)) +
    scale_fill_manual(values = management_colors) +  
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")  # Hide legend
  
  # (G4) Boxplot: StandAge Distribution Above Threshold
  G4 <- ggplot(df_filtered, aes(x = forest_cat, y = StandAge, fill = forest_cat)) +
    geom_boxplot(outlier.shape = 1, outlier.color = "gray40", outlier.size = 1, outlier.alpha = 0.3) +
    labs(title = paste(taxa, "-", quartile, "- Stand Age Distribution Above Threshold"),
         x = "Forest Category",
         y = "StandAge") +
    scale_fill_manual(values = management_colors) +  
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")  # Hide legend
  
  # Arrange all 4 plots in a single row
  grid.arrange(G1, G2, G3, G4, ncol = 4)
}

# Generate graphs for each taxa (only "Median" or "Q3" for moths)
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
    
    # ???? Filtra solo gli anni in cui la % ?? > 0 (tranne per G1)
    df_filtered <- df %>%
      filter(!!sym(col_name) > 0)
    
    if (plot_type == "bar") {
      # (G1) Bar plot: Relative % of Years Above 0 per Forest Category
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
      # (G2) Scatter plot: Year vs. % Above Threshold
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
      # (G3) Boxplot: Distribution of % Above by Forest Category
      p <- ggplot(df_filtered, aes(x = forest_cat, y = !!sym(col_name), fill = forest_cat)) +
        geom_boxplot(outlier.color = "black", outlier.alpha = 0.1) +
        labs(title = paste(taxa), # "-", quartile, "- Distribution of % Above"),
             x = "Forest Category",
             y = paste("% Above", quartile)) +
        scale_fill_manual(values = management_colors) +  
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")  # Legenda in basso
      
    } else if (plot_type == "Stand Age") {
      # (G4) Boxplot: Stand Age Distribution Above Threshold
      p <- ggplot(df_filtered, aes(x = forest_cat, y = StandAge, fill = forest_cat)) +
        geom_boxplot(outlier.shape = 1, outlier.color = "gray40", outlier.size = 1, outlier.alpha = 0.3) +
        labs(title = paste(taxa), # "-", quartile, "- Age Distribution Above Threshold"),
             x = "Forest Category",
             y = "Stand Age") +
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

# Genera un grafico per ogni tipo (bar, scatter, box, age) per tutti i taxa
plot_single_type_all_taxa(BDV_recovery_filtered, "bar")   # Bar plot
plot_single_type_all_taxa(BDV_recovery_filtered, "scatter")  # Scatter plot
plot_single_type_all_taxa(BDV_recovery_filtered, "box")    # Boxplot
plot_single_type_all_taxa(BDV_recovery_filtered, "Stand Age")    # Stand Age Distribution

#-------

dev.off()

# write excel % csv
writexl::write_xlsx(BDV_recovery_filtered, "C:/iLand/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/BDV_recovery_data_all_V13_no_old_growth.xlsx")
readr::write_csv(BDV_recovery_filtered, "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/BDV_recovery_data_all_V13_no_old_growth.csv")

################################################################################
######################            THE END                 ######################
################################################################################