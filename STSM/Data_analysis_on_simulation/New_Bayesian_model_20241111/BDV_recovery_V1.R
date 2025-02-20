# Load required packages
#library(ggplot2)
#library(ggpubr)    # For stat_pvalue_manual
library(tidyr)
library(dplyr)
#library(rstatix)   # For Kruskal-Wallis and correlation tests
library(readxl)    # For reading Excel files

# Load the data
BDV_predictors <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/20_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood.xlsx")

# Remake the names in the table for a better understanding and coding
BDV_predictors <- BDV_predictors %>%
  mutate(management_type = recode(management_type,
                                  "EX" = "Broadl",
                                  "MZ" = "Mixed",
                                  "IN" = "Transition",
                                  "PA" = "Clearcutted",
                                  "PR" = "Old-growth",
                                  "ML" = "Conifer")) %>%
  select(-c(Lichens_RD1, Macrofungi_RD1, Moths_RD1, `Macrofungi x red-listed (2.652)`, `Moths x red-listed (0.574)`)) %>%
  rename(
    Bryophytes = `Epiphytic / epixilic bryophytes (0.212)`,
    Lichens = `Lichens (0.137)`,
    Macrofungi = `Macrofungi (2.118)`,
    Beetles = `Non-flying beetles (0.053)`,
    Moths = `Moths (0.566)`
  )

# Check
table(BDV_predictors$management_type)
glimpse(BDV_predictors)
BDV_predictors

#-------------------------------------------------------------------------------
# Make Old growth forest quartiles for select the 25% and 50% BDV threshold per Taxa sp richness

# Filter only Old-growth category
BDV_old_growth <- BDV_predictors %>% 
  filter(management_type == "Old-growth")

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
# Now assign to them the management_type

# Extract plotID from run names and detect managed versions
bayesian_results_all <- bayesian_results_all %>%
  mutate(
    plotID = gsub(".*(L\\d+_\\d+).*", "\\1", run), # Extract Lx_xx code
    is_mng = grepl("_mng", run) # Check if "_mng" is in the run name
  )

# Merge with BDV_predictors to get management_type
bayesian_results_all <- bayesian_results_all %>%
  left_join(select(BDV_predictors, plotID, management_type), by = "plotID") %>%
  mutate(
    management_type = if_else(is_mng, paste0(management_type, "_mng"), management_type)
  )

# Check unique management types after update
unique(bayesian_results_all$management_type)

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
  select(plotID, year = year...1, age = age...2, management_type,
         BRYOPHYTES = PRED_RICH_BRYOPHYTES, 
         LICHENS = PRED_RICH_LICHENS, 
         MACROFUNGI = PRED_RICH_MACROFUNGI, 
         BEETLES = PRED_RICH_BEETLES, 
         MOTHS = PRED_RICH_MOTHS)  

# Compute percentages above each quartile
BDV_recovery <- data_filtered %>%
  group_by(plotID, year,management_type, age) %>%
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
  filter(!grepl("_mng", management_type))

#-------------------------------------------------------------------------------
# Let's remove the zero


#-------------------------------------------------------------------------------
# Example Data (assuming the data is already loaded as 'data_processed')
# You would replace the below example data with your actual data
# data_processed <- your_data

library(ggplot2)
library(gridExtra)

# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
pdf(paste0(dataroot, "BDV_recovery_all.pdf"), height=9, width=16)

# BRYOPHYTES--------------------------------------------------------------------
# Bar plot of `%_above_Median_BRYOPHYTES` by management type
P1 <- ggplot(BDV_recovery, aes(x = management_type, y = `%_above_Median_BRYOPHYTES`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Bryophytes Above Median by Management Type",
       x = "Management Type",
       y = "% of Predictions Above Median Bryophytes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Faceted plot of `%_above_Median_BRYOPHYTES` by management type and year
P2 <- ggplot(BDV_recovery, aes(x = year, y = `%_above_Median_BRYOPHYTES`, color = management_type)) +
  geom_line() +
  facet_wrap(~ management_type) +
  labs(title = "Bryophytes Predictions Above PR Sp.Richness Median by Management Type and Year",
       x = "Year",
       y = "% of Predictions Above Median Bryophytes") +
  theme_minimal()


# Plot grid arrange
grid.arrange(P1,P2, ncol=2)

# LICHENS-----------------------------------------------------------------------
# Bar plot of `%_above_Median_LICHENS` by management type
P1 <- ggplot(BDV_recovery, aes(x = management_type, y = `%_above_Median_LICHENS`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Lichens Above Median by Management Type",
       x = "Management Type",
       y = "% Above Median Lichens") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Faceted plot of `%_above_Median_LICHENS` by management type and year
P2 <- ggplot(BDV_recovery, aes(x = year, y = `%_above_Median_LICHENS`, color = management_type)) +
  geom_line() +
  facet_wrap(~ management_type) +
  labs(title = "Lichens Above Median by Management Type and Year",
       x = "Year",
       y = "% Above Median Lichens") +
  theme_minimal()


# Plot grid arrange
grid.arrange(P1,P2, ncol=2)

# MACROFUNGI-----------------------------------------------------------------------
# Bar plot of `%_above_Median_MACROFUNGI` by management type
P1 <- ggplot(BDV_recovery, aes(x = management_type, y = `%_above_Median_MACROFUNGI`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Macrofungi Above Median by Management Type",
       x = "Management Type",
       y = "% Above Median Macrofungi") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Faceted plot of `%_above_Median_MACROFUNGI` by management type and year
P2 <- ggplot(BDV_recovery, aes(x = year, y = `%_above_Median_MACROFUNGI`, color = management_type)) +
  geom_line() +
  facet_wrap(~ management_type) +
  labs(title = "Macrofungi Above Median by Management Type and Year",
       x = "Year",
       y = "% Above Median Macrofungi") +
  theme_minimal()


# Plot grid arrange
grid.arrange(P1,P2, ncol=2)


# BEETLES-----------------------------------------------------------------------
# Bar plot of `%_above_Median_BEETLES` by management type
P1 <- ggplot(BDV_recovery, aes(x = management_type, y = `%_above_Median_BEETLES`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Beetles Above Median by Management Type",
       x = "Management Type",
       y = "% Above Median Beetles") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Faceted plot of `%_above_Median_BEETLES` by management type and year
P2 <- ggplot(BDV_recovery, aes(x = year, y = `%_above_Median_BEETLES`, color = management_type)) +
  geom_line() +
  facet_wrap(~ management_type) +
  labs(title = "Beetles Above Median by Management Type and Year",
       x = "Year",
       y = "% Above Median Beetles") +
  theme_minimal()


# Plot grid arrange
grid.arrange(P1,P2, ncol=2)


# MOTHS-----------------------------------------------------------------------
# Bar plot of `%_above_Median_BEETLES` by management type
P1 <- ggplot(BDV_recovery, aes(x = management_type, y = `%_above_Median_MOTHS`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Moths Above Median by Management Type",
       x = "Management Type",
       y = "% Above Median Moths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Faceted plot of `%_above_Median_BEETLES` by management type and year
P2 <- ggplot(BDV_recovery, aes(x = year, y = `%_above_Median_MOTHS`, color = management_type)) +
  geom_line() +
  facet_wrap(~ management_type) +
  labs(title = "Moths Above Median by Management Type and Year",
       x = "Year",
       y = "% Above Median Moths") +
  theme_minimal()


# Plot grid arrange
grid.arrange(P1,P2, ncol=2)



#-------------------------------------------------------------------------------
# Bar plot of `%_above_Median_BRYOPHYTES` by management type

# Bryophyta
p1 <- ggplot(BDV_recovery_filtered, aes(x = management_type, y = `%_above_Q1_BRYOPHYTES`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Bryophyta ", #(phylum) Above Old-Growth Forest Sp. Richness Q1 by Management Type",
       x = "Management Type",
       y = "% Above Q1 Bryophyta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(BDV_recovery_filtered, aes(x = management_type, y = `%_above_Median_BRYOPHYTES`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Bryophyta ", #(phylum) Above Old-Growth Forest Sp. Richness Median by Management Type",
       x = "Management Type",
       y = "% Above Median Bryophyta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(BDV_recovery_filtered, aes(x = management_type, y = `%_above_Q3_BRYOPHYTES`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Bryophyta ", #(phylum) Above Old-Growth Forest Sp. Richness Q3 by Management Type",
       x = "Management Type",
       y = "% Above Q3 Bryophyta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Lichens

p4 <- ggplot(BDV_recovery_filtered, aes(x = management_type, y = `%_above_Q1_LICHENS`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Lichens ", #(informal group) Above Old-Growth Forest Sp. Richness Q1 by Management Type",
       x = "Management Type",
       y = "% Above Q1 Lichenes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p5 <- ggplot(BDV_recovery_filtered, aes(x = management_type, y = `%_above_Median_LICHENS`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Lichens ", #(informal group) Above Old-Growth Forest Sp. Richness Median by Management Type",
       x = "Management Type",
       y = "% Above Median Lichenes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p6 <- ggplot(BDV_recovery_filtered, aes(x = management_type, y = `%_above_Q3_LICHENS`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Lichens ", #(informal group) Above Old-Growth Forest Sp. Richness Q3 by Management Type",
       x = "Management Type",
       y = "% Above Q3 Lichenes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Beetles

p7 <- ggplot(BDV_recovery_filtered, aes(x = management_type, y = `%_above_Q1_BEETLES`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Coleoptera ", #(order - mainly saproxylic) Above Old-Growth Forest Sp. Richness Q1 by Management Type",
       x = "Management Type",
       y = "% Above Q1 Coleoptera") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p8 <- ggplot(BDV_recovery_filtered, aes(x = management_type, y = `%_above_Median_BEETLES`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Coleoptera ", #(order - mainly saproxylic) Above Old-Growth Forest Sp. Richness Median by Management Type",
       x = "Management Type",
       y = "% Above Median Coleoptera") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p9 <- ggplot(BDV_recovery_filtered, aes(x = management_type, y = `%_above_Q3_BEETLES`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Coleoptera ", #(order - mainly saproxylic) Above Old-Growth Forest Sp. Richness Q3 by Management Type",
       x = "Management Type",
       y = "% Above Q3 Coleoptera") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Macrofungi

p10 <- ggplot(BDV_recovery_filtered, aes(x = management_type, y = `%_above_Q1_MACROFUNGI`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Fungi ", #(kingdom) - Basidiomycota / Ascomycota (phyla principali) Above Old-Growth Forest Sp. Richness Q1 by Management Type",
       x = "Management Type",
       y = "% Above Q1 Macrofungi") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p11 <- ggplot(BDV_recovery_filtered, aes(x = management_type, y = `%_above_Median_MACROFUNGI`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Fungi ", #(kingdom) - Basidiomycota / Ascomycota (main phyla) Above Old-Growth Forest Sp. Richness Median by Management Type",
       x = "Management Type",
       y = "% Above Median Macrofungi") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p12 <- ggplot(BDV_recovery_filtered, aes(x = management_type, y = `%_above_Q3_MACROFUNGI`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Fungi ", #(kingdom) - Basidiomycota / Ascomycota (phyla principali) Above Old-Growth Forest Sp. Richness Q1 by Management Type",
       x = "Management Type",
       y = "% Above Q3 Macrofungi") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Moths

p13 <- ggplot(BDV_recovery_filtered, aes(x = management_type, y = `%_above_Q1_MOTHS`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Lepidoptera", #(order - only moths) Above Old-Growth Forest Sp. Richness Q1 by Management Type",
       x = "Management Type",
       y = "% Above Q1 Moths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p14 <- ggplot(BDV_recovery_filtered, aes(x = management_type, y = `%_above_Median_MOTHS`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Lepidoptera", #(order - only moths) Above Above Old-Growth Forest Sp. Richness Median by Management Type",
       x = "Management Type",
       y = "% Above Median Moths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p15 <- ggplot(BDV_recovery_filtered, aes(x = management_type, y = `%_above_Q3_MOTHS`, fill = management_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Lepidoptera", #(order - only moths) Above Old-Growth Forest Sp. Richness Q3 by Management Type",
       x = "Management Type",
       y = "% Above Q3 Moths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot grid arrange
grid.arrange(p3,p2,p6,p5,p9,p8,p12,p11,p15,p14, ncol=3)

# Plot the q1, q2, q3 per taxa
grid.arrange(p1,p4,p7,p10,p13, ncol=2)

grid.arrange(p2,p5,p8,p11,p14, ncol=2)

grid.arrange(p3,p6,p9,p12,p15, ncol=2)

# plot the q1 + q2 + q3 in evry taxa

grid.arrange(p1,p2,p3, ncol=3)

grid.arrange(p4,p5,p6, ncol=3)

grid.arrange(p7,p8,p9, ncol=3)

grid.arrange(p10,p11,p12, ncol=3)

grid.arrange(p13,p14,p15, ncol=3)


#-------------------------------------------------------------------------------


library(dplyr)
library(ggplot2)
library(gridExtra)


# 🔹 Define fixed colors for management types
management_colors <- c(
  "Old-growth"   = "#1f77b4",  # Blue → Represents conservation and stability
  "Broadl" = "#ff7f0e",  # Orange → Associated with deciduous trees
  "Mixed"       = "#2ca02c",  # Green → Represents biodiversity and mixture
  "Conifer"  = "#d62728",  # Red → Symbolizing evergreen dominance
  "Transition"  = "#9467bd",  # Purple → Signifies change and transformation
  "Clearcutted" = "#8c564b"   # Brown → Reflecting soil exposure and disturbance
)

management_colors <- c(
  "Old-growth"   = "#3B9AB2",  # Elegant teal → Stability, conservation  
  "Broadl" = "#E69F00",  # Warm golden amber → Deciduous richness  
  "Mixed"       = "#009E73",  # Vibrant jade green → Diversity and life  
  "Conifer"  = "#D55E00",  # Burnt orange → Evergreen dominance  
  "Transition"  = "#CC79A7",  # Soft magenta → Transformation and change  
  "Clearcutted" = "#7A4E2D"   # Earthy cocoa brown → Soil and disturbance  
)

management_colors <- c(
  "Old-growth"   = "#3B9AB2",  # Elegant teal → Stability, conservation  
  "Broadl" = "#D55E00",  # Warm golden amber → Deciduous richness  
  "Mixed"       = "#E69F00",  # Vibrant jade green → Diversity and life  
  "Conifer"  = "darkolivegreen",  # Burnt orange → Evergreen dominance  
  "Transition"  = "#FF1493",  # Soft magenta → Transformation and change  
  "Clearcutted" = "#7A4E2D"   # Earthy cocoa brown → Soil and disturbance  
)

management_colors <- c(
  "Old-growth"   = "#3B9AB2",  # Elegant teal → Stability, conservation  
  "Broadl" = "#FF8247",  # Warm golden amber → Deciduous richness  
  "Mixed"       = "goldenrod1",  # Vibrant jade green → Diversity and life  
  "Conifer"  = "darkolivegreen",  # Burnt orange → Evergreen dominance  
  "Transition"  = "#9370DB",  # Soft magenta → Transformation and change  
  "Clearcutted" = "navajowhite3"   # Earthy cocoa brown → Soil and disturbance  
)

# FUNZIONE per generare i grafici per un dato taxa e quartile
plot_taxa_quartile <- function(df, taxa, quartile) {
  
  # Nome della colonna da analizzare
  col_name <- paste0("%_above_", quartile, "_", taxa)
  
  # 🔹 Filtra solo gli anni in cui la % è > 0 (tranne per G1)
  df_filtered <- df %>%
    filter(!!sym(col_name) > 0)
  
  # 1️⃣ (G1) Bar plot: Numero di anni in cui la % supera 0 per management type
  G1 <- df %>%
    filter(!!sym(col_name) > 0) %>%
    group_by(management_type) %>%
    summarise(years_above_0 = n()) %>%
    ggplot(aes(x = management_type, y = years_above_0, fill = management_type)) +
    geom_bar(stat = "identity") +
    labs(title = paste(taxa, quartile, "- Years Above 0"),
         x = "Management Type",
         y = "Years Above 0") +
    scale_fill_manual(values = management_colors) +  # 🔹 Fixed colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 2️⃣ (G2) Scatter plot: Year vs. % sopra la soglia, colorato per management type
  G2 <- ggplot(df_filtered, aes(x = year, y = !!sym(col_name), color = management_type)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE) +
    labs(title = paste(taxa, quartile, "- % Above Threshold per Year"),
         x = "Year",
         y = paste("% Above", quartile)) +
    scale_color_manual(values = management_colors) +  # 🔹 Fixed colors
    theme_minimal()
  
  # 3️⃣ (G3) Boxplot: Distribuzione delle % sopra la soglia per management type
  G3 <- ggplot(df_filtered, aes(x = management_type, y = !!sym(col_name), fill = management_type)) +
    geom_boxplot(outlier.color = "black", outlier.alpha = 0.1) +
    labs(title = paste(taxa, quartile, "- Distribution of % Above"),
         x = "Management Type",
         y = paste("% Above", quartile)) +
    scale_fill_manual(values = management_colors) +  # 🔹 Fixed colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 4️⃣ (G4) Boxplot per età in cui si supera la soglia
  G4 <- ggplot(df_filtered, aes(x = management_type, y = age, fill = management_type)) +
    geom_boxplot(outlier.shape = 1, outlier.color = "gray40", outlier.size = 1, outlier.alpha = 0.3) +
    labs(title = paste(taxa, quartile, "- Age Distribution Above Threshold"),
         x = "Management Type",
         y = "Age") +
    scale_fill_manual(values = management_colors) +  # 🔹 Fixed colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Mostra i 4 grafici insieme
  grid.arrange(G1, G2, G3, G4, ncol = 2)
}

# 🚀 Esempio per Bryophytes e quartile Median
plot_taxa_quartile(BDV_recovery_filtered, "BRYOPHYTES", "Median")

# Per generare tutti i grafici per ogni taxa e quartile:
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")
quartiles <- c("Q1", "Median", "Q3")

for (taxa in taxa_list) {
  for (quartile in quartiles) {
    plot_taxa_quartile(BDV_recovery_filtered, taxa, quartile)
  }
}



dev.off()

# write excel
writexl::write_xlsx(BDV_recovery, "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/BDV_recovery_data_all.xlsx")
