# Load required packages
#library(ggplot2)
#library(ggpubr)    # For stat_pvalue_manual
library(tidyr)
library(dplyr)
#library(rstatix)   # For Kruskal-Wallis and correlation tests
library(readxl)    # For reading Excel files

# Load the data
BDV_predictors <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/20_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood_tes_Copy.xlsx")

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
table(BDV_predictors$forest_cat) # here you see the divisions between old growth coniferous and broadleaves
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
BDV_recovery_filtered <- BDV_recovery %>% filter(year >= 0 & year <= 350)

#-------------------------------------------------------------------------------
# Example Data (assuming the data is already loaded as 'data_processed')
# You would replace the below example data with your actual data
# data_processed <- your_data

library(ggplot2)
library(gridExtra)

# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
pdf(paste0(dataroot, "BDV_recovery_all_V5.pdf"), height=9, width=16)

# BRYOPHYTES--------------------------------------------------------------------
# Bar plot of `%_above_Median_BRYOPHYTES` by Forest Category
P1 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Median_BRYOPHYTES`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Bryophytes Above Median by Forest Category",
       x = "Forest Category",
       y = "% of Predictions Above Median Bryophytes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Faceted plot of `%_above_Median_BRYOPHYTES` by Forest Category and year
P2 <- ggplot(BDV_recovery_filtered, aes(x = year, y = `%_above_Median_BRYOPHYTES`, color = forest_cat)) +
  geom_line() +
  facet_wrap(~ forest_cat) +
  labs(title = "Bryophytes Predictions Above PR Sp.Richness Median by Forest Category and Year",
       x = "Year",
       y = "% of Predictions Above Median Bryophytes") +
  theme_minimal()


# Plot grid arrange
grid.arrange(P1,P2, ncol=2)

# LICHENS-----------------------------------------------------------------------
# Bar plot of `%_above_Median_LICHENS` by Forest Category
P1 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Median_LICHENS`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Lichens Above Median by Forest Category",
       x = "Forest Category",
       y = "% Above Median Lichens") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Faceted plot of `%_above_Median_LICHENS` by Forest Category and year
P2 <- ggplot(BDV_recovery_filtered, aes(x = year, y = `%_above_Median_LICHENS`, color = forest_cat)) +
  geom_line() +
  facet_wrap(~ forest_cat) +
  labs(title = "Lichens Above Median by Forest Category and Year",
       x = "Year",
       y = "% Above Median Lichens") +
  theme_minimal()


# Plot grid arrange
grid.arrange(P1,P2, ncol=2)

# MACROFUNGI-----------------------------------------------------------------------
# Bar plot of `%_above_Median_MACROFUNGI` by Forest Category
P1 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Median_MACROFUNGI`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Macrofungi Above Median by Forest Category",
       x = "Forest Category",
       y = "% Above Median Macrofungi") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Faceted plot of `%_above_Median_MACROFUNGI` by Forest Category and year
P2 <- ggplot(BDV_recovery_filtered, aes(x = year, y = `%_above_Median_MACROFUNGI`, color = forest_cat)) +
  geom_line() +
  facet_wrap(~ forest_cat) +
  labs(title = "Macrofungi Above Median by Forest Category and Year",
       x = "Year",
       y = "% Above Median Macrofungi") +
  theme_minimal()


# Plot grid arrange
grid.arrange(P1,P2, ncol=2)


# BEETLES-----------------------------------------------------------------------
# Bar plot of `%_above_Median_BEETLES` by Forest Category
P1 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Median_BEETLES`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Beetles Above Median by Forest Category",
       x = "Forest Category",
       y = "% Above Median Beetles") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Faceted plot of `%_above_Median_BEETLES` by Forest Category and year
P2 <- ggplot(BDV_recovery_filtered, aes(x = year, y = `%_above_Median_BEETLES`, color = forest_cat)) +
  geom_line() +
  facet_wrap(~ forest_cat) +
  labs(title = "Beetles Above Median by Forest Category and Year",
       x = "Year",
       y = "% Above Median Beetles") +
  theme_minimal()


# Plot grid arrange
grid.arrange(P1,P2, ncol=2)


# MOTHS-----------------------------------------------------------------------
# Bar plot of `%_above_Median_BEETLES` by Forest Category
P1 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Median_MOTHS`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Moths Above Median by Forest Category",
       x = "Forest Category",
       y = "% Above Median Moths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Faceted plot of `%_above_Median_BEETLES` by Forest Category and year
P2 <- ggplot(BDV_recovery_filtered, aes(x = year, y = `%_above_Median_MOTHS`, color = forest_cat)) +
  geom_line() +
  facet_wrap(~ forest_cat) +
  labs(title = "Moths Above Median by Forest Category and Year",
       x = "Year",
       y = "% Above Median Moths") +
  theme_minimal()


# Plot grid arrange
grid.arrange(P1,P2, ncol=2)



#-------------------------------------------------------------------------------
# Bar plot of `%_above_Median_BRYOPHYTES` by Forest Category

# Bryophyta
p1 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Q1_BRYOPHYTES`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Bryophyta ", #(phylum) Above Old-Growth Forest Sp. Richness Q1 by Forest Category",
       x = "Forest Category",
       y = "% Above Q1 Bryophyta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Median_BRYOPHYTES`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Bryophyta ", #(phylum) Above Old-Growth Forest Sp. Richness Median by Forest Category",
       x = "Forest Category",
       y = "% Above Median Bryophyta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Q3_BRYOPHYTES`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Bryophyta ", #(phylum) Above Old-Growth Forest Sp. Richness Q3 by Forest Category",
       x = "Forest Category",
       y = "% Above Q3 Bryophyta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Lichens

p4 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Q1_LICHENS`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Lichens ", #(informal group) Above Old-Growth Forest Sp. Richness Q1 by Forest Category",
       x = "Forest Category",
       y = "% Above Q1 Lichenes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p5 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Median_LICHENS`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Lichens ", #(informal group) Above Old-Growth Forest Sp. Richness Median by Forest Category",
       x = "Forest Category",
       y = "% Above Median Lichenes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p6 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Q3_LICHENS`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Lichens ", #(informal group) Above Old-Growth Forest Sp. Richness Q3 by Forest Category",
       x = "Forest Category",
       y = "% Above Q3 Lichenes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Beetles

p7 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Q1_BEETLES`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Coleoptera ", #(order - mainly saproxylic) Above Old-Growth Forest Sp. Richness Q1 by Forest Category",
       x = "Forest Category",
       y = "% Above Q1 Coleoptera") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p8 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Median_BEETLES`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Coleoptera ", #(order - mainly saproxylic) Above Old-Growth Forest Sp. Richness Median by Forest Category",
       x = "Forest Category",
       y = "% Above Median Coleoptera") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p9 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Q3_BEETLES`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Coleoptera ", #(order - mainly saproxylic) Above Old-Growth Forest Sp. Richness Q3 by Forest Category",
       x = "Forest Category",
       y = "% Above Q3 Coleoptera") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Macrofungi

p10 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Q1_MACROFUNGI`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Fungi ", #(kingdom) - Basidiomycota / Ascomycota (phyla principali) Above Old-Growth Forest Sp. Richness Q1 by Forest Category",
       x = "Forest Category",
       y = "% Above Q1 Macrofungi") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p11 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Median_MACROFUNGI`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Fungi ", #(kingdom) - Basidiomycota / Ascomycota (main phyla) Above Old-Growth Forest Sp. Richness Median by Forest Category",
       x = "Forest Category",
       y = "% Above Median Macrofungi") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p12 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Q3_MACROFUNGI`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Fungi ", #(kingdom) - Basidiomycota / Ascomycota (phyla principali) Above Old-Growth Forest Sp. Richness Q1 by Forest Category",
       x = "Forest Category",
       y = "% Above Q3 Macrofungi") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Moths

p13 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Q1_MOTHS`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Lepidoptera", #(order - only moths) Above Old-Growth Forest Sp. Richness Q1 by Forest Category",
       x = "Forest Category",
       y = "% Above Q1 Moths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p14 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Median_MOTHS`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Lepidoptera", #(order - only moths) Above Above Old-Growth Forest Sp. Richness Median by Forest Category",
       x = "Forest Category",
       y = "% Above Median Moths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p15 <- ggplot(BDV_recovery_filtered, aes(x = forest_cat, y = `%_above_Q3_MOTHS`, fill = forest_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Lepidoptera", #(order - only moths) Above Old-Growth Forest Sp. Richness Q3 by Forest Category",
       x = "Forest Category",
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


# ???? Define fixed colors for Forest Categorys
management_colors <- c(
  "Old-Growth"   = "#1f77b4",  # Blue ??? Represents conservation and stability
  "Native Broadleaves" = "#ff7f0e",  # Orange ??? Associated with deciduous trees
  "Non-Native Coniferous"  = "#2ca02c")

management_colors <- c(
  "Old-Growth"   = "#3B9AB2",  # Elegant teal ??? Stability, conservation  
  "Beech-Oak" = "#E69F00",  # Warm golden amber ??? Deciduous richness 
  "Conifer"  = "#009E73"  # Burnt orange ??? Evergreen dominance 
)

management_colors <- c(
  "Old-Growth"   = "#3B9AB2",  # Elegant teal ??? Stability, conservation  
  "Beech-Oak" = "#D55E00",  # Warm golden amber ??? Deciduous richness 
  "Conifer"  = "darkolivegreen" 
)

management_colors <- c(
  "Old-Growth"   = "#3B9AB2",  # Elegant teal ??? Stability, conservation  
  "Native Broadleaves" = "#FF8247",  # Warm golden amber ??? Deciduous richness  
  "Non-Native Coniferous"  = "darkolivegreen" 
)

# FUNZIONE per generare i grafici per un dato taxa e quartile
plot_taxa_quartile <- function(df, taxa, quartile) {
  
  # Nome della colonna da analizzare
  col_name <- paste0("%_above_", quartile, "_", taxa)
  
  # ???? Filtra solo gli anni in cui la % ?? > 0 (tranne per G1)
  df_filtered <- df %>%
    filter(!!sym(col_name) > 0)
  
  # 1?????? (G1) Bar plot: Numero di anni in cui la % supera 0 per Forest Category
  G1 <- df %>%
    filter(!!sym(col_name) > 0) %>%
    group_by(forest_cat) %>%
    summarise(years_above_0 = n()) %>%
    ggplot(aes(x = forest_cat, y = years_above_0, fill = forest_cat)) +
    geom_bar(stat = "identity") +
    labs(title = paste(taxa, quartile, "- Years Above 0"),
         x = "Forest Category",
         y = "Years Above 0") +
    scale_fill_manual(values = management_colors) +  # ???? Fixed colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 2?????? (G2) Scatter plot: Year vs. % sopra la soglia, colorato per Forest Category
  G2 <- ggplot(df_filtered, aes(x = year, y = !!sym(col_name), color = forest_cat)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "loess", se = FALSE, size = 1.5, span = 0.3) + # Lower span = more wiggly curve
    labs(title = paste(taxa, quartile, "- % Above Threshold per Year"),
         x = "Year",
         y = paste("% Above", quartile)) +
    scale_color_manual(values = management_colors) +  # ???? Fixed colors
    theme_minimal()
  
  # 3?????? (G3) Boxplot: Distribuzione delle % sopra la soglia per Forest Category
  G3 <- ggplot(df_filtered, aes(x = forest_cat, y = !!sym(col_name), fill = forest_cat)) +
    geom_boxplot(outlier.color = "black", outlier.alpha = 0.1) +
    labs(title = paste(taxa, quartile, "- Distribution of % Above"),
         x = "Forest Category",
         y = paste("% Above", quartile)) +
    scale_fill_manual(values = management_colors) +  # ???? Fixed colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 4?????? (G4) Boxplot per et?? in cui si supera la soglia
  G4 <- ggplot(df_filtered, aes(x = forest_cat, y = age, fill = forest_cat)) +
    geom_boxplot(outlier.shape = 1, outlier.color = "gray40", outlier.size = 1, outlier.alpha = 0.3) +
    labs(title = paste(taxa, quartile, "- Age Distribution Above Threshold"),
         x = "Forest Category",
         y = "Age") +
    scale_fill_manual(values = management_colors) +  # ???? Fixed colors
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Mostra i 4 grafici insieme
  grid.arrange(G1, G2, G3, G4, ncol = 2)
}

# ???? Esempio per Bryophytes e quartile Median
# plot_taxa_quartile(BDV_recovery_filtered, "BRYOPHYTES", "Median")

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
writexl::write_xlsx(BDV_recovery_filtered, "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/BDV_recovery_data_all_V5.xlsx")


## SECOND PART OF THE ANALYSIS WHERE WE CALCULATE THE YEAR OF BDV RECOVERY - 10% OF THE VALUES >= OF THE Q1,Q2 OR Q3 THRESHOLD.
library(readxl)
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(stringr)

#------------------
BDV_recovery_data <- read_xlsx("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/BDV_recovery_data_all_V5.xlsx")
BDV_recovery_data

#------------------
# 1?????? Initial recovery stage: first row (year == 0) per plot
initial_recovery_stage <- BDV_recovery_data %>%
  filter(year == 0) %>%
  group_by(plotID) %>%
  slice(1) %>%
  ungroup()

initial_recovery_stage

# 2?????? Recovery years (YoR) per plot and taxa-threshold. The YoR will be assigned at every % threshold Taxa richness, so instead then the % reaching the threshold now you will have the year in which at least 10% recovered.
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
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
pdf(paste0(dataroot, "BDV_recovery_V5.pdf"), height=9, width=16)

#--------------------
management_colors <- c(
  "Old-Growth"   = "#3B9AB2",  # Elegant teal ??? Stability, conservation  
  "Native Broadleaves" = "#FF8247",  # Warm golden amber ??? Deciduous richness  
  "Non-Native Coniferous"  = "darkolivegreen" 
)


# ??????? Function to generate 2x3 plots for a given taxa
plot_taxa_overview <- function(df, taxa_name) {
  df_taxa <- df %>%
    filter(str_detect(taxa_threshold, taxa_name))
  
  quartiles <- c("Q1", "Median", "Q3")
  
  # ???? Generate plots per quartile
  plots <- lapply(quartiles, function(q) {
    df_q <- df_taxa %>%
      filter(str_detect(taxa_threshold, q))
    
    # ???? Scatter plot
    scatter <- ggplot(df_q, aes(x = YoR, y = age, color = forest_cat)) +
      geom_point(alpha = 0.7, size = 2.5) +
      geom_smooth(method = "lm", se = FALSE, size = 1.5, span = 0.3) +
      labs(title = paste(taxa_name, "-", q, "Scatter"), x = "YoR", y = "Age") +
      scale_color_manual(values = management_colors) +
      theme_minimal() +
      theme(legend.position = "none")
    
    # ???? Boxplot
    box <- ggplot(df_q, aes(x = forest_cat, y = YoR, fill = forest_cat)) +
      geom_boxplot(outlier.shape = 21, outlier.size = 1.5, outlier.color = "black") +
      labs(title = paste(taxa_name, "-", q, "Boxplot"), x = "Forest Categories", y = "YoR") +
      scale_fill_manual(values = management_colors) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    list(scatter, box)
  })
  
  # ???? Arrange plots in a 2x3 grid (scatter on top, box below)
  grid.arrange(
    plots[[1]][[1]], plots[[2]][[1]], plots[[3]][[1]],
    plots[[1]][[2]], plots[[2]][[2]], plots[[3]][[2]],
    ncol = 3,
    top = paste("Taxa Overview:", taxa_name)
  )
}

# ???? Generate plots for all taxa
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")
for (taxa in taxa_list) {
  plot_taxa_overview(recovery_long, taxa)
}


# LET'S DO THE SAME VISUALIZATION BUT WITH A SCATTER PLOT AND A BOXPLOT OF THE RECOVERY FOR EVERY QUARTILE AND TAXA


# ??????? Function to generate 1x2 plots (scatter + boxplot) per taxa and quartile
plot_taxa_quartile_1x2 <- function(df, taxa_name, quartile) {
  df_filtered <- df %>%
    filter(str_detect(taxa_threshold, taxa_name) & str_detect(taxa_threshold, quartile))
  
  # ???? Scatter plot
  scatter <- ggplot(df_filtered, aes(x = YoR, y = age, color = forest_cat)) +
    geom_point(alpha = 0.7, size = 2.5) +
    labs(title = paste(taxa_name, "-", quartile, "Scatter Plot"), x = "YoR", y = "Init Age") +
    scale_color_manual(values = management_colors) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # ???? Boxplot
  box <- ggplot(df_filtered, aes(x = forest_cat, y = YoR, fill = forest_cat)) +
    geom_boxplot(outlier.shape = 21, outlier.size = 1.5, outlier.color = "black") +
    labs(title = paste(taxa_name, "-", quartile, "Boxplot"), x = "Forest Categories", y = "YoR") +
    scale_fill_manual(values = management_colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  # ???? Combine scatter + boxplot in 1x2 layout
  grid.arrange(scatter, box, ncol = 2, top = paste("Taxa:", taxa_name, "| Quartile:", quartile))
}

# ???? Loop over taxa and quartiles to generate all plots
taxa_list <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")
quartiles <- c("Q1", "Median", "Q3")

for (taxa in taxa_list) {
  for (q in quartiles) {
    plot_taxa_quartile_1x2(recovery_long, taxa, q)
  }
}

#--------
dev.off()

##################    THE END     ################