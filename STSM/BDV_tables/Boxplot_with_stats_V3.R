############################################ FIRST METHOD
# Load required packages
library(ggplot2)
library(ggpubr)    # Required for stat_pvalue_manual
library(tidyr)
library(dplyr)
library(rstatix)   # For Kruskal-Wallis and correlation tests
library(readxl)    # For reading Excel files


# BDV predictors by Jenik
BDV_predictors <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/20_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood.xlsx")

#BDV_predictors <- BDV_predictors%>%
#  group_by(plotID)%>%
#  mutate(deadwood50 = (deadwood/4))


BDV_predictors
# Reshape data: Convert taxonomic groups from columns to rows
BDV_long <- BDV_predictors %>%
  pivot_longer(cols = c(`Epiphytic / epixilic bryophytes (0.212)`, 
                        `Lichens (0.137)`, 
                        `Macrofungi (2.118)`, 
                        `Non-flying beetles (0.053)`, 
                        `Moths (0.566)`), 
               names_to = "Taxonomic_Group", 
               values_to = "Species_Count")

# Create the boxplot
ggplot(BDV_long, aes(x = management_type, y = Species_Count)) +
  geom_boxplot(aes(fill = management_type)) + 
  facet_wrap(~Taxonomic_Group, scales = "free") + 
  theme_minimal() + 
  labs(title = "Species Richness per Taxonomic Group across Management Types",
       x = "Management Type", y = "Number of Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





################################################################################ SECOND METHOD
# Load the data
BDV_predictors <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/20_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood.xlsx")

# Rename problematic column names
BDV_predictors <- BDV_predictors %>%
  rename(broadl_40 = `broadl>40`)  # Replace `>` with `_` for easier handling

# Select species richness columns (EXCLUDE red-listed groups and binary 0/1 column)
species_cols <- c("Epiphytic / epixilic bryophytes (0.212)", 
                  "Lichens (0.137)", "Macrofungi (2.118)", 
                  "Non-flying beetles (0.053)", "Moths (0.566)")

# Select predictor columns for statistical testing
predictor_cols <- c("age", "deadwood", "lai", "ba_broadl_spec", 
                    "trees_10_40", "broadl_40")  

# Reshape species richness data to long format
BDV_species_long <- BDV_predictors %>%
  pivot_longer(cols = species_cols, 
               names_to = "Species", 
               values_to = "Richness")

# Extract site information from plotID (L1, L2, etc.)
BDV_species_long <- BDV_species_long %>%
  mutate(Site = substr(plotID, 1, 2))


# Prepare statistical tests: Correlation between species richness and predictors
stat_tests <- BDV_species_long %>%
  left_join(BDV_predictors %>% select(plotID), by = "plotID") %>%
  pivot_longer(cols = predictor_cols, names_to = "Predictor", values_to = "Predictor_Value") %>%
  group_by(Species, Predictor) %>%
  cor_test(Richness, Predictor_Value, method = "spearman") %>%  # Spearman correlation
  mutate(p.signif = case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "ns"  # Not significant
  ))

# Plot species richness across management types with significance
ggplot(BDV_species_long, aes(x = management_type, y = Richness)) +
  geom_boxplot(fill = "gray70", color = "black", outlier.shape = NA) +  # Boxplots in gray
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  # Dots colored by site
  facet_wrap(~Species, scales = "free", ncol=5) +  # Facet for each species group, important scales free to leave the y scale free
  theme_minimal() +
  labs(title = "Species Richness Across Management Types",
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")  



############################################ THIRD METHOD

# Load the data
BDV_predictors <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/20_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood.xlsx")

# Rename problematic column names
BDV_predictors <- BDV_predictors %>%
  rename(broadl_40 = `broadl>40`)  

# Select species richness columns (EXCLUDE red-listed groups and binary 0/1 column)
species_cols <- c("Epiphytic / epixilic bryophytes (0.212)", 
                  "Lichens (0.137)", "Macrofungi (2.118)", 
                  "Non-flying beetles (0.053)", "Moths (0.566)")

# Reshape species richness data to long format
BDV_species_long <- BDV_predictors %>%
  pivot_longer(cols = species_cols, 
               names_to = "Species", 
               values_to = "Richness")

# Extract site information from plotID (L1, L2, etc.)
BDV_species_long <- BDV_species_long %>%
  mutate(Site = substr(plotID, 1, 2))

# Define the five species names
species_names <- c("Epiphytic / epixilic bryophytes (0.212)", 
                   "Lichens (0.137)", "Macrofungi (2.118)", 
                   "Non-flying beetles (0.053)", "Moths (0.566)")

# G1: Plot for first species
G1 <- ggplot(subset(BDV_species_long, Species == species_names[1]), 
             aes(x = management_type, y = Richness)) +
  geom_boxplot(fill = "gray70", color = "black", outlier.shape = NA) +  
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  
  theme_minimal() +
  labs(title = paste("G1 - Species Richness for", species_names[1]),
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
print(G1)

# G2: Plot for second species
G2 <- ggplot(subset(BDV_species_long, Species == species_names[2]), 
             aes(x = management_type, y = Richness)) +
  geom_boxplot(fill = "gray70", color = "black", outlier.shape = NA) +  
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  
  theme_minimal() +
  labs(title = paste("G2 - Species Richness for", species_names[2]),
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
print(G2)

# G3: Plot for third species
G3 <- ggplot(subset(BDV_species_long, Species == species_names[3]), 
             aes(x = management_type, y = Richness)) +
  geom_boxplot(fill = "gray70", color = "black", outlier.shape = NA) +  
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  
  theme_minimal() +
  labs(title = paste("G3 - Species Richness for", species_names[3]),
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
print(G3)

# G4: Plot for fourth species
G4 <- ggplot(subset(BDV_species_long, Species == species_names[4]), 
             aes(x = management_type, y = Richness)) +
  geom_boxplot(fill = "gray70", color = "black", outlier.shape = NA) +  
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  
  theme_minimal() +
  labs(title = paste("G4 - Species Richness for", species_names[4]),
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
print(G4)

# G5: Plot for fifth species
G5 <- ggplot(subset(BDV_species_long, Species == species_names[5]), 
             aes(x = management_type, y = Richness)) +
  geom_boxplot(fill = "gray70", color = "black", outlier.shape = NA) +  
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  
  theme_minimal() +
  labs(title = paste("G5 - Species Richness for", species_names[5]),
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
print(G5)


#------------------------------------------------------------------------------- To save plots



# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
#dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
#pdf(paste0(dataroot, "boxplot.pdf"), height=8, width=12)
#dev.off()

