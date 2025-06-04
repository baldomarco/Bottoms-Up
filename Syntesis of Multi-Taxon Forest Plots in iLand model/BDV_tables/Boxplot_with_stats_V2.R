# Load required packages
library(ggplot2)
library(ggpubr)    # For stat_pvalue_manual
library(tidyr)
library(dplyr)
library(rstatix)   # For Kruskal-Wallis and correlation tests
library(readxl)    # For reading Excel files




# --- Define the color palette --- #
management_colors <- c(
  "Old-Growth" = "#3B9AB2",  # Elegant teal → Stability, conservation  
  "Native Broadleaves" = "#FF8247",  # Warm golden amber → Deciduous richness  
  "Non-Native Coniferous" = "darkolivegreen"  # Earthy green → Non-native conifers
)


# Load the data
BDV_predictors <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/20_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood_tes - Copy.xlsx")

# Remake the names in the table for a better understanding and coding
BDV_predictors_cleaned <- BDV_predictors %>%
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
table(BDV_predictors_cleaned$forest_cat)
glimpse(BDV_predictors_cleaned)
BDV_predictors_cleaned

# Reshape species richness data to long format
BDV_species_long <- BDV_predictors_cleaned %>%
  pivot_longer(cols = c("Bryophytes", "Lichens", "Macrofungi", "Beetles", "Moths"), 
               names_to = "Species", 
               values_to = "Richness")

# Extract site information from plotID (L1, L2, etc.)
BDV_species_long <- BDV_species_long %>%
  mutate(Site = substr(plotID, 1, 2))

# Perform Kruskal-Wallis test to compare species richness across management types
stat_tests <- BDV_species_long %>%
  group_by(Species,Site) %>%
  kruskal_test(Richness ~ forest_cat)

# Perform Kruskal-Wallis test to compare species richness across management types
stat_tests <- BDV_species_long %>%
  group_by(Species) %>%
  kruskal_test(Richness ~ forest_cat)

# Create pairwise comparisons for the Kruskal-Wallis test results
pairwise_comparisons <- BDV_species_long %>%
  group_by(Species) %>%
  pairwise_wilcox_test(Richness ~ forest_cat, p.adjust.method = "bonferroni")

# Add significance labels (asterisks) based on p-value thresholds
pairwise_comparisons <- pairwise_comparisons %>%
  mutate(p.signif = case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "ns"
  ))

#-------------------------------------------------------------------------------
# PLOT THE SPECIES RICHNESS AS BOXPLOTS WITH K-W STAT TEST BETWEEN FOREST CATEGORIES
#-------------------------------------------------------------------------------

# Create the plot for Bryophytes (G1)
G1 <- ggplot(BDV_species_long %>% filter(Species == "Bryophytes"), aes(x = forest_cat, y = Richness)) +
  geom_boxplot(aes(fill = forest_cat), color = "black", outlier.shape = NA) +  # Boxplots
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  # Dots
  stat_pvalue_manual(pairwise_comparisons %>%
                       filter(Species == "Bryophytes"), 
                     label = "p.signif", y.position = 42) +  # Add significance just above the boxplot max absolute richness + 5
  theme_minimal() +
  scale_fill_manual(values = management_colors) +  # Fixed colors
  labs(title = "Species Richness for Bryophytes - Kruskal-Wallis test",  # Updated title with statistical test
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

G1

# Create the plot for Lichens (G2)
G2 <- ggplot(BDV_species_long %>% filter(Species == "Lichens"), aes(x = forest_cat, y = Richness)) +
  geom_boxplot(aes(fill = forest_cat), color = "black", outlier.shape = NA) +  # Boxplots
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  # Dots
  stat_pvalue_manual(pairwise_comparisons %>%
                       filter(Species == "Lichens"), 
                     label = "p.signif", y.position = 76) +  # Add significance just above the boxplot max absolute richness + 5
  theme_minimal() +
  scale_fill_manual(values = management_colors) +  # Fixed colors
  labs(title = "Species Richness for Lichens - Kruskal-Wallis test",  # Updated title with statistical test
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

G2

# Create the plot for Macrofungi (G3)
G3 <- ggplot(BDV_species_long %>% filter(Species == "Macrofungi"), aes(x = forest_cat, y = Richness)) +
  geom_boxplot(aes(fill = forest_cat), color = "black", outlier.shape = NA) +  # Boxplots
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  # Dots
  stat_pvalue_manual(pairwise_comparisons %>%
                       filter(Species == "Macrofungi"), 
                     label = "p.signif", y.position = 289) +  # Add significance just above the boxplot max absolute richness + 5
  theme_minimal() +
  scale_fill_manual(values = management_colors) +  # Fixed colors
  labs(title = "Species Richness for Macrofungi - Kruskal-Wallis test",            # Updated title with statistical test
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

G3

# Create the plot for Beetles (G4)
G4 <- ggplot(BDV_species_long %>% filter(Species == "Beetles"), aes(x = forest_cat, y = Richness)) +
  geom_boxplot(aes(fill = forest_cat), color = "black", outlier.shape = NA) +  # Boxplots
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  # Dots
  stat_pvalue_manual(pairwise_comparisons %>%
                       filter(Species == "Beetles"), 
                     label = "p.signif", y.position = 20) +  # Add significance just above the boxplot
  theme_minimal() +
  scale_fill_manual(values = management_colors) +  # Fixed colors
  labs(title = "Species Richness for Beetles - Kruskal-Wallis test",  # Updated title with statistical test
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

G4


# Create the plot for Moths (G5)
G5 <- ggplot(BDV_species_long %>% filter(Species == "Moths"), aes(x = forest_cat, y = Richness)) +
  geom_boxplot(aes(fill = forest_cat), color = "black", outlier.shape = NA) +  # Boxplots
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  # Dots
  stat_pvalue_manual(pairwise_comparisons %>%
                       filter(Species == "Moths"), 
                     label = "p.signif", y.position = 111) +  # Add significance just above the boxplot
  theme_minimal() +
  scale_fill_manual(values = management_colors) +  # Fixed colors
  labs(title = "Species Richness for Moths - Kruskal-Wallis test",  # Updated title with statistical test
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

G5


#-------------------------------------------------------------------------------
# PLOT THE SPECIES RICHNESS AS BOXPLOTS WITH K-W STAT TEST APPLIED ON TAXON DATA
#-------------------------------------------------------------------------------

###########################################
#         GOOOOOOOOOOOOOOOOOOOD
############################################

# Perform Kruskal-Wallis test for each species (no grouping by management type)
stat_tests <- BDV_species_long %>%
  group_by(Species) %>%
  kruskal_test(Richness ~ forest_cat) %>%
  mutate(p.signif = case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "ns"
  ))

# Get max y values for positioning
max_y_values <- BDV_species_long %>%
  group_by(Species) %>%
  summarize(y_max = max(Richness, na.rm = TRUE))

# Merge p-values with max_y_values
stat_tests <- stat_tests %>%
  left_join(max_y_values, by = "Species") %>%
  mutate(y.position = y_max)  

# Bryophytes--------------------------------------------------------------------

# Select species (G1 example: Bryophytes)
species_name <- "Bryophytes"
filtered_data <- BDV_species_long %>% filter(Species == species_name)
filtered_pvalues <- stat_tests %>% filter(Species == species_name)

# Add one single p-value annotation (above entire plot)
G1 <- ggplot(filtered_data, aes(x = forest_cat, y = Richness)) +
  geom_boxplot(aes(fill = forest_cat), color = "black", outlier.shape = NA) +  
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  
  annotate("text", x = 1.5, y = max(filtered_data$Richness, na.rm = TRUE) + 10, 
           label = paste("KW p-value:", filtered_pvalues$p.signif), size = 5) +  
  theme_minimal() +
  scale_fill_manual(values = management_colors) +  # Fixed colors
  labs(title = paste("Species Richness for", species_name, "- Kruskal-Wallis Test"),  
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Print the plot
print(G1)

# Lichens --------------------------------------------------------------------

# Select species (G1 example: Bryophytes)
species_name <- "Lichens"
filtered_data <- BDV_species_long %>% filter(Species == species_name)
filtered_pvalues <- stat_tests %>% filter(Species == species_name)

# Add one single p-value annotation (above entire plot)
G2 <- ggplot(filtered_data, aes(x = forest_cat, y = Richness)) +
  geom_boxplot(aes(fill = forest_cat), color = "black", outlier.shape = NA) +  
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  
  annotate("text", x = 1.5, y = max(filtered_data$Richness, na.rm = TRUE) + 10, 
           label = paste("KW p-value:", filtered_pvalues$p.signif), size = 5) +  
  theme_minimal() +
  scale_fill_manual(values = management_colors) +  # Fixed colors
  labs(title = paste("Species Richness for", species_name, "- Kruskal-Wallis Test"),  
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Print the plot
print(G2)

# Macromycetes --------------------------------------------------------------------

# Select species (G1 example: Bryophytes)
species_name <- "Macrofungi"
filtered_data <- BDV_species_long %>% filter(Species == species_name)
filtered_pvalues <- stat_tests %>% filter(Species == species_name)

# Add one single p-value annotation (above entire plot)
G3 <- ggplot(filtered_data, aes(x = forest_cat, y = Richness)) +
  geom_boxplot(aes(fill = forest_cat), color = "black", outlier.shape = NA) +  
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  
  annotate("text", x = 1.5, y = max(filtered_data$Richness, na.rm = TRUE) + 10, 
           label = paste("KW p-value:", filtered_pvalues$p.signif), size = 5) +  
  theme_minimal() +
  scale_fill_manual(values = management_colors) +  # Fixed colors
  labs(title = paste("Species Richness for", species_name, "- Kruskal-Wallis Test"),  
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Print the plot
print(G3)


# Beetles --------------------------------------------------------------------

# Select species (G1 example: Bryophytes)
species_name <- "Beetles"
filtered_data <- BDV_species_long %>% filter(Species == species_name)
filtered_pvalues <- stat_tests %>% filter(Species == species_name)

# Add one single p-value annotation (above entire plot)
G4 <- ggplot(filtered_data, aes(x = forest_cat, y = Richness)) +
  geom_boxplot(aes(fill = forest_cat), color = "black", outlier.shape = NA) +  
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  
  annotate("text", x = 1.5, y = max(filtered_data$Richness, na.rm = TRUE) + 10, 
           label = paste("KW p-value:", filtered_pvalues$p.signif), size = 5) +  
  theme_minimal() +
  scale_fill_manual(values = management_colors) +  # Fixed colors
  labs(title = paste("Species Richness for", species_name, "- Kruskal-Wallis Test"),  
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Print the plot
print(G4)


# Moths --------------------------------------------------------------------

# Select species (G1 example: Bryophytes)
species_name <- "Moths"
filtered_data <- BDV_species_long %>% filter(Species == species_name)
filtered_pvalues <- stat_tests %>% filter(Species == species_name)

# Add one single p-value annotation (above entire plot)
G5 <- ggplot(filtered_data, aes(x = forest_cat, y = Richness)) +
  geom_boxplot(aes(fill = forest_cat), color = "black", outlier.shape = NA) +  
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  
  annotate("text", x = 1.5, y = max(filtered_data$Richness, na.rm = TRUE) + 10, 
           label = paste("KW p-value:", filtered_pvalues$p.signif), size = 5) +  
  theme_minimal() +
  scale_fill_manual(values = management_colors) +  # Fixed colors
  labs(title = paste("Species Richness for", species_name, "- Kruskal-Wallis Test"),  
       x = "Management Type", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Print the plot
print(G5)



#------------------------------------------------------------------------------- To save plots



# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
pdf(paste0(dataroot, "02_boxplot_with_stats.pdf"), height=8, width=12)
dev.off()

