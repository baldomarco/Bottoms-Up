############################################ FIRST METHOD
# Load required packages
library(ggplot2)
library(ggpubr)    # Required for stat_pvalue_manual
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


# Reshape data: Convert taxonomic groups from columns to rows
BDV_long <- BDV_predictors_cleaned %>%
  pivot_longer(cols = c("Bryophytes", "Lichens", "Macrofungi", "Beetles", "Moths"), 
               names_to = "Taxonomic_Group", 
               values_to = "Species_Count")

# Create the boxplot
ggplot(BDV_long, aes(x = forest_cat, y = Species_Count)) +
  geom_boxplot(aes(fill = forest_cat)) + 
  facet_wrap(~Taxonomic_Group, scales = "free", ncol=5) + 
  theme_minimal() + 
  scale_fill_manual(values = management_colors) +  # Fixed colors
  labs(title = "Species Richness per Taxonomic Group across Forest Categories",
       x = "Forest Categories", y = "Number of Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create the boxplot with larger text
ggplot(BDV_long, aes(x = forest_cat, y = Species_Count)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(aes(fill = forest_cat)) + 
  facet_wrap(~Taxonomic_Group, scales = "free", ncol = 5) + 
  theme_minimal() + 
  scale_fill_manual(values = management_colors) +  # Fixed colors
  labs(title = "Species Richness per Taxonomic Group across Forest Categories",
       x = "Forest Categories", y = "Number of Species") +
  theme(
    plot.title = element_text(size = 19, face = "bold"),        # Title
    axis.title = element_text(size = 15),                       # Axis titles
    axis.text = element_text(size = 11),                        # Axis numbers
    axis.text.x = element_text(angle = 45, hjust = 1),          # Rotated x labels
    legend.title = element_text(size = 15),                     # Legend title
    legend.text = element_text(size = 11),                      # Legend labels
    strip.text = element_text(size = 15)                        # Facet labels
  )

# third way
ggplot(BDV_long, aes(x = forest_cat, y = Species_Count)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(aes(fill = forest_cat)) + 
  facet_wrap(~Taxonomic_Group, scales = "free", ncol = 5) + 
  theme_minimal() + 
  scale_fill_manual(values = management_colors) +  # Fixed colors
  scale_y_continuous(
    breaks = seq(0, max(BDV_long$Species_Count), by = 5),  # Adjust step size as needed
    labels = scales::comma_format()  # Use commas for better readability
  ) +
  labs(title = "Species Richness per Taxonomic Group across Forest Categories",
       x = "Forest Categories", y = "Number of Species") +
  theme(
    plot.title = element_text(size = 19, face = "bold"),        # Title
    axis.title = element_text(size = 15),                       # Axis titles
    axis.text = element_text(size = 11),                        # Axis numbers
    axis.text.x = element_text(angle = 45, hjust = 1),          # Rotated x labels
    legend.title = element_text(size = 15),                     # Legend title
    legend.text = element_text(size = 11),                      # Legend labels
    strip.text = element_text(size = 15),                       # Facet labels
    panel.grid.major = element_blank(),                         # Remove major gridlines
    panel.grid.minor = element_blank(),                         # Remove minor gridlines
    axis.line = element_blank()                                 # Remove axis lines
  )

# BEST WAY
library(ggh4x)

# Custom breaks for each Taxonomic_Group
custom_breaks <- list(
  "Beetles" = seq(0, 18, 3),
  "Bryophytes" = seq(0, 40, 5),
  "Lichen" = seq(0, 75, 5),
  "Macrofungi" = seq(0, 300, 20),
  "Moths" = seq(0, 120, 10)
)

# Generate list of scales per facet
y_scales <- lapply(names(custom_breaks), function(group) {
  scale_y_continuous(breaks = custom_breaks[[group]])
})

# Create the plot
ggplot(BDV_long, aes(x = forest_cat, y = Species_Count)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(aes(fill = forest_cat)) +
  facet_wrap2(~Taxonomic_Group, scales = "free_y", ncol = 5) +
  ggh4x::facetted_pos_scales(y = y_scales) +
  scale_fill_manual(values = management_colors) +
  theme_minimal() +
  labs(
    title = "Species Richness per Taxonomic Group across Forest Categories",
    x = "Forest Categories", y = "Number of Species"
  ) +
  theme(
    plot.title = element_text(size = 19, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 15),
    panel.grid = element_blank()
  )

# Density plots con asse Y come numero di plot
ggplot(BDV_long, aes(x = Species_Count, fill = forest_cat, color = forest_cat)) +
  geom_density(aes(y = ..count..), alpha = 0.4, size = 1.2) +
  facet_wrap(~ Taxonomic_Group, scales = "free", ncol = 3) +
  scale_fill_manual(values = management_colors, name = "Forest Category") +
  scale_color_manual(values = management_colors, name = "Forest Category") +
  guides(color = "none") +  # Nasconde la seconda legenda
  labs(title = "Distribuzione assoluta della ricchezza specifica per gruppo tassonomico",
       x = "Numero di specie", y = "Numero di plot") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Histogram for Species Richness (Species_Count)
ggplot(BDV_long, aes(x = Species_Count, fill = forest_cat)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~ Taxonomic_Group, scales = "free", ncol = 3) +
  scale_fill_manual(values = management_colors) +
  labs(title = "Species Richness Distribution per Taxonomic Group",
       x = "Number of Species", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Histogram for DeadWood_C (Density) as a proxy
ggplot(BDV_long, aes(x = DeadWood_C, fill = forest_cat)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~ Taxonomic_Group, scales = "free", ncol = 3) +
  scale_fill_manual(values = management_colors) +
  labs(title = "Deadwood Density Distribution per Taxonomic Group",
       x = "Deadwood (g/m²)", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Crea una nuova colonna con i gruppi "Managed" e "Unmanaged"
BDV_long_2 <- BDV_long %>%
  mutate(management_group = case_when(
    forest_cat %in% c("Native Broadleaves", "Non-Native Coniferous") ~ "Managed",
    forest_cat == "Old-Growth" ~ "Unmanaged",
    TRUE ~ forest_cat  # Lascia invariati altri gruppi se presenti
  ))

# Verifica il risultato
head(BDV_long_2)

# Crea un boxplot per i gruppi "Managed" vs "Unmanaged" (per ogni Gruppo Tassonomico)
ggplot(BDV_long_2, aes(x = management_group, y = Species_Count, fill = management_group)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot() +
  facet_wrap(~Taxonomic_Group, scales = "free_y", ncol = 5) +
  scale_fill_manual(values = c("Managed" = "#FF8247", "Unmanaged" = "#3B9AB2")) +  # Colori personalizzati
  labs(title = "Confronto della Ricchezza Specifica: Managed vs Unmanaged",
       x = "Gruppi Forestali", y = "Numero di Specie") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 19, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 15)
  )

# Grafico di densità per la ricchezza specifica (Managed vs Unmanaged)
ggplot(BDV_long_2, aes(x = Species_Count, fill = management_group, color = management_group)) +
  geom_density(aes(y = ..count..), alpha = 0.4, size = 1.2) +
  facet_wrap(~ Taxonomic_Group, scales = "free", ncol = 3) +
  scale_fill_manual(values = c("Managed" = "#FF8247", "Unmanaged" = "#3B9AB2")) +  # Colori personalizzati
  scale_color_manual(values = c("Managed" = "#FF8247", "Unmanaged" = "#3B9AB2")) +
  guides(color = "none") +  # Nasconde la seconda legenda
  labs(title = "Distribuzione di Densità della Ricchezza Specifica: Managed vs Unmanaged",
       x = "Numero di Specie", y = "Numero di Plot") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Grafico di Istogramma per la ricchezza specifica (Managed vs Unmanaged)
ggplot(BDV_long_2, aes(x = Species_Count, fill = management_group, color = management_group)) +
  geom_histogram(bins = 20, alpha = 0.6, position = "identity") +
  facet_wrap(~ Taxonomic_Group, scales = "free", ncol = 3) +
  scale_fill_manual(values = c("Managed" = "#FF8247", "Unmanaged" = "#3B9AB2")) +  # Colori personalizzati
  scale_color_manual(values = c("Managed" = "#FF8247", "Unmanaged" = "#3B9AB2")) +
  guides(color = "none") +  # Nasconde la seconda legenda
  labs(title = "Distribuzione di Densità della Ricchezza Specifica: Managed vs Unmanaged",
       x = "Numero di Specie", y = "Numero di Plot") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


################################################################################ SECOND METHOD
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
    Moths = `Moths (0.566)`,
    broadl_40 = `broadl>40`
  )

# Select predictor columns for statistical testing
predictor_cols <- c("Age20percentOldestTrees", "DeadWood_C", "LAI", "ba_broadl", 
                    "trees_10_40", "broadl_40")  

# Select species richness columns (EXCLUDE red-listed groups and binary 0/1 column)
species_cols <- c("Epiphytic / epixilic bryophytes (0.212)", 
                  "Lichens (0.137)", "Macrofungi (2.118)", 
                  "Non-flying beetles (0.053)", "Moths (0.566)")

# Reshape species richness data to long format
BDV_species_long <- BDV_predictors_cleaned %>%
  pivot_longer(cols = c("Bryophytes", "Lichens", "Macrofungi", "Beetles", "Moths"), 
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
  cor_test(Richness, Predictor_Value, method = "kendall") %>%  # Alternative spearman and pearson correlation
  mutate(p.signif = case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "ns"  # Not significant
  ))

# Plot species richness across management types with significance
ggplot(BDV_species_long, aes(x = forest_cat, y = Richness)) +
  geom_boxplot(aes(fill = forest_cat), color = "black", outlier.shape = NA) +  # Boxplots in gray
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  # Dots colored by site
  facet_wrap(~Species, scales = "free", ncol=5) +  # Facet for each species group, important scales free to leave the y scale free
  theme_minimal() +
  scale_fill_manual(values = management_colors) +  # Fixed colors
  labs(title = "Species Richness Across Forest Categories",
       x = "Forest Categories", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")  



############################################ THIRD METHOD

# Load the data
BDV_predictors <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_track/20_Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC_GAMage_snags_tot_deadwood_tes - Copy.xlsx")


# Remake the names in the table for a better understanding and coding
BDV_predictors_cleaned <- BDV_predictors %>%
  mutate(forest_cat = recode(forest_cat,
                             "Beech-Oak" = "Native Broadleaves",
                             "Conifer" = "Non-Native Coniferous",
                             "Old-Growth" = "Old-Growth")) %>%
  select(-c(Lichens_RD1, Macrofungi_RD1, Moths_RD1, `Macrofungi x red-listed (2.652)`, `Moths x red-listed (0.574)`)) %>%
  rename(broadl_40 = `broadl>40`
  )


# Select species richness columns (EXCLUDE red-listed groups and binary 0/1 column)
species_cols <- c("Epiphytic / epixilic bryophytes (0.212)", 
                  "Lichens (0.137)", "Macrofungi (2.118)", 
                  "Non-flying beetles (0.053)", "Moths (0.566)")

# Reshape species richness data to long format
BDV_species_long <- BDV_predictors_cleaned %>%
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
             aes(x = forest_cat, y = Richness)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +  # T ends
  geom_boxplot(aes(fill = forest_cat), color = "black", outlier.shape = TRUE) +  
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  
  theme_minimal() +
  scale_fill_manual(values = management_colors) +  # Fixed colors
  labs(title = paste("G1 - Species Richness for", species_names[1]),
       x = "Forest Categories", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
print(G1)

# G2: Plot for second species
G2 <- ggplot(subset(BDV_species_long, Species == species_names[2]), 
             aes(x = forest_cat, y = Richness)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +  # T ends
  geom_boxplot(aes(fill = forest_cat), color = "black", outliers = FALSE) +  
  #geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +  
  theme_minimal() +
  scale_fill_manual(values = management_colors) +  # Same color scheme
  labs(title = paste("G2 - Species Richness for", species_names[2]),
       x = "Forest Categories", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
print(G2)


# G3: Plot for third species
G3 <- ggplot(subset(BDV_species_long, Species == species_names[3]), 
             aes(x = forest_cat, y = Richness)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(aes(fill = forest_cat), color = "black", outlier.shape = TRUE) +
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +
  theme_minimal() +
  scale_fill_manual(values = management_colors) +
  labs(title = paste("G3 - Species Richness for", species_names[3]),
       x = "Forest Categories", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
print(G3)

# G4: Plot for fourth species
G4 <- ggplot(subset(BDV_species_long, Species == species_names[4]), 
             aes(x = forest_cat, y = Richness)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(aes(fill = forest_cat), color = "black", outlier.shape = TRUE) +
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +
  theme_minimal() +
  scale_fill_manual(values = management_colors) +
  labs(title = paste("G4 - Species Richness for", species_names[4]),
       x = "Forest Categories", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
print(G4)


# G5: Plot for fifth species
G5 <- ggplot(subset(BDV_species_long, Species == species_names[5]), 
             aes(x = forest_cat, y = Richness)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(aes(fill = forest_cat), color = "black", outlier.shape = TRUE) +
  geom_jitter(aes(color = Site), width = 0.2, size = 2, alpha = 0.7) +
  theme_minimal() +
  scale_fill_manual(values = management_colors) +
  labs(title = paste("G5 - Species Richness for", species_names[5]),
       x = "Forest Categories", y = "Species Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
print(G5)


#-------------------------------------------------------------------------------
# KARNEL DENSITY PLOT

# DIVIDED PER SITES

# Ensure Species and Site are factors
BDV_species_long <- BDV_species_long %>%
  mutate(
    Species = factor(Species),
    Site = factor(Site)
  )

# Plot
ggplot(BDV_species_long, aes(x = Richness, fill = Species, color = Species)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  facet_wrap(~ Site, ncol = 3) +  # 2x3 grid layout
  labs(
    title = "Species Richness Density per Taxon across Sites",
    x = "Species Richness", y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    strip.text = element_text(size = 14),
    axis.title = element_text(size = 13)
  )


# DIVIDED PER TAXA

# Ensure 'Species' and 'Site' are factors
BDV_species_long <- BDV_species_long %>%
  mutate(
    Species = factor(Species),
    Site = factor(Site)
  )

# Plot: 5 facets (Species), 6 densities (Sites) per facet
ggplot(BDV_species_long, aes(x = Richness, fill = Site, color = Site)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  facet_wrap(~ Species, ncol = 3, scales = "free") +  # 5 horizontal boxes
  labs(
    title = "Species Richness Distributions per Taxonomic Group (by Site)",
    x = "Species Richness", y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    strip.text = element_text(size = 14),
    axis.title = element_text(size = 13),
    panel.grid = element_blank()
  )


#------------------------------------------------------------------------------- To save plots



# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
#dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
#pdf(paste0(dataroot, "03_boxplot_all.pdf"), height=8, width=12)

