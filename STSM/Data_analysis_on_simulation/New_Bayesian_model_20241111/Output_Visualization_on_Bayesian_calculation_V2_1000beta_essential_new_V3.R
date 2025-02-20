library(ggpointdensity)
library(ggplot2)
library(viridis)
library(stringr)

################################################################################
# Let's first of all calculate the median richness in protected plots

library(dplyr)

# Filter management type Protected area
BDV_PR <- BDV_predictors %>% filter(management_type == "PR")

species_columns <- c("Epiphytic / epixilic bryophytes (0.212)", 
                     "Lichens (0.137)", 
                     "Macrofungi (2.118)", 
                     "Non-flying beetles (0.053)", 
                     "Moths")

# Calculate the medians
median_species_richness <- BDV_PR %>%
  select(all_of(species_columns)) %>%
  summarise(across(everything(), median, na.rm = TRUE))

median_species_richness)


################################################################################
# L6 ___________________________________________________________________________
# Reduce dataset by 95%
reduced_data <- bayesian_results_all %>%
  sample_frac(0.05)

# BEETLES OFFICIAL PLOT V4
# Filter BDV_predictors for only L6 site plots
BDV_predictors_L6 <- BDV_predictors %>%
  filter(str_detect(plotID, "^L6_"))

# Replace plotID with row numbers
BDV_predictors_L6$plotID <- seq_len(nrow(BDV_predictors_L6))

# Rename column to 'Beetles'
colnames(BDV_predictors_L6)[which(colnames(BDV_predictors_L6) == "Non-flying beetles (0.053)")] <- "Beetles"

# Update the plot using only BDV_predictors_L6
B0 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = BEETLES_PRED_RICH_50, linetype = "Non-Flying Beetles Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Non-flying beetles (0.053)`, color = "red", linetype = "dashed", size = 0.5) +
  
  # Violin plot and jitter plot with only L6 data
  geom_violin(data = BDV_predictors_L6, 
              aes(x = plotID + 591, y = Beetles),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  geom_jitter(data = BDV_predictors_L6, 
              aes(x = plotID + 591, y = Beetles),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(
    values = c(
      "Non-Flying Beetles Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  labs(
    title = "L6 - Density Point Cloud of Predicted Non-Flying Beetles Sp. Richness with Beta Functions and Observed Richness",
    x = "Year",
    y = "Predicted Sp. Richness of Non-Flying Beetles"
  ) +
  
  facet_wrap(~run, ncol = 6) +
  
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(B0)

#-------------------------------------------------------------------------------
#########################
# Bryophytes
#########################

# Filter BDV_predictors for only L6 site plots
BDV_predictors_L6 <- BDV_predictors %>%
  filter(str_detect(plotID, "^L6_"))

# Replace plotID with row numbers
BDV_predictors_L6$plotID <- seq_len(nrow(BDV_predictors_L6))

# Rename column to 'Bryophytes'
colnames(BDV_predictors_L6)[which(colnames(BDV_predictors_L6) == "Epiphytic / epixilic bryophytes (0.212)")] <- "Bryophytes"

B1 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BRYOPHYTES)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = BRYO_PRED_RICH_50, linetype = "Bryophytes Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Epiphytic / epixilic bryophytes (0.212)`, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L6, aes(x = plotID + 590, y = Bryophytes), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L6, aes(x = plotID + 590, y = Bryophytes), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c("Bryophytes Function" = "solid"), name = "Legend") +
  
  labs(title = "L6 - Density Point Cloud of Predicted Bryophytes Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Bryophytes") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B1)

#-------------------------------------------------------------------------------
#########################
# Lichens
#########################

# Rename column to 'Lichens'
colnames(BDV_predictors_L6)[which(colnames(BDV_predictors_L6) == "Lichens (0.137)")] <- "Lichens"

B2 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_LICHENS)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = LICHEN_PRED_RICH_50, linetype = "Lichens Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Lichens (0.137)` , color = "red", linetype = "dashed", size = 0.5) +

  geom_violin(data = BDV_predictors_L6, aes(x = plotID + 590, y = Lichens), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L6, aes(x = plotID + 590, y = Lichens), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c("Lichens Function" = "solid"), name = "Legend") +
  
  labs(title = "L6 - Density Point Cloud of Predicted Lichens Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Lichens") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B2)

#-------------------------------------------------------------------------------
#########################
# Macrofungi
#########################

# Rename column to 'Macromycetes'
colnames(BDV_predictors_L6)[which(colnames(BDV_predictors_L6) == "Macrofungi (2.118)")] <- "Macromycetes"

B3 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = MACROFUNGI_PRED_RICH_50, linetype = "Macromycetes Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Macrofungi (2.118)`, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L6, aes(x = plotID + 590, y = Macromycetes), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L6, aes(x = plotID + 590, y = Macromycetes), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c( "Macromycetes Function" = "solid"), name = "Legend") +
  
  labs(title = "L6 - Density Point Cloud of Predicted Macromycetes Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Macromycetes") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B3)



#-------------------------------------------------------------------------------
#########################
# Moths
#########################


# Rename column for Moths in BDV_predictors_L6
colnames(BDV_predictors_L6)[which(colnames(BDV_predictors_L6) == "Moths (0.566)")] <- "Moths"

# Create Moths richness plot
B5 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Moths function (main prediction line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = MOTHS_PRED_RICH_50, linetype = "Moths Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines for reference points
  geom_hline(yintercept = median_species_richness$Moths, color = "red", linetype = "dashed", size = 0.5) +
  
  # Add violin plot for Moths in BDV_predictors_L6
  geom_violin(data = BDV_predictors_L6, 
              aes(x = plotID + 590, y = Moths),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  # Add jitter points for Moths
  geom_jitter(data = BDV_predictors_L6, 
              aes(x = plotID + 590, y = Moths),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  # Custom line type scale for beta lines
  scale_linetype_manual(
    values = c(
      "Moths Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "L6 - Density Point Cloud of Predicted Moths Sp. Richness with Beta Functions",
    x = "Year",
    y = "Predicted Sp. Richness of Moths"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 6) +
  
  # Themes and adjustments
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(B5)


#   L1
################################################################################
#_______________________________________________________________________________
# Reduce dataset by 95%
reduced_data <- bayesian_results_all %>%
  sample_frac(0.05)

# BEETLES OFFICIAL PLOT V4
# Filter BDV_predictors for only L1 site plots
BDV_predictors_L1 <- BDV_predictors %>%
  filter(str_detect(plotID, "^L1_"))

# Replace plotID with row numbers
BDV_predictors_L1$plotID <- seq_len(nrow(BDV_predictors_L1))

# Rename column to 'Beetles'
colnames(BDV_predictors_L1)[which(colnames(BDV_predictors_L1) == "Non-flying beetles (0.053)")] <- "Beetles"

# Update the plot using only BDV_predictors_L1
B0 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = BEETLES_PRED_RICH_50, linetype = "Non-Flying Beetles Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Non-flying beetles (0.053)`, color = "red", linetype = "dashed", size = 0.5) +
  
  # Violin plot and jitter plot with only L1 data
  geom_violin(data = BDV_predictors_L1, 
              aes(x = plotID + 591, y = Beetles),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  geom_jitter(data = BDV_predictors_L1, 
              aes(x = plotID + 591, y = Beetles),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(
    values = c(
      "Non-Flying Beetles Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  labs(
    title = "L1 - Density Point Cloud of Predicted Non-Flying Beetles Sp. Richness with Beta Functions and Observed Richness",
    x = "Year",
    y = "Predicted Sp. Richness of Non-Flying Beetles"
  ) +
  
  facet_wrap(~run, ncol = 6) +
  
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(B0)

#-------------------------------------------------------------------------------
#########################
# Bryophytes
#########################

# Filter BDV_predictors for only L1 site plots
BDV_predictors_L1 <- BDV_predictors %>%
  filter(str_detect(plotID, "^L1_"))

# Replace plotID with row numbers
BDV_predictors_L1$plotID <- seq_len(nrow(BDV_predictors_L1))

# Rename column to 'Bryophytes'
colnames(BDV_predictors_L1)[which(colnames(BDV_predictors_L1) == "Epiphytic / epixilic bryophytes (0.212)")] <- "Bryophytes"

B1 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BRYOPHYTES)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = BRYO_PRED_RICH_50, linetype = "Bryophytes Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Epiphytic / epixilic bryophytes (0.212)`, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L1, aes(x = plotID + 590, y = Bryophytes), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L1, aes(x = plotID + 590, y = Bryophytes), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c("Bryophytes Function" = "solid"), name = "Legend") +
  
  labs(title = "L1 - Density Point Cloud of Predicted Bryophytes Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Bryophytes") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B1)

#-------------------------------------------------------------------------------
#########################
# Lichens
#########################

# Rename column to 'Lichens'
colnames(BDV_predictors_L1)[which(colnames(BDV_predictors_L1) == "Lichens (0.137)")] <- "Lichens"

B2 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_LICHENS)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = LICHEN_PRED_RICH_50, linetype = "Lichens Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Lichens (0.137)` , color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L1, aes(x = plotID + 590, y = Lichens), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L1, aes(x = plotID + 590, y = Lichens), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c("Lichens Function" = "solid"), name = "Legend") +
  
  labs(title = "L1 - Density Point Cloud of Predicted Lichens Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Lichens") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B2)

#-------------------------------------------------------------------------------
#########################
# Macrofungi
#########################

# Rename column to 'Macromycetes'
colnames(BDV_predictors_L1)[which(colnames(BDV_predictors_L1) == "Macrofungi (2.118)")] <- "Macromycetes"

B3 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = MACROFUNGI_PRED_RICH_50, linetype = "Macromycetes Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Macrofungi (2.118)`, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L1, aes(x = plotID + 590, y = Macromycetes), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L1, aes(x = plotID + 590, y = Macromycetes), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c( "Macromycetes Function" = "solid"), name = "Legend") +
  
  labs(title = "L1 - Density Point Cloud of Predicted Macromycetes Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Macromycetes") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B3)



#-------------------------------------------------------------------------------
#########################
# Moths
#########################


# Rename column for Moths in BDV_predictors_L1
colnames(BDV_predictors_L1)[which(colnames(BDV_predictors_L1) == "Moths (0.566)")] <- "Moths"

# Create Moths richness plot
B5 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Moths function (main prediction line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = MOTHS_PRED_RICH_50, linetype = "Moths Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines for reference points
  geom_hline(yintercept = median_species_richness$Moths, color = "red", linetype = "dashed", size = 0.5) +
  
  # Add violin plot for Moths in BDV_predictors_L1
  geom_violin(data = BDV_predictors_L1, 
              aes(x = plotID + 590, y = Moths),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  # Add jitter points for Moths
  geom_jitter(data = BDV_predictors_L1, 
              aes(x = plotID + 590, y = Moths),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  # Custom line type scale for beta lines
  scale_linetype_manual(
    values = c(
      "Moths Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "L1 - Density Point Cloud of Predicted Moths Sp. Richness with Beta Functions",
    x = "Year",
    y = "Predicted Sp. Richness of Moths"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 6) +
  
  # Themes and adjustments
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(B5)





# L2
################################################################################
#_______________________________________________________________________________
# Reduce dataset by 95%
reduced_data <- bayesian_results_all %>%
  sample_frac(0.05)

# BEETLES OFFICIAL PLOT V4
# Filter BDV_predictors for only L2 site plots
BDV_predictors_L2 <- BDV_predictors %>%
  filter(str_detect(plotID, "^L2_"))

# Replace plotID with row numbers
BDV_predictors_L2$plotID <- seq_len(nrow(BDV_predictors_L2))

# Rename column to 'Beetles'
colnames(BDV_predictors_L2)[which(colnames(BDV_predictors_L2) == "Non-flying beetles (0.053)")] <- "Beetles"

# Update the plot using only BDV_predictors_L2
B0 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = BEETLES_PRED_RICH_50, linetype = "Non-Flying Beetles Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Non-flying beetles (0.053)`, color = "red", linetype = "dashed", size = 0.5) +
  
  # Violin plot and jitter plot with only L2 data
  geom_violin(data = BDV_predictors_L2, 
              aes(x = plotID + 591, y = Beetles),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  geom_jitter(data = BDV_predictors_L2, 
              aes(x = plotID + 591, y = Beetles),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(
    values = c(
      "Non-Flying Beetles Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  labs(
    title = "L2 - Density Point Cloud of Predicted Non-Flying Beetles Sp. Richness with Beta Functions and Observed Richness",
    x = "Year",
    y = "Predicted Sp. Richness of Non-Flying Beetles"
  ) +
  
  facet_wrap(~run, ncol = 6) +
  
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(B0)

#-------------------------------------------------------------------------------
#########################
# Bryophytes
#########################

# Filter BDV_predictors for only L2 site plots
BDV_predictors_L2 <- BDV_predictors %>%
  filter(str_detect(plotID, "^L2_"))

# Replace plotID with row numbers
BDV_predictors_L2$plotID <- seq_len(nrow(BDV_predictors_L2))

# Rename column to 'Bryophytes'
colnames(BDV_predictors_L2)[which(colnames(BDV_predictors_L2) == "Epiphytic / epixilic bryophytes (0.212)")] <- "Bryophytes"

B1 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BRYOPHYTES)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = BRYO_PRED_RICH_50, linetype = "Bryophytes Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Epiphytic / epixilic bryophytes (0.212)`, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L2, aes(x = plotID + 590, y = Bryophytes), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L2, aes(x = plotID + 590, y = Bryophytes), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c("Bryophytes Function" = "solid"), name = "Legend") +
  
  labs(title = "L2 - Density Point Cloud of Predicted Bryophytes Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Bryophytes") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B1)

#-------------------------------------------------------------------------------
#########################
# Lichens
#########################

# Rename column to 'Lichens'
colnames(BDV_predictors_L2)[which(colnames(BDV_predictors_L2) == "Lichens (0.137)")] <- "Lichens"

B2 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_LICHENS)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = LICHEN_PRED_RICH_50, linetype = "Lichens Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Lichens (0.137)` , color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L2, aes(x = plotID + 590, y = Lichens), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L2, aes(x = plotID + 590, y = Lichens), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c("Lichens Function" = "solid"), name = "Legend") +
  
  labs(title = "L2 - Density Point Cloud of Predicted Lichens Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Lichens") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B2)

#-------------------------------------------------------------------------------
#########################
# Macrofungi
#########################

# Rename column to 'Macromycetes'
colnames(BDV_predictors_L2)[which(colnames(BDV_predictors_L2) == "Macrofungi (2.118)")] <- "Macromycetes"

B3 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = MACROFUNGI_PRED_RICH_50, linetype = "Macromycetes Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Macrofungi (2.118)`, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L2, aes(x = plotID + 590, y = Macromycetes), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L2, aes(x = plotID + 590, y = Macromycetes), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c( "Macromycetes Function" = "solid"), name = "Legend") +
  
  labs(title = "L2 - Density Point Cloud of Predicted Macromycetes Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Macromycetes") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B3)



#-------------------------------------------------------------------------------
#########################
# Moths
#########################


# Rename column for Moths in BDV_predictors_L2
colnames(BDV_predictors_L2)[which(colnames(BDV_predictors_L2) == "Moths (0.566)")] <- "Moths"

# Create Moths richness plot
B5 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Moths function (main prediction line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = MOTHS_PRED_RICH_50, linetype = "Moths Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines for reference points
  geom_hline(yintercept = median_species_richness$Moths, color = "red", linetype = "dashed", size = 0.5) +
  
  # Add violin plot for Moths in BDV_predictors_L2
  geom_violin(data = BDV_predictors_L2, 
              aes(x = plotID + 590, y = Moths),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  # Add jitter points for Moths
  geom_jitter(data = BDV_predictors_L2, 
              aes(x = plotID + 590, y = Moths),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  # Custom line type scale for beta lines
  scale_linetype_manual(
    values = c(
      "Moths Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "L2 - Density Point Cloud of Predicted Moths Sp. Richness with Beta Functions",
    x = "Year",
    y = "Predicted Sp. Richness of Moths"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 6) +
  
  # Themes and adjustments
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(B5)





# L3
################################################################################
#_______________________________________________________________________________
# Reduce dataset by 95%
reduced_data <- bayesian_results_all %>%
  sample_frac(0.05)

# BEETLES OFFICIAL PLOT V4
# Filter BDV_predictors for only L3 site plots
BDV_predictors_L3 <- BDV_predictors %>%
  filter(str_detect(plotID, "^L3_"))

# Replace plotID with row numbers
BDV_predictors_L3$plotID <- seq_len(nrow(BDV_predictors_L3))

# Rename column to 'Beetles'
colnames(BDV_predictors_L3)[which(colnames(BDV_predictors_L3) == "Non-flying beetles (0.053)")] <- "Beetles"

# Update the plot using only BDV_predictors_L3
B0 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = BEETLES_PRED_RICH_50, linetype = "Non-Flying Beetles Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Non-flying beetles (0.053)`, color = "red", linetype = "dashed", size = 0.5) +
  
  # Violin plot and jitter plot with only L3 data
  geom_violin(data = BDV_predictors_L3, 
              aes(x = plotID + 591, y = Beetles),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  geom_jitter(data = BDV_predictors_L3, 
              aes(x = plotID + 591, y = Beetles),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(
    values = c(
      "Non-Flying Beetles Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  labs(
    title = "L3 - Density Point Cloud of Predicted Non-Flying Beetles Sp. Richness with Beta Functions and Observed Richness",
    x = "Year",
    y = "Predicted Sp. Richness of Non-Flying Beetles"
  ) +
  
  facet_wrap(~run, ncol = 6) +
  
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(B0)

#-------------------------------------------------------------------------------
#########################
# Bryophytes
#########################

# Filter BDV_predictors for only L3 site plots
BDV_predictors_L3 <- BDV_predictors %>%
  filter(str_detect(plotID, "^L3_"))

# Replace plotID with row numbers
BDV_predictors_L3$plotID <- seq_len(nrow(BDV_predictors_L3))

# Rename column to 'Bryophytes'
colnames(BDV_predictors_L3)[which(colnames(BDV_predictors_L3) == "Epiphytic / epixilic bryophytes (0.212)")] <- "Bryophytes"

B1 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BRYOPHYTES)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = BRYO_PRED_RICH_50, linetype = "Bryophytes Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Epiphytic / epixilic bryophytes (0.212)`, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L3, aes(x = plotID + 590, y = Bryophytes), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L3, aes(x = plotID + 590, y = Bryophytes), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c("Bryophytes Function" = "solid"), name = "Legend") +
  
  labs(title = "L3 - Density Point Cloud of Predicted Bryophytes Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Bryophytes") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B1)

#-------------------------------------------------------------------------------
#########################
# Lichens
#########################

# Rename column to 'Lichens'
colnames(BDV_predictors_L3)[which(colnames(BDV_predictors_L3) == "Lichens (0.137)")] <- "Lichens"

B2 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_LICHENS)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = LICHEN_PRED_RICH_50, linetype = "Lichens Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Lichens (0.137)` , color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L3, aes(x = plotID + 590, y = Lichens), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L3, aes(x = plotID + 590, y = Lichens), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c("Lichens Function" = "solid"), name = "Legend") +
  
  labs(title = "L3 - Density Point Cloud of Predicted Lichens Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Lichens") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B2)

#-------------------------------------------------------------------------------
#########################
# Macrofungi
#########################

# Rename column to 'Macromycetes'
colnames(BDV_predictors_L3)[which(colnames(BDV_predictors_L3) == "Macrofungi (2.118)")] <- "Macromycetes"

B3 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = MACROFUNGI_PRED_RICH_50, linetype = "Macromycetes Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Macrofungi (2.118)`, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L3, aes(x = plotID + 590, y = Macromycetes), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L3, aes(x = plotID + 590, y = Macromycetes), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c( "Macromycetes Function" = "solid"), name = "Legend") +
  
  labs(title = "L3 - Density Point Cloud of Predicted Macromycetes Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Macromycetes") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B3)



#-------------------------------------------------------------------------------
#########################
# Moths
#########################


# Rename column for Moths in BDV_predictors_L3
colnames(BDV_predictors_L3)[which(colnames(BDV_predictors_L3) == "Moths (0.566)")] <- "Moths"

# Create Moths richness plot
B5 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Moths function (main prediction line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = MOTHS_PRED_RICH_50, linetype = "Moths Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines for reference points
  geom_hline(yintercept = median_species_richness$Moths, color = "red", linetype = "dashed", size = 0.5) +
  
  # Add violin plot for Moths in BDV_predictors_L3
  geom_violin(data = BDV_predictors_L3, 
              aes(x = plotID + 590, y = Moths),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  # Add jitter points for Moths
  geom_jitter(data = BDV_predictors_L3, 
              aes(x = plotID + 590, y = Moths),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  # Custom line type scale for beta lines
  scale_linetype_manual(
    values = c(
      "Moths Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "L3 - Density Point Cloud of Predicted Moths Sp. Richness with Beta Functions",
    x = "Year",
    y = "Predicted Sp. Richness of Moths"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 6) +
  
  # Themes and adjustments
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(B5)





# L4
################################################################################
#_______________________________________________________________________________
# Reduce dataset by 95%
reduced_data <- bayesian_results_all %>%
  sample_frac(0.05)

# BEETLES OFFICIAL PLOT V4
# Filter BDV_predictors for only L4 site plots
BDV_predictors_L4 <- BDV_predictors %>%
  filter(str_detect(plotID, "^L4_"))

# Replace plotID with row numbers
BDV_predictors_L4$plotID <- seq_len(nrow(BDV_predictors_L4))

# Rename column to 'Beetles'
colnames(BDV_predictors_L4)[which(colnames(BDV_predictors_L4) == "Non-flying beetles (0.053)")] <- "Beetles"

# Update the plot using only BDV_predictors_L4
B0 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = BEETLES_PRED_RICH_50, linetype = "Non-Flying Beetles Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Non-flying beetles (0.053)`, color = "red", linetype = "dashed", size = 0.5) +
  
  # Violin plot and jitter plot with only L4 data
  geom_violin(data = BDV_predictors_L4, 
              aes(x = plotID + 591, y = Beetles),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  geom_jitter(data = BDV_predictors_L4, 
              aes(x = plotID + 591, y = Beetles),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(
    values = c(
      "Non-Flying Beetles Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  labs(
    title = "L4 - Density Point Cloud of Predicted Non-Flying Beetles Sp. Richness with Beta Functions and Observed Richness",
    x = "Year",
    y = "Predicted Sp. Richness of Non-Flying Beetles"
  ) +
  
  facet_wrap(~run, ncol = 6) +
  
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(B0)

#-------------------------------------------------------------------------------
#########################
# Bryophytes
#########################

# Filter BDV_predictors for only L4 site plots
BDV_predictors_L4 <- BDV_predictors %>%
  filter(str_detect(plotID, "^L4_"))

# Replace plotID with row numbers
BDV_predictors_L4$plotID <- seq_len(nrow(BDV_predictors_L4))

# Rename column to 'Bryophytes'
colnames(BDV_predictors_L4)[which(colnames(BDV_predictors_L4) == "Epiphytic / epixilic bryophytes (0.212)")] <- "Bryophytes"

B1 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BRYOPHYTES)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = BRYO_PRED_RICH_50, linetype = "Bryophytes Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Epiphytic / epixilic bryophytes (0.212)`, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L4, aes(x = plotID + 590, y = Bryophytes), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L4, aes(x = plotID + 590, y = Bryophytes), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c("Bryophytes Function" = "solid"), name = "Legend") +
  
  labs(title = "L4 - Density Point Cloud of Predicted Bryophytes Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Bryophytes") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B1)

#-------------------------------------------------------------------------------
#########################
# Lichens
#########################

# Rename column to 'Lichens'
colnames(BDV_predictors_L4)[which(colnames(BDV_predictors_L4) == "Lichens (0.137)")] <- "Lichens"

B2 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_LICHENS)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = LICHEN_PRED_RICH_50, linetype = "Lichens Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Lichens (0.137)` , color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L4, aes(x = plotID + 590, y = Lichens), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L4, aes(x = plotID + 590, y = Lichens), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c("Lichens Function" = "solid"), name = "Legend") +
  
  labs(title = "L4 - Density Point Cloud of Predicted Lichens Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Lichens") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B2)

#-------------------------------------------------------------------------------
#########################
# Macrofungi
#########################

# Rename column to 'Macromycetes'
colnames(BDV_predictors_L4)[which(colnames(BDV_predictors_L4) == "Macrofungi (2.118)")] <- "Macromycetes"

B3 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = MACROFUNGI_PRED_RICH_50, linetype = "Macromycetes Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Macrofungi (2.118)`, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L4, aes(x = plotID + 590, y = Macromycetes), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L4, aes(x = plotID + 590, y = Macromycetes), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c( "Macromycetes Function" = "solid"), name = "Legend") +
  
  labs(title = "L4 - Density Point Cloud of Predicted Macromycetes Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Macromycetes") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B3)



#-------------------------------------------------------------------------------
#########################
# Moths
#########################


# Rename column for Moths in BDV_predictors_L4
colnames(BDV_predictors_L4)[which(colnames(BDV_predictors_L4) == "Moths (0.566)")] <- "Moths"

# Create Moths richness plot
B5 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Moths function (main prediction line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = MOTHS_PRED_RICH_50, linetype = "Moths Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines for reference points
  geom_hline(yintercept = median_species_richness$Moths, color = "red", linetype = "dashed", size = 0.5) +
  
  # Add violin plot for Moths in BDV_predictors_L4
  geom_violin(data = BDV_predictors_L4, 
              aes(x = plotID + 590, y = Moths),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  # Add jitter points for Moths
  geom_jitter(data = BDV_predictors_L4, 
              aes(x = plotID + 590, y = Moths),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  # Custom line type scale for beta lines
  scale_linetype_manual(
    values = c(
      "Moths Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "L4 - Density Point Cloud of Predicted Moths Sp. Richness with Beta Functions",
    x = "Year",
    y = "Predicted Sp. Richness of Moths"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 6) +
  
  # Themes and adjustments
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(B5)



# L5
################################################################################
#_______________________________________________________________________________
# Reduce dataset by 95%
reduced_data <- bayesian_results_all %>%
  sample_frac(0.05)

# BEETLES OFFICIAL PLOT V4
# Filter BDV_predictors for only L5 site plots
BDV_predictors_L5 <- BDV_predictors %>%
  filter(str_detect(plotID, "^L5_"))

# Replace plotID with row numbers
BDV_predictors_L5$plotID <- seq_len(nrow(BDV_predictors_L5))

# Rename column to 'Beetles'
colnames(BDV_predictors_L5)[which(colnames(BDV_predictors_L5) == "Non-flying beetles (0.053)")] <- "Beetles"

# Update the plot using only BDV_predictors_L5
B0 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = BEETLES_PRED_RICH_50, linetype = "Non-Flying Beetles Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Non-flying beetles (0.053)`, color = "red", linetype = "dashed", size = 0.5) +
  
  # Violin plot and jitter plot with only L5 data
  geom_violin(data = BDV_predictors_L5, 
              aes(x = plotID + 591, y = Beetles),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  geom_jitter(data = BDV_predictors_L5, 
              aes(x = plotID + 591, y = Beetles),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(
    values = c(
      "Non-Flying Beetles Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  labs(
    title = "L5 - Density Point Cloud of Predicted Non-Flying Beetles Sp. Richness with Beta Functions and Observed Richness",
    x = "Year",
    y = "Predicted Sp. Richness of Non-Flying Beetles"
  ) +
  
  facet_wrap(~run, ncol = 6) +
  
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(B0)

#-------------------------------------------------------------------------------
#########################
# Bryophytes
#########################

# Filter BDV_predictors for only L5 site plots
BDV_predictors_L5 <- BDV_predictors %>%
  filter(str_detect(plotID, "^L5_"))

# Replace plotID with row numbers
BDV_predictors_L5$plotID <- seq_len(nrow(BDV_predictors_L5))

# Rename column to 'Bryophytes'
colnames(BDV_predictors_L5)[which(colnames(BDV_predictors_L5) == "Epiphytic / epixilic bryophytes (0.212)")] <- "Bryophytes"

B1 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BRYOPHYTES)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = BRYO_PRED_RICH_50, linetype = "Bryophytes Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Epiphytic / epixilic bryophytes (0.212)`, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L5, aes(x = plotID + 590, y = Bryophytes), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L5, aes(x = plotID + 590, y = Bryophytes), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c("Bryophytes Function" = "solid"), name = "Legend") +
  
  labs(title = "L5 - Density Point Cloud of Predicted Bryophytes Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Bryophytes") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B1)

#-------------------------------------------------------------------------------
#########################
# Lichens
#########################

# Rename column to 'Lichens'
colnames(BDV_predictors_L5)[which(colnames(BDV_predictors_L5) == "Lichens (0.137)")] <- "Lichens"

B2 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_LICHENS)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = LICHEN_PRED_RICH_50, linetype = "Lichens Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Lichens (0.137)` , color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L5, aes(x = plotID + 590, y = Lichens), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L5, aes(x = plotID + 590, y = Lichens), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c("Lichens Function" = "solid"), name = "Legend") +
  
  labs(title = "L5 - Density Point Cloud of Predicted Lichens Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Lichens") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B2)

#-------------------------------------------------------------------------------
#########################
# Macrofungi
#########################

# Rename column to 'Macromycetes'
colnames(BDV_predictors_L5)[which(colnames(BDV_predictors_L5) == "Macrofungi (2.118)")] <- "Macromycetes"

B3 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, aes(x = year, y = MACROFUNGI_PRED_RICH_50, linetype = "Macromycetes Function"), color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness$`Macrofungi (2.118)`, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors_L5, aes(x = plotID + 590, y = Macromycetes), fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  geom_jitter(data = BDV_predictors_L5, aes(x = plotID + 590, y = Macromycetes), color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  scale_linetype_manual(values = c( "Macromycetes Function" = "solid"), name = "Legend") +
  
  labs(title = "L5 - Density Point Cloud of Predicted Macromycetes Sp. Richness with Beta Functions", x = "Year", y = "Predicted Sp. Richness of Macromycetes") +
  facet_wrap(~run, ncol = 6) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), legend.position = "right")

print(B3)



#-------------------------------------------------------------------------------
#########################
# Moths
#########################


# Rename column for Moths in BDV_predictors_L5
colnames(BDV_predictors_L5)[which(colnames(BDV_predictors_L5) == "Moths (0.566)")] <- "Moths"

# Create Moths richness plot
B5 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Moths function (main prediction line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = MOTHS_PRED_RICH_50, linetype = "Moths Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines for reference points
  geom_hline(yintercept = median_species_richness$Moths, color = "red", linetype = "dashed", size = 0.5) +
  
  # Add violin plot for Moths in BDV_predictors_L5
  geom_violin(data = BDV_predictors_L5, 
              aes(x = plotID + 590, y = Moths),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  # Add jitter points for Moths
  geom_jitter(data = BDV_predictors_L5, 
              aes(x = plotID + 590, y = Moths),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  # Custom line type scale for beta lines
  scale_linetype_manual(
    values = c(
      "Moths Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "L5 - Density Point Cloud of Predicted Moths Sp. Richness with Beta Functions",
    x = "Year",
    y = "Predicted Sp. Richness of Moths"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 6) +
  
  # Themes and adjustments
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(B5)





