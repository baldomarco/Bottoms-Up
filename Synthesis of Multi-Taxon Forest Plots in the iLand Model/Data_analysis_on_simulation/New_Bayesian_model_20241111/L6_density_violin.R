#-------------------------------------------------------------------------------
library(ggpointdensity)
library(ggplot2)
library(viridis)

# Reduce dataset by 50%
reduced_data <- bayesian_results_all %>%
  sample_frac(0.5)


#-------------------------------------------------------------------------------
# BRYOPHYTES OFFICIAL PLOT V1

# Rename column to 'bryophytes'
colnames(BDV_predictors)[which(colnames(BDV_predictors) == "Epiphytic / epixilic bryophytes (0.212)")] <- "Bryophytes"

# Combined Plot with Violin Plot and Dots
B1 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BRYOPHYTES)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Beta 1 and Beta 2 lines
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = BRYO_PRED_RICH_50_beta1, linetype = "Beta1 * age"), color = "#00A091", size = 0.9, inherit.aes = FALSE) +
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = BRYO_PRED_RICH_50_beta2, linetype = "Beta2 * deadwood"), color = "#533600", size = 0.9, inherit.aes = FALSE) +
  
  # Bryophytes function (single solid line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = BRYO_PRED_RICH_50, linetype = "Bryophytes Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines at y = 4 and y = 37
  geom_hline(yintercept = 9, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 12, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_violin(data = BDV_predictors, 
              aes(x = plotID + 590, y = Bryophytes),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  geom_jitter(data = BDV_predictors, 
              aes(x = plotID + 590, y = Bryophytes),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  
  scale_linetype_manual(
    values = c(
      "Beta1 * age" = "solid",
      "Beta2 * deadwood" = "solid",
      "Bryophytes Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "L6 - Density Point Cloud of Predicted Bryophytes Sp. Richness with Beta Functions",
    x = "Year",
    y = "Predicted Sp. Richness of Bryophytes"
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
print(B1)

#-------------------------------------------------------------------------------
# LICHENS OFFICIAL PLOT V1

# Rename column to 'lichens'
colnames(BDV_predictors)[which(colnames(BDV_predictors) == "Lichens (0.137)")] <- "Lichens"

B2 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_LICHENS)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Beta 1 and Beta 2 lines
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = LICHEN_PRED_RICH_50_beta1, linetype = "Beta1 * age"), color = "#00A091", size = 0.9, inherit.aes = FALSE) +
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = LICHEN_PRED_RICH_50_beta2, linetype = "Beta2 * lai_sim"), color = "darkgreen", size = 0.9, inherit.aes = FALSE) +
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = LICHEN_PRED_RICH_50_beta3, linetype = "Beta3 * broadl_40"), color = "chocolate3", size = 0.9, inherit.aes = FALSE) +
  
  # Bryophytes function (single solid line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = LICHEN_PRED_RICH_50, linetype = "Lichens Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines at y = 4 and y = 37
  geom_hline(yintercept = 11, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 24, color = "red", linetype = "dashed", size = 0.5) +
  
  # Add the violin plot within the density point graph
  geom_violin(data = BDV_predictors, 
              aes(x = plotID + 590, y = Lichens),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  geom_jitter(data = BDV_predictors, 
              aes(x = plotID + 590, y = Lichens),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  
  # Custom linetype scale for beta lines
  scale_linetype_manual(
    values = c(
      "Beta1 * age" = "solid",
      "Beta2 * lai_sim" = "solid",
      "Beta3 * broadl_40" = "solid",
      "Lichens Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "L6 - Density Point Cloud of Predicted Lichens Sp. Richness with Beta Functions",
    x = "Year",
    y = "Predicted Sp. Richness of Lichens"
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
print(B2)

#-------------------------------------------------------------------------------
# MACROFUNGI OFFICIAL PLOT V1

# Rename column to 'bryophytes'
colnames(BDV_predictors)[which(colnames(BDV_predictors) == "Macrofungi (2.118)")] <- "Macromycetes"

B3 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Beta 1 and Beta 2 lines
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = MACROFUNGI_PRED_RICH_50_beta1, linetype = "Beta1 * age"), color = "#00A091", size = 0.9, inherit.aes = FALSE) +
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = MACROFUNGI_PRED_RICH_50_beta2, linetype = "Beta2 * deadwood"), color = "#533600", size = 0.9, inherit.aes = FALSE) +
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = MACROFUNGI_PRED_RICH_50_beta3, linetype = "Beta3 * ba_broadl"), color = "chocolate3", size = 0.9, inherit.aes = FALSE) +
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = MACROFUNGI_PRED_RICH_50_beta4, linetype = "Beta4 * tree_10_40"), color = "#EEC9E5", size = 0.9, inherit.aes = FALSE) +
  
  # Bryophytes function (single solid line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = MACROFUNGI_PRED_RICH_50, linetype = "Macromycetes Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines at y = 4 and y = 37
  #geom_hline(yintercept = 52, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 116, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_hline(yintercept = 138, color = "red", linetype = "dashed", size = 0.5) +
  
  
  # Add the violin plot within the density point graph
  geom_violin(data = BDV_predictors, 
              aes(x = plotID + 590, y = Macromycetes),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  geom_jitter(data = BDV_predictors, 
              aes(x = plotID + 590, y = Macromycetes),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  
  
  # Custom linetype scale for beta lines
  scale_linetype_manual(
    values = c(
      "Beta1 * age" = "solid",
      "Beta2 * deadwood" = "solid",
      "Beta3 * ba_broadl" = "solid",
      "Beta4 * tree_10_40" = "solid",
      "Macromycetes Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "L6 - Density Point Cloud of Predicted Macromycetes Sp. Richness with Beta Functions",
    x = "Year",
    y = "Predicted Sp. Richness of Macromycetes"
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
print(B3)


#-------------------------------------------------------------------------------
# MOTHS OFFICIAL PLOT V1

# Rename column to 'bryophytes'
colnames(BDV_predictors)[which(colnames(BDV_predictors) == "Moths (0.566)")] <- "Moths"

B5 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Beta 1 and Beta 2 lines
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = MOTHS_PRED_RICH_50_beta1, linetype = "Beta1 * tree_10_40_2"), color = "#EEC9E5", size = 0.9, inherit.aes = FALSE) +
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = MOTHS_PRED_RICH_50_beta2, linetype = "Beta2 * broadl_40"), color = "chocolate3", size = 0.9, inherit.aes = FALSE) +
  
  # Bryophytes function (single solid line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = MOTHS_PRED_RICH_50, linetype = "Moths Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines at y = 25 and y = 106
  #geom_hline(yintercept = 25, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 56, color = "red", linetype = "dashed", size = 0.5) +
  #geom_hline(yintercept = 106, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 59, color = "red", linetype = "dashed", size = 0.5) +
  
  
  # Add the violin plot within the density point graph
  geom_violin(data = BDV_predictors, 
              aes(x = plotID + 590, y = Moths),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  geom_jitter(data = BDV_predictors, 
              aes(x = plotID + 590, y = Moths),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  
  # Custom line type scale for beta lines
  scale_linetype_manual(
    values = c(
      "Beta1 * tree_10_40_2" = "solid",
      "Beta2 * broadl_40" = "solid",
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


dev.off()



















