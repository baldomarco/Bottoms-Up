#-------------------------------------------------------------------------------
library(ggpointdensity)
library(ggplot2)
library(viridis)

taxa_quartiles_thrashold # Remember to run the code in BDV_recovery_V4 or previous versions

################################################################################
# Let's first of all calculate the median richness in protected plots

library(dplyr)

# Filter management type Protected area
BDV_PR <- BDV_predictors %>% filter(management_type == "PR")

# Remember to change the excel file for BDV_predictors should be the V20
species_columns <- c("Epiphytic / epixilic bryophytes (0.212)", 
                     "Lichens (0.137)", 
                     "Macrofungi (2.118)", 
                     "Non-flying beetles (0.053)", 
                     "Moths (0.566)")

# Calculate the medians
median_species_richness <- BDV_PR %>%
  select(all_of(species_columns)) %>%
  summarise(across(everything(), median, na.rm = TRUE))

median_species_richness


################################################################################
#_______________________________________________________________________________
# FILTER FOR THE FIRST 350 YEARS
bayesian_results_all_filtered <- bayesian_results_all %>% filter(year...1 >= 0 & year...1 <= 350)
Bayesian_BDV_model_V3_multi <- Bayesian_BDV_model_V3_multi %>% filter(year >= 0 & year <= 350)


# Reduce dataset by 95%
reduced_data <- bayesian_results_all_filtered %>%
  sample_frac(0.005)

# BEETLES OFFICIAL PLOT V4

# Replace plotID with row numbers
BDV_predictors$plotID <- seq_len(nrow(BDV_predictors)) # required for the data structure ansd plots argument names

#------------------------------------------------------------------------ Start to plot
# Rename column to 'beetles'
colnames(BDV_predictors)[which(colnames(BDV_predictors) == "Non-flying beetles (0.053)")] <- "Beetles"

# Combined Plot with Violin Plot and Dots
B0 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  geom_pointdensity(alpha = 0.5) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = BEETLES_PRED_RICH_50, linetype = "Median Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = 11.5, color = "red", linetype = "dashed", size = 0.5) +
  
  
  # Classic minimal boxplot for BDV_predictors
  geom_boxplot(data = BDV_predictors, 
               aes(x = 400, y = Beetles),
               width = 5,
               notch = TRUE,
               notchwidth = 0.5,
               outlier.shape = NA,        # no outliers (optional)
               fill = NA,                 # old school — transparent
               color = "black", 
               inherit.aes = FALSE) +
  
  stat_summary(data = BDV_predictors,   # adds red dot at median
               aes(x = 400, y = Beetles),
               fun = median,
               geom = "point",
               color = "red",
               size = 3,
               inherit.aes = FALSE) +
  
  
  scale_linetype_manual(
    values = c("Median Function" = "solid"),
    name = "Legend"
  ) +
  
  labs(
    title = "Density Point Cloud of Predicted Coleoptera Sp. Richness",
    x = "Year",
    y = "Predicted Sp. Richness of Coleoptera"
  ) +
  
  facet_wrap(~run, ncol = 10) +
  
  theme_minimal(base_size = 28) +  
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right",
    strip.text = element_text(size = 20),   # was 10
    axis.title = element_text(size = 24),   # was 12
    axis.text = element_text(size = 20),    # was 10
    legend.text = element_text(size = 20),  # was 10
    legend.title = element_text(size = 20), # was 10
    plot.title = element_text(size = 32, hjust = 0.5), # was 16
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank(),  # remove minor gridlines
    strip.background = element_rect(fill = "white")# white facet strip background
  )

# Print the plot
print(B0)


#-------------------------------------------------------------------------------
# BRYOPHYTES OFFICIAL PLOT V1

# Rename column to 'Bryophytes'
colnames(BDV_predictors)[which(colnames(BDV_predictors) == "Epiphytic / epixilic bryophytes (0.212)")] <- "Bryophytes"

# Combined Plot with Violin Plot and Dots
B1 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BRYOPHYTES)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.4) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Bryophytes function (single solid line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = BRYO_PRED_RICH_50, linetype = "Median Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines at y = 4 and y = 37
  geom_hline(yintercept = 17.5, color = "red", linetype = "dashed", size = 0.5) +
  
  # Classic minimal boxplot for BDV_predictors
  geom_boxplot(data = BDV_predictors, 
               aes(x = 400, y = Bryophytes),
               width = 5,
               notch = TRUE,
               notchwidth = 0.5,
               outlier.shape = NA,        # no outliers (optional)
               fill = NA,                 # old school — transparent
               color = "black", 
               inherit.aes = FALSE) +
  
  stat_summary(data = BDV_predictors,   # adds red dot at median
               aes(x = 400, y = Bryophytes),
               fun = median,
               geom = "point",
               color = "red",
               size = 3,
               inherit.aes = FALSE) +
  
  
  scale_linetype_manual(
    values = c(
      "Median Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "Density Point Cloud of Predicted Bryophyta Sp. Richness",
    x = "Year",
    y = "Predicted Sp. Richness of Bryophyta"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 10) +
  
  theme_minimal(base_size = 28) +  # was 14
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right",
    strip.text = element_text(size = 14),   # was 10
    axis.title = element_text(size = 24),   # was 12
    axis.text = element_text(size = 20),    # was 10
    legend.text = element_text(size = 20),  # was 10
    legend.title = element_text(size = 20), # was 10
    plot.title = element_text(size = 32, hjust = 0.5), # was 16
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white")
  )

# Print the plot
print(B1)

#-------------------------------------------------------------------------------
# LICHENS OFFICIAL PLOT V1

# Rename column to 'lichens'
colnames(BDV_predictors)[which(colnames(BDV_predictors) == "Lichens (0.137)")] <- "Lichens"

B2 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_LICHENS)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.2) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  
  # Bryophytes function (single solid line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = LICHEN_PRED_RICH_50, linetype = "Median Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines at y = 4 and y = 37
  geom_hline(yintercept = 18.5, color = "red", linetype = "dashed", size = 0.5) +
  
  # Classic minimal boxplot for BDV_predictors
  geom_boxplot(data = BDV_predictors, 
               aes(x = 400, y = Lichens),
               width = 5,
               notch = TRUE,
               notchwidth = 0.5,
               outlier.shape = NA,        # no outliers (optional)
               fill = NA,                 # old school — transparent
               color = "black", 
               inherit.aes = FALSE) +
  
  stat_summary(data = BDV_predictors,   # adds red dot at median
               aes(x = 400, y = Lichens),
               fun = median,
               geom = "point",
               color = "red",
               size = 3,
               inherit.aes = FALSE) +
  
  
  # Custom linetype scale for beta lines
  scale_linetype_manual(
    values = c(
      "Median Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "Density Point Cloud of Predicted Lichenes Sp. Richness",
    x = "Year",
    y = "Predicted Sp. Richness of Lichenes"
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 75, by = 10),  # Only show labels from 10 upwards
    expand = expansion(mult = c(0, 0.05))
  )+

  # Facet by 'run'
  facet_wrap(~run, ncol = 10) +
  
  theme_minimal(base_size = 28) +  # was 14
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right",
    strip.text = element_text(size = 14),   # was 10
    axis.title = element_text(size = 24),   # was 12
    axis.text = element_text(size = 20),    # was 10
    legend.text = element_text(size = 20),  # was 10
    legend.title = element_text(size = 20), # was 10
    plot.title = element_text(size = 32, hjust = 0.5), # was 16
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white")
  )


# Print the plot
print(B2)

#-------------------------------------------------------------------------------
# MACROFUNGI OFFICIAL PLOT V1

# Rename column to 'Macromycetes'
colnames(BDV_predictors)[which(colnames(BDV_predictors) == "Macrofungi (2.118)")] <- "Macrofungi"

B3 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.2) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  
  # Bryophytes function (single solid line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = MACROFUNGI_PRED_RICH_50, linetype = "Median Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines at y = 4 and y = 37
  #geom_hline(yintercept = 52, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 192, color = "red", linetype = "dashed", size = 0.5) +
  
  # Classic minimal boxplot for BDV_predictors
  geom_boxplot(data = BDV_predictors, 
               aes(x = 400, y = Macrofungi),
               width = 5,
               notch = TRUE,
               notchwidth = 0.5,
               outlier.shape = TRUE,        # no outliers (optional)
               fill = NA,                 # old school — transparent
               color = "black", 
               inherit.aes = FALSE) +
  
  stat_summary(data = BDV_predictors,   # adds red dot at median
               aes(x = 400, y = Macrofungi),
               fun = median,
               geom = "point",
               color = "red",
               size = 3,
               inherit.aes = FALSE) +
  
  
  # Custom linetype scale for beta lines
  scale_linetype_manual(
    values = c(
      "Median Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "Density Point Cloud of Predicted Macromycetes Sp. Richness",
    x = "Year",
    y = "Predicted Sp. Richness of Macromycetes"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 10) +
  
  theme_minimal(base_size = 28) +  # was 14
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right",
    strip.text = element_text(size = 14),   # was 10
    axis.title = element_text(size = 24),   # was 12
    axis.text = element_text(size = 20),    # was 10
    legend.text = element_text(size = 20),  # was 10
    legend.title = element_text(size = 20), # was 10
    plot.title = element_text(size = 32, hjust = 0.5), # was 16
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white")
  )

# Print the plot
print(B3)


#-------------------------------------------------------------------------------
# MOTHS OFFICIAL PLOT V1

# Rename column to 'Moths'
colnames(BDV_predictors)[which(colnames(BDV_predictors) == "Moths (0.566)")] <- "Moths"

B5 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.4) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  
  # Bryophytes function (single solid line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = MOTHS_PRED_RICH_50, linetype = "Median Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines at y = 25 and y = 106
  #geom_hline(yintercept = 25, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 62, color = "red", linetype = "dashed", size = 0.5) +

  # Classic minimal boxplot for BDV_predictors
  geom_boxplot(data = BDV_predictors, 
               aes(x = 400, y = Moths),
               width = 5,
               notch = TRUE,
               notchwidth = 0.5,
               outlier.shape = TRUE,        # no outliers (optional)
               fill = NA,                 # old school — transparent
               color = "black", 
               inherit.aes = FALSE) +
  
  stat_summary(data = BDV_predictors,   # adds red dot at median
               aes(x = 400, y = Moths),
               fun = median,
               geom = "point",
               color = "red",
               size = 3,
               inherit.aes = FALSE) +
  
  
  # Custom line type scale for beta lines
  scale_linetype_manual(
    values = c(
      "Median Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "Density Point Cloud of Predicted Lepidoptera Sp. Richness",
    x = "Year",
    y = "Predicted Sp. Richness of Lepidoptera"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 10) +
  
  theme_minimal(base_size = 14) +  
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right",
    strip.text = element_text(size = 14),   # was 10
    axis.title = element_text(size = 24),   # was 12
    axis.text = element_text(size = 20),    # was 10
    legend.text = element_text(size = 20),  # was 10
    legend.title = element_text(size = 20), # was 10
    plot.title = element_text(size = 32, hjust = 0.5), # was 16
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white")
  )

# Print the plot
print(B5)


dev.off()




theme_minimal(base_size = 14) +  
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right",
    strip.text = element_text(size = 10),   
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    plot.title = element_text(size = 16, hjust = 0.5),
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank(),  # remove minor gridlines
    strip.background = element_rect(fill = "white")# white facet strip background
  )







# Assign your plot to an object first, e.g., 'p'
p <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  geom_density_2d_filled(contour_var = "density", alpha = 1) +
  
  scale_fill_brewer(palette = "Spectral", direction = 1) +
  
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = BEETLES_PRED_RICH_50, linetype = "Non-Flying Beetles Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = median_species_richness,
             color = "red", linetype = "dashed", size = 0.5) +
  
  scale_linetype_manual(
    values = c("Non-Flying Beetles Function" = "solid"),
    name = "Legend"
  ) +
  
  labs(
    title = "Continuous Density Plot of Predicted Non-Flying Beetles Sp. Richness",
    x = "Year",
    y = "Predicted Sp. Richness of Non-Flying Beetles"
  ) +
  
  facet_wrap(~run, ncol = 10) +
  
  # Start with theme_minimal() - this is fine, we'll override its defaults
  theme_minimal() +
  
  theme(
    # --- Backgrounds: These are the critical lines for white background ---
    plot.background = element_rect(fill = "white", color = NA), # Overall plot background
    panel.background = element_rect(fill = "white", color = NA), # Background of each facet panel
    strip.background = element_rect(fill = "white", color = NA), # Background of facet labels
    
    # --- Grid Lines: These are for removing grids ---
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # --- Other elements (as per your previous settings) ---
    panel.border = element_rect(color = "black", fill = NA, size = 0.5), # Border around each panel
    legend.position = "right",
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    plot.title = element_text(size = 32, hjust = 0.5)
  )

# --- THE CRITICAL STEP FOR SAVING WITH A WHITE BACKGROUND ---
# Make sure to run this line *after* defining your plot object 'p'
# Explicitly setting 'bg = "white"' in ggsave() ensures the output file has a white background.
ggsave("my_final_plot_with_white_bg.jpg", plot = p, width = 15, height = 10, units = "in", dpi = 300, bg = "white")




















