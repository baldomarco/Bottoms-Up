library(ggpointdensity)
library(ggplot2)

# Switch to a Linear Model or GAM

ggplot(bayesian_results_all, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  geom_pointdensity() +  # Density point cloud
  scale_color_viridis_c() +  # Fancy color palette
  stat_smooth(method = "lm", color = "blue", size = 1) +  # Linear smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Richness of Beetles",
    title = "Density Point Cloud with Linear Fit"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

#-------------------------------------------------------------------------------
# Pre-Aggregate Data (Mean Values for Each Year)

# Calculate mean richness per year and run
mean_data <- bayesian_results_all %>%
  group_by(year...1, run) %>%
  summarize(mean_beetles = mean(PRED_RICH_BEETLES, na.rm = TRUE), .groups = "drop")

# Plot using aggregated data for smoother line
ggplot(mean_data, aes(x = year...1, y = mean_beetles)) +
  geom_line(color = "blue", size = 1) +  # Line for mean values
  geom_pointdensity(data = bayesian_results_all, aes(x = year...1, y = PRED_RICH_BEETLES)) +  # Overlay point cloud
  scale_color_viridis_c() +  # Fancy color palette
  facet_wrap(~run) +
  labs(
    x = "Year",
    y = "Predicted Richness of Beetles",
    title = "Density Point Cloud with Mean Line"
  ) +
  theme_minimal()


#-------------------------------------------------------------------------------
# Reduce Dataset Size (50%) and Use GAM

# Reduce dataset
reduced_data <- bayesian_results_all %>% sample_frac(0.5)

# Plot with reduced data BEETLES
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  geom_pointdensity() +
  scale_color_viridis_c() +
  stat_smooth(method = "gam", color = "red", size = 1) +
  facet_wrap(~run) +
  labs(
    x = "Year",
    y = "Predicted Richness of Beetles",
    title = "Density Point Cloud with GAM (Reduced Data)"
  ) +
  coord_cartesian(ylim = c(0, 20))+
  theme_minimal()


# Plot with reduced data BRYOPHYTES
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BRYOPHYTES)) +
  geom_pointdensity() +
  scale_color_viridis_c() +
  stat_smooth(method = "gam", color = "red", size = 1) +
  facet_wrap(~run) +
  labs(
    x = "Year",
    y = "Predicted Richness of Bryophytes",
    title = "Density Point Cloud with GAM (Reduced Data)"
  ) +
  coord_cartesian(ylim = c(0, 30))+
  theme_minimal()


# Plot with reduced data LICHENS
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_LICHENS)) +
  geom_pointdensity() +
  scale_color_viridis_c() +
  stat_smooth(method = "gam", color = "red", size = 1) +
  facet_wrap(~run) +
  labs(
    x = "Year",
    y = "Predicted Richness of Lichens",
    title = "Density Point Cloud Lichens richness with GAM fitting (Reduced Data)"
  ) +
  coord_cartesian(ylim = c(0, 50))+
  theme_minimal()



# Plot with reduced data MACROFUNGI
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_pointdensity() +
  scale_color_viridis_c() +
  stat_smooth(method = "gam", color = "red", size = 1) +
  facet_wrap(~run) +
  labs(
    x = "Year",
    y = "Predicted Richness of Macrofungi",
    title = "Density Point Cloud Macrofungi species richness with GAM fitting"
  ) +
  coord_cartesian(ylim = c(0, 300))+
  theme_minimal()


# Plot with reduced data MACROFUNGI RED
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI_RED)) +
  geom_pointdensity() +
  scale_color_viridis_c() +
  stat_smooth(method = "gam", color = "red", size = 1) +
  facet_wrap(~run) +
  labs(
    x = "Year",
    y = "Predicted Richness of Macrofungi Red Listed Species Over Weighted",
    title = "Density Point Cloud Macrofungi Red Listed Species Richness Over Weighted with GAM Fitting"
  ) +
  coord_cartesian(ylim = c(0, 300))+
  theme_minimal()



# Plot with reduced data MOTHS
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS)) +
  geom_pointdensity() +
  scale_color_viridis_c() +
  stat_smooth(method = "gam", color = "red", size = 1) +
  facet_wrap(~run) +
  labs(
    x = "Year",
    y = "Predicted Richness of Moths",
    title = "Density Point Cloud Moths Species Richness with GAM Fitting"
  ) +
  coord_cartesian(ylim = c(0, 150))+
  theme_minimal()


# Plot with reduced data MOTHS RED
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS_RED)) +
  geom_pointdensity() +
  scale_color_viridis_c() +
  stat_smooth(method = "gam", color = "red", size = 1) +
  facet_wrap(~run) +
  labs(
    x = "Year",
    y = "Predicted Richness of Moths Red Listed Species Over Weighted",
    title = "Density Point Cloud Moths Red Listed Species Richness Over Weighted with GAM Fitting"
  ) +
  coord_cartesian(ylim = c(0, 150))+
  theme_minimal()

#-------------------------------------------------------------------------------
# Reduce dataset
reduced_data <- bayesian_results_all %>% sample_frac(0.5)

# Plot with reduced data MACROFUNGI
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_pointdensity() +
  scale_color_viridis_c() +
  stat_smooth(method = "gam", color = "red", size = 1) +
  facet_wrap(~run) +
  labs(
    x = "Year",
    y = "Predicted Richness of Macrofungi",
    title = "Density Point Cloud Macrofungi species richness with GAM fitting"
  ) +
  coord_cartesian(ylim = c(0, 300))+
  theme_minimal()

#----------------------------------------------- V3
# Reduce dataset by 50%
reduced_data <- bayesian_results_all %>%
  sample_frac(0.5)

# Plot with reduced data
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  geom_pointdensity() +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "black", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Richness of Beetles",
    title = "Density Point Cloud of Predicted Beetle Richness (Reduced Data)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

 #360961 #000004
# Plot with reduced data
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_pointdensity() +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#F0F921", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Richness of Beetles",
    title = "Density Point Cloud of Predicted Macrofungi Richness"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# #360961 #000004
# Plot with reduced data
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_pointdensity() +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Richness of Macrofungi",
    title = "Density Point Cloud of Predicted Macrofungi Richness"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend


# Plot with transparency
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_pointdensity(alpha = 0.05) +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Richness of Macrofungi",
    title = "Density Point Cloud of Predicted Macrofungi Richness"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend








library(viridis)
# Reduce dataset by 50%
reduced_data <- bayesian_results_all %>%
  sample_frac(0.5)

# #360961 #000004
# Plot with reduced data
library(ggplot2)
library(ggpointdensity)
library(viridis)

ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_pointdensity(alpha = 1, size = 1.5) +  # Add transparency and adjust point size
  scale_color_viridis(option = "H", direction = -1, name = "Density") +  # Reverse color direction for better contrast
  stat_smooth(method = "gam", color = "#000004", size = 1.2) +  # GAM smoothing line with prominent color
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Richness of Macrofungi",
    title = "Density Shaded Points with Fitted Line"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    legend.position = "right",  # Adjust legend position
    panel.grid.minor = element_blank(),  # Simplify the grid
    text = element_text(size = 12)  # Adjust text size for clarity
  )

# V4

ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_pointdensity(alpha = 1, size = 1.5) +  # Add transparency and adjust point size
  scale_color_viridis(option = "H", direction = -1, name = "Density") +  # Reverse color direction for better contrast
  stat_smooth(method = "gam", color = "#000004", size = 1.2) +  # GAM smoothing line with prominent color
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Richness of Macrofungi",
    title = "Density Shaded Points with Fitted Line"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    legend.position = "right",  # Adjust legend position
    panel.grid.minor = element_blank(),  # Simplify the grid
    text = element_text(size = 12)  # Adjust text size for clarity
  )








#-------------------------------------------------------------------------------
# Version 3

library(ggplot2)
library(viridis)

ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_hex(bins = 30) +  # Hexagonal binning with 30 bins
  scale_fill_viridis(option = "H", guide = "colorbar") +  # Use Viridis color palette
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # GAM smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Richness of Macrofungi",
    title = "Hexagonal Density Shading of Predicted Macrofungi Richness"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend


#-------------------------------------------------------------------------------
# Heatmap of Species Richness Over Time

library(ggplot2)
library(viridis)

ggplot(bayesian_results_all, aes(x = year...1, y = run, fill = PRED_RICH_BEETLES)) +
  geom_tile() +  # Create heatmap tiles
  scale_fill_viridis_c(option = "H") +  # Fancy color palette
  labs(
    x = "Year",
    y = "Run",
    fill = "Beetle Richness",
    title = "Heatmap of Predicted Beetle Richness Over Time"
  ) +
  theme_minimal()


# Animated Temporal Change
# Use gganimate to show changes in predicted richness over time as an animation.
library(devtools) 
install.packages("RCurl")
library(RCurl)
install.packages("httr")
library(av) 
install.packages("gifski")

set_config( config( ssl_verifypeer = 0L ) )

devtools::install_github("RcppCore/Rcpp")

devtools::install_github("thomasp85/gganimate", force = TRUE)


library(ggplot2)
library(gganimate)

ggplot(bayesian_results_all, aes(x = year...1, y = PRED_RICH_BEETLES, color = run)) +
  geom_point(alpha = 0.5) +
  #stat_smooth(method = "loess", color = "red", se = FALSE) +
  scale_color_viridis_d() +
  labs(
    x = "Year",
    y = "Predicted Beetle Richness",
    title = "Temporal Change in Beetle Richness: {closest_state}",
    color = "Run"
  ) +
  theme_minimal() +
  transition_states(year...1, transition_length = 2, state_length = 1) +
  ease_aes('linear')


# Pairplot for Predictor Relationships
# Use GGally::ggpairs() to explore relationships between predictors like age, deadwood, lai_sim, and species richness.
# Why this works: It highlights correlations between predictors and species richness.

library(GGally)

# Select relevant columns
pairplot_data <- bayesian_results_all %>%
  select(age...2, deadwood...3, lai_sim...4, PRED_RICH_BEETLES)

# Create pairplot
ggpairs(pairplot_data, 
        aes(color = PRED_RICH_BEETLES > mean(PRED_RICH_BEETLES))) +
  scale_color_viridis_d() +
  labs(title = "Pairplot of Predictors and Beetle Richness")


# Ridge Plot for Richness Distribution
# Use a ridge plot to compare the distribution of predicted richness across run for a specific taxa.
# Why this works: It shows richness distributions across runs compactly while maintaining detail.
install.packages("ggridges")
library(ggridges)

ggplot(bayesian_results_all, aes(x = PRED_RICH_BEETLES, y = as.factor(run), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis_c() +
  labs(
    x = "Predicted Beetle Richness",
    y = "Run",
    title = "Distribution of Beetle Richness Across Runs"
  ) +
  theme_minimal()



# Interactive Plots with Plotly
# Make your scatterplots and line graphs interactive to explore richness trends dynamically.
# Why this works: Interactivity makes it easier to explore individual runs and identify outliers.
install.packages("plotly")
library(plotly)

p <- ggplot(bayesian_results_all, aes(x = year...1, y = PRED_RICH_BEETLES, color = run)) +
  geom_point(alpha = 0.3) +
  stat_smooth(method = "loess", se = FALSE, color = "red") +
  scale_color_viridis_d() +
  labs(
    x = "Year",
    y = "Predicted Beetle Richness",
    title = "Interactive Plot of Beetle Richness Over Time"
  ) +
  theme_minimal()

ggplotly(p)


# Parallel Coordinates Plot
# Visualize relationships across all predictors and richness in a multi-dimensional space.
# Why this works: It highlights multi-variable relationships at a glance.

library(GGally)

# Select key variables
parallel_data <- bayesian_results_all %>%
  select(age...2, deadwood...3, lai_sim...4, PRED_RICH_BEETLES, run)

# Parallel coordinates plot
ggparcoord(
  data = parallel_data,
  columns = 1:4,  # Columns to plot
  groupColumn = 5,  # Group by 'run'
  scale = "globalminmax"
) +
  scale_color_viridis_d() +
  labs(
    x = "Predictors",
    y = "Value",
    title = "Parallel Coordinates Plot of Predictors and Beetle Richness"
  ) +
  theme_minimal()


# Trellis Plot for Multiple Taxa
# Compare species richness trends across multiple taxa (e.g., beetles, moths, etc.) using faceted line plots.
# Why this works: It allows cross-taxa comparisons of richness trends.

library(tidyr)

# Reshape data to long format
long_data <- bayesian_results_all %>%
  select(year...1, PRED_RICH_BEETLES, PRED_RICH_MOTHS, PRED_RICH_MACROFUNGI) %>%
  pivot_longer(cols = starts_with("PRED_RICH"), names_to = "Taxa", values_to = "Richness")

# Plot
ggplot(long_data, aes(x = year...1, y = Richness, color = Taxa)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~Taxa, scales = "free_y") +
  scale_color_viridis_d() +
  labs(
    x = "Year",
    y = "Richness",
    title = "Species Richness Trends Across Taxa"
  ) +
  theme_minimal()

