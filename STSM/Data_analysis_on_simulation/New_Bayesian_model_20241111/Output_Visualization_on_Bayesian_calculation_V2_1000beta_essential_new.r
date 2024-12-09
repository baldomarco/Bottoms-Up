library(ggpointdensity)
library(ggplot2)
library(viridis)

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
    y = "Predicted Richness of Non-Flying Beetles",
    title = "Density Point Cloud with GAM fitting Non-Flying Beetles Sp. Richness"
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
    title = "Density Point Cloud with GAM fitting Bryophytes Sp. Richness"
  ) +
  #coord_cartesian(ylim = c(0, 30))+
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
    title = "Density Point Cloud with GAM fitting Lichens Sp. Richness"
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
    title = "Density Point Cloud with GAM fitting Macrofungi Sp. Richness"
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
    title = "Density Point Cloud with GAM fitting Macrofungi Red Listed Sp. Richness Over Weighted"
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
    title = "Density Point Cloud with GAM fitting Moths Sp. Richness"
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
    title = "Density Point Cloud with GAM fitting Moths Red Listed Species Richness Over Weighted"
  ) +
  coord_cartesian(ylim = c(0, 150))+
  theme_minimal()


#-------------------------------------------------------------------------------

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


#-------------------------------------------------------------------------------
# Plot with transparency

#-------------------------------------------------------------------------------
# Reduce Dataset Size (50%) and Use GAM

# Reduce dataset
reduced_data <- bayesian_results_all %>% sample_frac(0.5)

# Non-Flying Beetles
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  geom_pointdensity(alpha = 0.05) +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Sp. Richness of Non-Flying Beetles",
    title = "Density Point Cloud of Predicted Non-Flying Beetles Sp. Richness (obs 12 - 12)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Bryophytes
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BRYOPHYTES)) +
  geom_pointdensity(alpha = 0.05) +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Sp. Richness of Bryophytes",
    title = "Density Point Cloud of Predicted Bryophytes Sp. Richness (obs 37 - 21)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Lichens
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_LICHENS)) +
  geom_pointdensity(alpha = 0.05) +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Sp. Richness of Lichens",
    title = "Density Point Cloud of Predicted Lichens Sp. Richness (obs 41 - 23)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Macrofungi
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_pointdensity(alpha = 0.05) +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Sp. Richness of Macrofungi",
    title = "Density Point Cloud of Predicted Macrofungi Sp. Richness (obs 268 - 237)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Macrofungi Red
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI_RED)) +
  geom_pointdensity(alpha = 0.05) +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Red List Sp. Richness of Macrofungi Over Weighted",
    title = "Density Point Cloud of Predicted Macrofungi Red List Sp. Richness Over Weighted (obs 408 - 367)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Moths
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS)) +
  geom_pointdensity(alpha = 0.05) +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Sp. Richness of Moths",
    title = "Density Point Cloud of Predicted Moths Sp. Richness (obs 72 - 55)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Moths Red
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS_RED)) +
  geom_pointdensity(alpha = 0.05) +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Red List Sp. Richness of Moths Over Weighted",
    title = "Density Point Cloud of Predicted Moths Red List Sp. Richness Over Weighted (obs 82 - 55)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

#-------------------------------------------------------------------------------
# Plot without transparency - Viridis palette H

#-------------------------------------------------------------------------------
# Reduce Dataset Size (50%) and Use GAM

# Reduce dataset
reduced_data <- bayesian_results_all %>% sample_frac(0.5)

# Non-Flying Beetles
D0 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
        geom_pointdensity() +  # Density point cloud
        scale_color_viridis(option = "H")+
        stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
        facet_wrap(~run) +  # Facet by 'run'
        labs(
          x = "Year",
          y = "Predicted Sp. Richness of Non-Flying Beetles",
          title = "Density Point Cloud of Predicted Non-Flying Beetles Sp. Richness (obs 12 - 12)"
        ) +
       theme_minimal() +  # Clean theme
       theme(legend.position = "right")  # Adjust legend

# Bryophytes
D1<- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BRYOPHYTES)) +
       geom_pointdensity() +  # Density point cloud
       scale_color_viridis(option = "H")+
       stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
       facet_wrap(~run) +  # Facet by 'run'
       labs(
         x = "Year",
         y = "Predicted Sp. Richness of Bryophytes",
         title = "Density Point Cloud of Predicted Bryophytes Sp. Richness (obs 37 - 21)"
        ) +
        theme_minimal() +  # Clean theme
        theme(legend.position = "right")  # Adjust legend

# Lichens
D2 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_LICHENS)) +
        geom_pointdensity() +  # Density point cloud
        scale_color_viridis(option = "H")+
        stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
        facet_wrap(~run) +  # Facet by 'run'
        labs(
          x = "Year",
          y = "Predicted Sp. Richness of Lichens",
        title = "Density Point Cloud of Predicted Lichens Sp. Richness (obs 41 - 23)"
        ) +
        theme_minimal() +  # Clean theme
        theme(legend.position = "right")  # Adjust legend

# Macrofungi
D3 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
        geom_pointdensity() +  # Density point cloud
        scale_color_viridis(option = "H")+
        stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
        facet_wrap(~run) +  # Facet by 'run'
        labs(
          x = "Year",
          y = "Predicted Sp. Richness of Macrofungi",
          title = "Density Point Cloud of Predicted Macrofungi Sp. Richness (obs 268 - 237)"
         ) +
         theme_minimal() +  # Clean theme
         theme(legend.position = "right")  # Adjust legend

# Macrofungi Red
D4 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI_RED)) +
        geom_pointdensity() +  # Density point cloud
        scale_color_viridis(option = "H")+
        stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
        facet_wrap(~run) +  # Facet by 'run'
        labs(
          x = "Year",
          y = "Predicted Red List Sp. Richness of Macrofungi Over Weighted",
          title = "Density Point Cloud of Predicted Macrofungi Red List Sp. Richness Over Weighted (obs 408 - 367)"
        ) +
        theme_minimal() +  # Clean theme
        theme(legend.position = "right")  # Adjust legend

# Moths
D5 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS)) +
        geom_pointdensity() +  # Density point cloud
        scale_color_viridis(option = "H")+
        stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
        facet_wrap(~run) +  # Facet by 'run'
        labs(
          x = "Year",
          y = "Predicted Sp. Richness of Moths",
          title = "Density Point Cloud of Predicted Moths Sp. Richness (obs 72 - 55)"
        ) +
        theme_minimal() +  # Clean theme
        theme(legend.position = "right")  # Adjust legend

# Moths Red
D6 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS_RED)) +
        geom_pointdensity() +  # Density point cloud
        scale_color_viridis(option = "H")+
        stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
        facet_wrap(~run) +  # Facet by 'run'
        labs(
          x = "Year",
          y = "Predicted Red List Sp. Richness of Moths Over Weighted",
          title = "Density Point Cloud of Predicted Moths Red List Sp. Richness Over Weighted (obs 82 - 55)"
        ) +
        theme_minimal() +  # Clean theme
        theme(legend.position = "right")  # Adjust legend

#-------------------------------------------------------------------------------
# Plot without transparency - viridis palette H inverted

#-------------------------------------------------------------------------------
# Reduce Dataset Size (50%) and Use GAM

# Reduce dataset
reduced_data <- bayesian_results_all %>% sample_frac(0.5)

# Non-Flying Beetles
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  geom_pointdensity(alpha = 1) +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Sp. Richness of Non-Flying Beetles",
    title = "Density Point Cloud of Predicted Non-Flying Beetles Sp. Richness (obs 12 - 12)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Bryophytes
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BRYOPHYTES)) +
  geom_pointdensity(alpha = 1) +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Sp. Richness of Bryophytes",
    title = "Density Point Cloud of Predicted Bryophytes Sp. Richness (obs 37 - 21)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Lichens
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_LICHENS)) +
  geom_pointdensity(alpha = 1) +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Sp. Richness of Lichens",
    title = "Density Point Cloud of Predicted Lichens Sp. Richness (obs 41 - 23)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Macrofungi
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_pointdensity(alpha = 1) +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Sp. Richness of Macrofungi",
    title = "Density Point Cloud of Predicted Macrofungi Sp. Richness (obs 268 - 237)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Macrofungi Red
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI_RED)) +
  geom_pointdensity(alpha = 1) +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Red List Sp. Richness of Macrofungi Over Weighted",
    title = "Density Point Cloud of Predicted Macrofungi Red List Sp. Richness Over Weighted (obs 408 - 367)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Moths
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS)) +
  geom_pointdensity(alpha = 1) +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Sp. Richness of Moths",
    title = "Density Point Cloud of Predicted Moths Sp. Richness (obs 72 - 55)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Moths Red
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS_RED)) +
  geom_pointdensity(alpha = 1) +  # Density point cloud
  scale_color_viridis(option = "H")+
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # gam smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Red List Sp. Richness of Moths Over Weighted",
    title = "Density Point Cloud of Predicted Moths Red List Sp. Richness Over Weighted (obs 82 - 55)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Plots Version 3 Hexagonal

# Non-Flying Beetles
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  geom_hex(bins = 30) +  # Hexagonal binning with 30 bins
  scale_fill_viridis(option = "H", guide = "colorbar") +  # Use Viridis color palette
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # GAM smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Non-Flying Beetles Sp. Richness",
    title = "Hexagonal Density Point Cloud of Predicted Non-Flying Beetles Sp. Richness (obs 12 - 12)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Bryophytes
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BRYOPHYTES)) +
  geom_hex(bins = 30) +  # Hexagonal binning with 30 bins
  scale_fill_viridis(option = "H", guide = "colorbar") +  # Use Viridis color palette
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # GAM smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Bryophytes Sp. Richness",
    title = "Hexagonal Density Point Cloud of Predicted Bryophytes Sp. Richness (obs 37 - 21)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Lichens
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_LICHENS)) +
  geom_hex(bins = 30) +  # Hexagonal binning with 30 bins
  scale_fill_viridis(option = "H", guide = "colorbar") +  # Use Viridis color palette
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # GAM smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Lichens Sp. Richness",
    title = "Hexagonal Density Point Cloud of Predicted Lichens Sp. Richness (obs 41 - 23)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Macrofungi
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
  geom_hex(bins = 30) +  # Hexagonal binning with 30 bins
  scale_fill_viridis(option = "H", guide = "colorbar") +  # Use Viridis color palette
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # GAM smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Macrofungi Sp. Richness",
    title = "Hexagonal Density Point Cloud of Predicted Macrofungi Sp. Richness (obs 268 - 237)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Macrofungi Red List
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI_RED)) +
  geom_hex(bins = 30) +  # Hexagonal binning with 30 bins
  scale_fill_viridis(option = "H", guide = "colorbar") +  # Use Viridis color palette
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # GAM smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Red List Macrofungi Sp. Richness",
    title = "Hexagonal Density Point Cloud of Predicted Red List Macrofungi Sp. Richness (obs 408 - 367)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Moths
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS)) +
  geom_hex(bins = 30) +  # Hexagonal binning with 30 bins
  scale_fill_viridis(option = "H", guide = "colorbar") +  # Use Viridis color palette
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # GAM smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Moths Sp. Richness",
    title = "Hexagonal Density Point Cloud of Predicted Moths Sp. Richness (obs 72 - 55)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend

# Moths Red List
ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS_RED)) +
  geom_hex(bins = 30) +  # Hexagonal binning with 30 bins
  scale_fill_viridis(option = "H", guide = "colorbar") +  # Use Viridis color palette
  stat_smooth(method = "gam", color = "#000004", size = 1) +  # GAM smoothing line
  facet_wrap(~run) +  # Facet by 'run'
  labs(
    x = "Year",
    y = "Predicted Red List Moths Sp. Richness",
    title = "Hexagonal Density Point Cloud of Predicted Red List Moths Sp. Richness (obs 82 - 55)"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "right")  # Adjust legend


#-------------------------------------------------------------------------------
# PLOT DOUBLE Y AXIS WITH AGE AND 50% PRED CURVE
#-------------------------------------------------------------------------------







#-------------------------------------------------------------------------------
# PLOT BETA COEFF SEPARATED AND 50% PRED CURVE 
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Bryophytes

b1 <- ggplot(Bayesian_BDV_model_V3_multi, aes(year, BRYO_PRED_RICH_50_beta1)) +
  geom_line() +
  ggtitle("Isolated [Beta 1 * AGE] BRYOPHYTES Time Series") +
  labs(x =  "Year", y = " Beta 1 * Age") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~run, ncol=2)+
  theme_bw()

b2 <- ggplot(Bayesian_BDV_model_V3_multi, aes(year, BRYO_PRED_RICH_50_beta2)) +
  geom_line() +
  ggtitle("Isolated [Beta 2 * Deaewood Volume] BRYOPHYTES Time Series") +
  labs(x =  "Year", y = " Beta 2 * DW Vol") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~run, ncol=2)+
  theme_bw()


# Plot grid arrange
grid.arrange(b1,b2,D1, ncol=1)


# IN CASE WANNA PLOT THEM TOGETHER
# Plotting beta1 and beta2 over time
B1 <- ggplot(Bayesian_BDV_model_V3_multi, aes(x = year)) +
  geom_line(aes(y = BRYO_PRED_RICH_50_beta1, color = "Beta 1 age"), size = 0.5) +
  geom_line(aes(y = BRYO_PRED_RICH_50_beta2, color = "Beta 2 deadwood"), size = 0.5) +
  geom_line(aes(y = BRYO_PRED_RICH_50, color = "Y"), size = 0.5) +
  labs(title = "Time Series of Bryophytes in function of Beta 1 and Beta 2",
       x = "Year",
       y = "Values") +
  theme_minimal() +
  facet_wrap(~run, ncol=2)+
  scale_color_manual(values = c("Beta 1 age" = "blue", "Beta 2 deadwood" = "black", "Y" = "chocolate3")) +
  scale_x_continuous(breaks = seq(min(Bayesian_BDV_model_V3_multi$year), max(Bayesian_BDV_model_V3_multi$year), by = 40),
                     labels = seq(min(Bayesian_BDV_model_V3_multi$year), max(Bayesian_BDV_model_V3_multi$year), by = 40))+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

# Plot grid arrange
grid.arrange(B1,P1, ncol=1)


#-------------------------------------------------------------------------------
# Lichens

# Plotting beta1, beta2, ... etc over time with Y

L1 <- ggplot(Bayesian_BDV_model_V3_multi, aes(x = year)) +
  geom_line(aes(y = LICHEN_PRED_RICH_50_beta1, color = "Beta 1 age"), size = 0.5) +
  geom_line(aes(y = LICHEN_PRED_RICH_50_beta2, color = "Beta 2 lai_sim"), size = 0.5) +
  geom_line(aes(y = LICHEN_PRED_RICH_50_beta3, color = "Beta 3 broadl_40_1"), size = 0.5) +
  geom_line(aes(y = LICHEN_PRED_RICH_50, color = "Y"), size = 0.5) +
  labs(title = "Time Series of Lichens in function of Beta 1 to 3",
       x = "Year",
       y = "Values") +
  theme_minimal() +
  facet_wrap(~run, ncol=2)+
  scale_color_manual(values = c("Beta 1 age" = "blue", "Beta 2 lai_sim" = "#8FBC8F", "Beta 3 broadl_40_1" = "#FF1493",  "Y" = "chocolate3")) +
  scale_x_continuous(breaks = seq(min(Bayesian_BDV_model_V3_multi$year), max(Bayesian_BDV_model_V3_multi$year), by = 40),
                     labels = seq(min(Bayesian_BDV_model_V3_multi$year), max(Bayesian_BDV_model_V3_multi$year), by = 40)) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

# Plot grid arrange
grid.arrange(L1, P2, ncol=1)


#-------------------------------------------------------------------------------
# Macrofungi

# Plotting beta1, beta2, ... etc over time with Y

MF1 <- ggplot(Bayesian_BDV_model_V3_multi, aes(x = year)) +
  geom_line(aes(y = MACROFUNGI_PRED_RICH_50_beta1, color = "Beta 1 age"), size = 0.5) +
  geom_line(aes(y = MACROFUNGI_PRED_RICH_50_beta2, color = "Beta 2 deadwood"), size = 0.5) +
  geom_line(aes(y = MACROFUNGI_PRED_RICH_50_beta3, color = "Beta 3 ba_broadl"), size = 0.5) +
  geom_line(aes(y = MACROFUNGI_PRED_RICH_50_beta4, color = "Beta 4 tree_10_40"), size = 0.5) +
  geom_line(aes(y = MACROFUNGI_PRED_RICH_50, color = "Y"), size = 0.5) +
  labs(title = "Time Series of Macrofungi in function of Beta 1 to 4",
       x = "Year",
       y = "Values") +
  theme_minimal() +
  facet_wrap(~run, ncol=2)+
  scale_color_manual(values = c("Beta 1 age" = "blue", "Beta 2 deadwood" = "black", "Beta 3 ba_broadl" = "#8FBC8F", "Beta 4 tree_10_40" = "#FF1493",  "Y" = "chocolate3")) +
  scale_x_continuous(breaks = seq(min(Bayesian_BDV_model_V3_multi$year), max(Bayesian_BDV_model_V3_multi$year), by = 40),
                     labels = seq(min(Bayesian_BDV_model_V3_multi$year), max(Bayesian_BDV_model_V3_multi$year), by = 40))+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

# Plot grid arrange
grid.arrange(MF1,P3, ncol=1)


#-------------------------------------------------------------------------------
# Red Listed - Macrofungi

# Plotting beta1, beta2, ... etc over time with Y

R_MF1 <- ggplot(Bayesian_BDV_model_V3_multi, aes(x = year)) +
  geom_line(aes(y = MACROFUNGI_RED_PRED_RICH_50_beta1, color = "Beta 1 deadwood"), size = 0.5) +
  geom_line(aes(y = MACROFUNGI_RED_PRED_RICH_50_beta2, color = "Beta 2 ba_broadl"), size = 0.5) +
  geom_line(aes(y = MACROFUNGI_RED_PRED_RICH_50, color = "Y"), size = 0.5) +
  labs(title = "Time Series of Red List Macrofungi in function of Beta 1 to 3",
       x = "Year",
       y = "Values") +
  theme_minimal() +
  facet_wrap(~run, ncol=2)+
  scale_color_manual(values = c("Beta 1 deadwood" = "black", "Beta 2 ba_broadl" = "#8FBC8F",  "Y" = "chocolate3")) +
  scale_x_continuous(breaks = seq(min(Bayesian_BDV_model_V3_multi$year), max(Bayesian_BDV_model_V3_multi$year), by = 40),
                     labels = seq(min(Bayesian_BDV_model_V3_multi$year), max(Bayesian_BDV_model_V3_multi$year), by = 40))+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

# Plot grid arrange
grid.arrange(R_MF1,P4, ncol=1)

#-------------------------------------------------------------------------------
# BEETLES

# Plotting beta1, beta2, ... etc over time with Y

NFB1 <- ggplot(Bayesian_BDV_model_V3_multi, aes(x = year)) +
  geom_line(aes(y = BEETLES_PRED_RICH_50_beta1, color = "Beta 1 deadwood"), size = 0.5) +
  geom_line(aes(y = BEETLES_PRED_RICH_50_beta2, color = "Beta 2 ba_broadl"), size = 0.5) +
  geom_line(aes(y = BEETLES_PRED_RICH_50, color = "Y"), size = 0.5) +
  labs(title = "Time Series of Non-flying Beetles in function of Beta 1 to 3",
       x = "Year",
       y = "Values") +
  theme_minimal() +
  facet_wrap(~run, ncol=2)+
  scale_color_manual(values = c("Beta 1 deadwood" = "black", "Beta 2 ba_broadl" = "#8FBC8F",  "Y" = "chocolate3")) +
  scale_x_continuous(breaks = seq(min(Bayesian_BDV_model_V3_multi$year), max(Bayesian_BDV_model_V3_multi$year), by = 40),
                     labels = seq(min(Bayesian_BDV_model_V3_multi$year), max(Bayesian_BDV_model_V3_multi$year), by = 40))+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

# Plot grid arrange
grid.arrange(NFB1,P5, ncol=1)


#-------------------------------------------------------------------------------
# MOTHS 

# Plotting beta1, beta2, ... etc over time with Y

M1 <- ggplot(Bayesian_BDV_model_V3_multi, aes(x = year)) +
  geom_line(aes(y = MOTHS_PRED_RICH_50_beta1, color = "Beta 1 tree_10_40_2"), size = 0.5) +
  geom_line(aes(y = MOTHS_PRED_RICH_50_beta2, color = "Beta 2 broadl_40"), size = 0.5) +
  geom_line(aes(y = MOTHS_PRED_RICH_50, color = "Y"), size = 0.5) +
  labs(title = "Time Series of Red List Moths in function of Beta 1 to 2",
       x = "Year",
       y = "Values") +
  theme_minimal() +
  facet_wrap(~run, ncol=2)+
  scale_color_manual(values = c("Beta 1 tree_10_40_2" = "black", "Beta 2 broadl_40" = "#8FBC8F",  "Y" = "chocolate3")) +
  scale_x_continuous(breaks = seq(min(Bayesian_BDV_model_V3_multi$year), max(Bayesian_BDV_model_V3_multi$year), by = 40),
                     labels = seq(min(Bayesian_BDV_model_V3_multi$year), max(Bayesian_BDV_model_V3_multi$year), by = 40))+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

# Plot grid arrange
grid.arrange(M1,P6, ncol=1)


#---------------------------------------------------------------------------------
# Red Listed - Moths

# Plotting beta1, beta2, ... etc over time with Y

R_M1 <- ggplot(Bayesian_BDV_model_V3_multi, aes(x = year)) +
  geom_line(aes(y = MOTHS_RED_PRED_RICH_50_beta1, color = "Beta 1 tree_10_40_2"), size = 0.5) +
  geom_line(aes(y = MOTHS_RED_PRED_RICH_50_beta2, color = "Beta 2 broadl_40"), size = 0.5) +
  geom_line(aes(y = MOTHS_RED_PRED_RICH_50, color = "Y"), size = 0.5) +
  labs(title = "Time Series of Red List Moths in function of Beta 1 to 2",
       x = "Year",
       y = "Values") +
  theme_minimal() +
  facet_wrap(~run, ncol=2)+
  scale_color_manual(values = c("Beta 1 tree_10_40_2" = "black", "Beta 2 broadl_40" = "#8FBC8F",  "Y" = "chocolate3")) +
  scale_x_continuous(breaks = seq(min(Bayesian_BDV_model_V3_multi$year), max(Bayesian_BDV_model_V3_multi$year), by = 40),
                     labels = seq(min(Bayesian_BDV_model_V3_multi$year), max(Bayesian_BDV_model_V3_multi$year), by = 40))+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

# Plot grid arrange
grid.arrange(R_M1,P7, ncol=1)








#-------------------------------------------------------------------------------
# Reduce dataset by 50%
reduced_data <- bayesian_results_all %>%
  sample_frac(0.5)

# BEETLES OFFICIAL PLOT V1

# Combined Plot with Separate Scales
A0 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Beta 1 and Beta 2 lines
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = BEETLES_PRED_RICH_50_beta1, linetype = "Beta1 * deadwood"), color = "#00A091", size = 0.9, inherit.aes = FALSE) +
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = BEETLES_PRED_RICH_50_beta2, linetype = "Beta2 * ba_broadl"), color = "chocolate3", size = 0.9, inherit.aes = FALSE) +
  
  # Beetles function (single solid line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = BEETLES_PRED_RICH_50, linetype = "Non-Flying Beetles Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines at y = 4 and y = 37
  geom_hline(yintercept = 4, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 15, color = "red", linetype = "dashed", size = 0.5) +
  
  # Custom linetype scale for beta lines
  scale_linetype_manual(
    values = c(
      "Beta1 * deadwood" = "solid",
      "Beta2 * ba_broadl" = "solid",
      "Non-Flying Beetles Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "Density Point Cloud of Predicted Non-Flying Beetles Sp. Richness with Beta Functions (obs richness 12 - 12)",
    x = "Year",
    y = "Predicted Sp. Richness of Non-Flying Beetles"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 2) +
  
  # Themes and adjustments
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(A0)


#-------------------------------------------------------------------------------
# BRYOPHYTES OFFICIAL PLOT V1

A1 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BRYOPHYTES)) +
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
  geom_hline(yintercept = 4, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 37, color = "red", linetype = "dashed", size = 0.5) +
  
  # Custom linetype scale for beta lines
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
    title = "Density Point Cloud of Predicted Bryophytes Sp. Richness with Beta Functions (obs richness 37 - 21)",
    x = "Year",
    y = "Predicted Sp. Richness of Bryophytes"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 2) +
  
  # Themes and adjustments
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(A1)



#-------------------------------------------------------------------------------
# Combined Plot (Alternative)
A1_alt <- ggplot() +
  # Density point cloud using stat_density_2d
  stat_density_2d(data = reduced_data, 
                  aes(x = year...1, y = PRED_RICH_BRYOPHYTES, fill = ..density..), 
                  geom = "tile", contour = FALSE, alpha = 0.6) +
  scale_fill_viridis_c(option = "H", direction = -1, name = "Density") +
  
  # Beta 1 and Beta 2 lines
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = BRYO_PRED_RICH_50_beta1, color = "Beta 1 age"), size = 0.5) +
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = BRYO_PRED_RICH_50_beta2, color = "Beta 2 deadwood"), size = 0.5) +
  
  # BRYO_PRED_RICH_50 as the function curve
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = BRYO_PRED_RICH_50, color = "Bryophytes Function"),
            size = 1) +
  
  # Custom scales
  scale_color_manual(
    values = c(
      "Beta 1 age" = "#00A091",
      "Beta 2 deadwood" = "#533600",
      "Bryophytes Function" = "darkred"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "Density Point Cloud of Predicted Bryophytes Sp. Richness with Beta Functions (obs richness 37 - 21)",
    x = "Year",
    y = "Values"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 2) +
  
  # Themes and adjustments
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
# print(A1_alt)

#-------------------------------------------------------------------------------
# LICHENS OFFICIAL PLOT V1

A2 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_LICHENS)) +
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
  geom_hline(yintercept = 4, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 71, color = "red", linetype = "dashed", size = 0.5) +
  
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
    title = "Density Point Cloud of Predicted Lichens Sp. Richness with Beta Functions (obs richness 41 - 23)",
    x = "Year",
    y = "Predicted Sp. Richness of Lichens"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 2) +
  
  # Themes and adjustments
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(A2)

#-------------------------------------------------------------------------------
# MACROFUNGI OFFICIAL PLOT V1

A3 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI)) +
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
            aes(x = year, y = MACROFUNGI_PRED_RICH_50_beta4, linetype = "Beta3 * tree_10_40"), color = "#EEC9E5", size = 0.9, inherit.aes = FALSE) +
  
  # Bryophytes function (single solid line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = MACROFUNGI_PRED_RICH_50, linetype = "Macrofungi Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines at y = 4 and y = 37
  geom_hline(yintercept = 52, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 284, color = "red", linetype = "dashed", size = 0.5) +
  
  # Custom linetype scale for beta lines
  scale_linetype_manual(
    values = c(
      "Beta1 * age" = "solid",
      "Beta2 * deadwood" = "solid",
      "Beta3 * ba_broadl = solid",
      "Beta3 * tree_10_40 = solid",
      "Macrofungi Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "Density Point Cloud of Predicted Macrofungi Sp. Richness with Beta Functions (obs richness 268 - 237)",
    x = "Year",
    y = "Predicted Sp. Richness of Macrofungi"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 2) +
  
  # Themes and adjustments
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(A3)

#-------------------------------------------------------------------------------
# MACROFUNGI RED OFFICIAL PLOT V1

A4 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MACROFUNGI_RED)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Beta 1 and Beta 2 lines
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = MACROFUNGI_RED_PRED_RICH_50_beta1, linetype = "Beta1 * deadwood"), color = "#00A091", size = 0.9, inherit.aes = FALSE) +
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = MACROFUNGI_RED_PRED_RICH_50_beta2, linetype = "Beta2 * ba_broadl"), color = "chocolate3", size = 0.9, inherit.aes = FALSE) +

  # Bryophytes function (single solid line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = MACROFUNGI_RED_PRED_RICH_50, linetype = "Macrofungi Red List Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines at y = 4 and y = 37
  geom_hline(yintercept = 75, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 494, color = "red", linetype = "dashed", size = 0.5) +
  
  # Custom linetype scale for beta lines
  scale_linetype_manual(
    values = c(
      "Beta1 * deadwood" = "solid",
      "Beta2 * ba_broadl" = "solid",
      "Macrofungi Red List Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "Density Point Cloud of Predicted Macrofungi Red List Sp. Richness with Beta Functions (obs richness 408 - 367)",
    x = "Year",
    y = "Predicted Sp. Richness of Red List Macrofungi Over Weighted"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 2) +
  
  # Themes and adjustments
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(A4)

#-------------------------------------------------------------------------------
# MOTHS OFFICIAL PLOT V1

A5 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS)) +
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
  geom_hline(yintercept = 25, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 106, color = "red", linetype = "dashed", size = 0.5) +
  
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
    title = "Density Point Cloud of Predicted Moths Sp. Richness with Beta Functions (obs richness 72 - 55)",
    x = "Year",
    y = "Predicted Sp. Richness of Moths"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 2) +
  
  # Themes and adjustments
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(A5)

#-------------------------------------------------------------------------------
# MOTHS RED OFFICIAL PLOT V1

A6 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_MOTHS_RED)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Beta 1 and Beta 2 lines
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = MOTHS_RED_PRED_RICH_50_beta1, linetype = "Beta1 * tree_10_40_2"), color = "#EEC9E5", size = 0.9, inherit.aes = FALSE) +
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = MOTHS_RED_PRED_RICH_50_beta2, linetype = "Beta2 * broadl_40"), color = "chocolate3", size = 0.9, inherit.aes = FALSE) +
  
  # MOTHS function (single solid line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = MOTHS_RED_PRED_RICH_50, linetype = "Moths Red List Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines at y = 25 and y = 116
  geom_hline(yintercept = 25, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 116, color = "red", linetype = "dashed", size = 0.5) +
  
  # Custom linetype scale for beta lines
  scale_linetype_manual(
    values = c(
      "Beta1 * tree_10_40_2" = "solid",
      "Beta2 * broadl_40" = "solid",
      "Moths Red List Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "Density Point Cloud of Predicted Moths Red List Sp. Richness with Beta Functions (obs richness 82 - 55)",
    x = "Year",
    y = "Predicted Sp. Richness of Red List Moths Over Weighted"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 2) +
  
  # Themes and adjustments
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(A6)




dev.off()

















































#############
WORKING IN PROGRESSS
#############
install.packages("ggnewscale")
library(ggnewscale)


A1 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BRYOPHYTES)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Beta 1 and Beta 2 lines
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = BRYO_PRED_RICH_50_beta1, linetype = "Beta 1 age"), color = "#00A091", size = 0.9, inherit.aes = FALSE) +
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = BRYO_PRED_RICH_50_beta2, linetype = "Beta 2 deadwood"), color = "#533600", size = 0.9, inherit.aes = FALSE) +
  
  # Bryophytes function (single solid line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = BRYO_PRED_RICH_50, linetype = "Bryophytes Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines at y = 4 and y = 37
  geom_hline(yintercept = 4, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 37, color = "red", linetype = "dashed", size = 0.5) +
  
  # Secondary y-axis for 'age'
  new_scale("y") + 
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = age / max(age) * max(BRYO_PRED_RICH_50)),  
            color = "#000004", size = 0.9, inherit.aes = FALSE) +
  scale_y_continuous(
    name = "Predicted Sp. Richness of Bryophytes",
    sec.axis = sec_axis(~ . * max(age) / max(BRYO_PRED_RICH_50), name = "Original Age Time Series")
  ) +
  
  # Adding the function annotation
  annotate("text", x = 1950, y = 25, label = "BRYO_PRED_RICH_50 = 9.71337 + 0.01584 * age + 0.00004 * deadwood", 
           color = "black", size = 4, hjust = 0) +
  
  # Titles and axis labels
  labs(
    title = "Density Point Cloud of Predicted Bryophytes Sp. Richness with Beta Functions (obs richness 37 - 21)",
    x = "Year",
    y = "Predicted Sp. Richness of Bryophytes"
  ) +
  
  # Facet by 'run'
  facet_wrap(~run, ncol = 2) +
  
  # Themes and adjustments
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "right"
  )

# Print the plot
print(A1)













library(ggplot2)
library(ggpointdensity)
library(viridis)

# Reduce dataset by 50%
reduced_data <- bayesian_results_all %>%
  sample_frac(0.5)

# #360961 #000004
# Plot with reduced data

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

