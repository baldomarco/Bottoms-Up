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
library(ggpointdensity)
library(ggplot2)
library(viridis)

# Reduce dataset by 50%
reduced_data <- bayesian_results_all %>%
  sample_frac(0.05)

# BEETLES OFFICIAL PLOT V1

# Combined Plot with Separate Scales
A0 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  # Density point cloud
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  # Beta 1 and Beta 2 lines
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = BEETLES_PRED_RICH_50_beta1, linetype = "Beta1 * deadwood"), color = "#533600", size = 0.9, inherit.aes = FALSE) +
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = BEETLES_PRED_RICH_50_beta2, linetype = "Beta2 * ba_broadl"), color = "chocolate3", size = 0.9, inherit.aes = FALSE) +
  
  # Beetles function (single solid line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = BEETLES_PRED_RICH_50, linetype = "Non-Flying Beetles Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines at y = 4 and y = 37
  #geom_hline(yintercept = 4, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 6, color = "red", linetype = "dashed", size = 0.5) +
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
    title = "Density Point Cloud of Predicted Non-Flying Beetles Sp. Richness with Beta Functions",
    x = "Year",
    y = "Predicted Sp. Richness of Non-Flying Beetles"
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
 # geom_hline(yintercept = 4, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_hline(yintercept = 10, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 19, color = "red", linetype = "dashed", size = 0.5) +
  
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
    title = "Density Point Cloud of Predicted Bryophytes Sp. Richness with Beta Functions",
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
print(A1)


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
  #geom_hline(yintercept = 4, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 13, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 18, color = "red", linetype = "dashed", size = 0.5) +
  
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
    title = "Density Point Cloud of Predicted Lichens Sp. Richness with Beta Functions",
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
            aes(x = year, y = MACROFUNGI_PRED_RICH_50_beta4, linetype = "Beta4 * tree_10_40"), color = "#EEC9E5", size = 0.9, inherit.aes = FALSE) +
  
  # Bryophytes function (single solid line)
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = MACROFUNGI_PRED_RICH_50, linetype = "Macrofungi Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  # Horizontal dashed lines at y = 4 and y = 37
  #geom_hline(yintercept = 52, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 181, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_hline(yintercept = 219, color = "red", linetype = "dashed", size = 0.5) +
  
  # Custom linetype scale for beta lines
  scale_linetype_manual(
    values = c(
      "Beta1 * age" = "solid",
      "Beta2 * deadwood" = "solid",
      "Beta3 * ba_broadl" = "solid",
      "Beta4 * tree_10_40" = "solid",
      "Macrofungi Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  # Titles and axis labels
  labs(
    title = "Density Point Cloud of Predicted Macrofungi Sp. Richness with Beta Functions (obs richness 129 - 142)",
    x = "Year",
    y = "Predicted Sp. Richness of Macrofungi"
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
print(A3)


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
  #geom_hline(yintercept = 25, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 69, color = "red", linetype = "dashed", size = 0.5) +
  #geom_hline(yintercept = 106, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 96, color = "red", linetype = "dashed", size = 0.5) +
  
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
    title = "Density Point Cloud of Predicted Moths Sp. Richness with Beta Functions",
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
print(A5)


dev.off()

################################################################################
#_______________________________________________________________________________
# Reduce dataset by 50%
reduced_data <- bayesian_results_all %>%
  sample_frac(0.05)

# BEETLES OFFICIAL PLOT V4

# Replace plotID with row numbers
BDV_predictors$plotID <- seq_len(nrow(BDV_predictors))
# Rename column to 'beetles'
colnames(BDV_predictors)[which(colnames(BDV_predictors) == "Non-flying beetles (0.053)")] <- "Beetles"

# Combined Plot with Violin Plot and Dots
B0 <- ggplot(reduced_data, aes(x = year...1, y = PRED_RICH_BEETLES)) +
  geom_pointdensity(alpha = 0.05) + 
  scale_color_viridis(option = "H", name = "Density") + 
  
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = BEETLES_PRED_RICH_50_beta1, linetype = "Beta1 * deadwood"), color = "#533600", size = 0.9, inherit.aes = FALSE) +
  geom_line(data = Bayesian_BDV_model_V3_multi,
            aes(x = year, y = BEETLES_PRED_RICH_50_beta2, linetype = "Beta2 * ba_broadl"), color = "chocolate3", size = 0.9, inherit.aes = FALSE) +
  
  geom_line(data = Bayesian_BDV_model_V3_multi, 
            aes(x = year, y = BEETLES_PRED_RICH_50, linetype = "Non-Flying Beetles Function"),
            color = "darkred", size = 1, inherit.aes = FALSE) +
  
  geom_hline(yintercept = 6, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 15, color = "red", linetype = "dashed", size = 0.5) +
  
  
  geom_violin(data = BDV_predictors, 
              aes(x = plotID + 591, y = Beetles),
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +
  
  geom_jitter(data = BDV_predictors, 
              aes(x = plotID + 591, y = Beetles),
              color = "darkblue", alpha = 0.5, size = 0.7, width = 0.2, inherit.aes = FALSE) +
  
  
  scale_linetype_manual(
    values = c(
      "Beta1 * deadwood" = "solid",
      "Beta2 * ba_broadl" = "solid",
      "Non-Flying Beetles Function" = "solid"
    ),
    name = "Legend"
  ) +
  
  labs(
    title = "Density Point Cloud of Predicted Non-Flying Beetles Sp. Richness with Beta Functions and Observed Richness",
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
  geom_hline(yintercept = 10, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 19, color = "red", linetype = "dashed", size = 0.5) +
  
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
    title = "Density Point Cloud of Predicted Bryophytes Sp. Richness with Beta Functions",
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
  geom_hline(yintercept = 13, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 18, color = "red", linetype = "dashed", size = 0.5) +
  
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
    title = "Density Point Cloud of Predicted Lichens Sp. Richness with Beta Functions",
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
  geom_hline(yintercept = 181, color = "red", linetype = "dashed", size = 0.5) +
  
  geom_hline(yintercept = 219, color = "red", linetype = "dashed", size = 0.5) +
  
  
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
    title = "Density Point Cloud of Predicted Macromycetes Sp. Richness with Beta Functions",
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
  geom_hline(yintercept = 69, color = "red", linetype = "dashed", size = 0.5) +
  #geom_hline(yintercept = 106, color = "red", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 96, color = "red", linetype = "dashed", size = 0.5) +
  
  
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
    title = "Density Point Cloud of Predicted Moths Sp. Richness with Beta Functions",
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



































