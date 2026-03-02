              # # SURVIVAL TEST V9 #



library(survival)
library(survminer)
library(dplyr)
library(ggpubr)
library(cowplot)

Recovery_wide_df <- read.csv("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output - Copy/recovery_analysis_by_taxon_climate_wide_xCompare.csv")
Recovery_wide_df

# Colors
management_colors <- c(
  "Native Broadleaves" = "chocolate",
  "Non-Native Coniferous" = "darkgreen"
)

# Taxa and climates
all_taxa <- c("BEETLES", "BRYOPHYTES", "LICHENS", "MACROFUNGI", "MOTHS")
climates <- c("1961_1990", "1991_2020") # simplified labels

# Data collectors
all_plot_data <- data.frame()
p_value_annotations <- data.frame()

for (current_taxon in all_taxa) {
  message(paste0("\n--- Processing Taxon: ", current_taxon, " ---"))
  
  # Find taxon-specific climate columns
  taxon_cols <- grep(paste0("^", current_taxon, "_CLIM"), colnames(Recovery_wide_df), value = TRUE)
  if (length(taxon_cols) == 0) next
  
  # Reshape wide → long
  temp_df <- Recovery_wide_df %>%
    select(plotID, forest_cat, StandAge, all_of(taxon_cols)) %>%
    tidyr::pivot_longer(
      cols = all_of(taxon_cols),
      names_to = "clim",
      values_to = "year"
    ) %>%
    mutate(
      clim = case_when(
        grepl("1961_1990", clim) ~ "Climate Series 1961 - 1990",
        grepl("1991_2020", clim) ~ "Climate Series 1991 - 2020",
        TRUE ~ clim
      ),
      recovered = ifelse(is.na(year), 0, 1),
      time = ifelse(is.na(year), 266, year),
      surv_obj = Surv(time, recovered),
      Taxon = current_taxon
    ) %>%
    filter(!is.na(forest_cat))
  
  # Skip if no usable data
  if (nrow(temp_df) < 5 || n_distinct(temp_df$forest_cat) < 2) next
  
  # Cox model with StandAge² and climate
  cox_model <- coxph(surv_obj ~ forest_cat + StandAge + I(StandAge^2) + clim, data = temp_df)
  cox_summary <- summary(cox_model)
  cox_summary
  
  # Extract global likelihood ratio p-value
  p_value <- cox_summary$sctest["pvalue"]
  p_value_text <- paste0("Cox p = ", format.pval(p_value, digits = 3))
  
  # Save annotation
  p_value_annotations <- bind_rows(p_value_annotations,
                                   data.frame(Taxon = current_taxon,
                                              Climate = unique(temp_df$clim),
                                              p_value_label = p_value_text,
                                              x = 10, y = 1.20))
  
  # Prediction grid
  mean_age <- mean(temp_df$StandAge, na.rm = TRUE)
  new_data <- expand.grid(
    forest_cat = levels(factor(temp_df$forest_cat)),
    clim = unique(temp_df$clim),
    StandAge = mean_age
  )
  
  fit <- survfit(cox_model, newdata = new_data)
  
  # Convert to df for plotting
  fit_df <- surv_summary(fit) %>%
    mutate(Taxon = current_taxon) %>%
    mutate(clim = rep(new_data$clim, each = length(unique(time)))) %>%
    mutate(forest_cat = rep(new_data$forest_cat, each = length(unique(time))))
  
  all_plot_data <- bind_rows(all_plot_data, fit_df)
}

# Factor ordering
all_plot_data$Taxon <- factor(all_plot_data$Taxon, levels = all_taxa)
all_plot_data$clim <- factor(all_plot_data$clim,
                             levels = c("Climate Series 1961 - 1990", "Climate Series 1991 - 2020"))
p_value_annotations$Taxon <- factor(p_value_annotations$Taxon, levels = all_taxa)
p_value_annotations$Climate <- factor(p_value_annotations$Climate,
                                      levels = c("Climate Series 1961 - 1990", "Climate Series 1991 - 2020"))



#-------------------------------------------------------------------------------
# Plot n 1
final_plot <- ggplot(all_plot_data, aes(x = time, y = 1 - surv, color = forest_cat, fill = forest_cat)) +
  geom_step(size = 1.2) +
  geom_ribbon(aes(ymin = 1 - upper, ymax = 1 - lower), alpha = 0.3, color = NA) +
  scale_color_manual(values = management_colors) +
  scale_fill_manual(values = management_colors) +
  labs(
    title = "Recovery by Forest Types × Climate",
    x = "Simulation Year", y = "Proportion recovered",
    color = "Forest Type", fill = "Forest Type"
  ) +
  ylim(0, 1.25) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  facet_grid(clim ~ Taxon, switch = "y") +
  geom_text(data = p_value_annotations,
            aes(x = 10, y = 1.20, label = p_value_label),
            inherit.aes = FALSE, hjust = 0, size = 3.5, fontface = "italic") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text.x = element_text(face = "bold", size = 12),
    strip.text.y = element_text(face = "bold", size = 12, angle = 270),
    strip.background = element_rect(fill = "grey90", color = "black"),
    panel.border = element_rect(color = "black", fill = NA)
  )


# plot 2
final_plot <- ggplot(all_plot_data, aes(x = time, y = 1 - surv, color = forest_cat, fill = forest_cat)) +
  geom_step(size = 1.2) +
  geom_ribbon(aes(ymin = 1 - upper, ymax = 1 - lower), alpha = 0.3, color = NA) +
  scale_color_manual(values = management_colors) +
  scale_fill_manual(values = management_colors) +
  labs(
    title = "Recovery by Forest Types × Climate",
    x = "Simulation Year", y = "Proportion recovered",
    color = "Forest Type", fill = "Forest Type"
  ) +
  # Axis formatting
  scale_x_continuous(breaks = seq(0, 266, 50)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1.25)) +
  
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  facet_grid(clim ~ Taxon, switch = "y") +
  geom_text(data = p_value_annotations,
            aes(x = 10, y = 1.20, label = p_value_label),
            inherit.aes = FALSE, hjust = 0, size = 3.5, fontface = "italic") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text.x = element_text(face = "bold", size = 12),
    strip.text.y = element_text(face = "bold", size = 12, angle = 270),
    strip.background = element_rect(fill = "grey90", color = "black"),
    panel.border = element_rect(color = "black", fill = NA)
  )


# plot 3
final_plot <- ggplot(all_plot_data, aes(x = time, y = 1 - surv, color = forest_cat, fill = forest_cat)) +
  geom_step(size = 1.2) +
  geom_ribbon(aes(ymin = 1 - upper, ymax = 1 - lower), alpha = 0.3, color = NA) +
  scale_color_manual(values = management_colors) +
  scale_fill_manual(values = management_colors) +
  labs(
    title = "Recovery by Forest Types × Climate",
    x = "Simulation Year", y = "Proportion recovered",
    color = "Forest Type", fill = "Forest Type"
  ) +
  # Axis formatting
  scale_x_continuous(breaks = seq(0, 266, 50)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1.25)) +
  
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  facet_grid(clim ~ Taxon, switch = "y") +
  geom_text(data = p_value_annotations,
            aes(x = 10, y = 1.20, label = p_value_label),
            inherit.aes = FALSE, hjust = 0, size = 3.5, fontface = "italic") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text.x = element_text(face = "bold", size = 12),
    strip.text.y = element_text(face = "bold", size = 12, angle = 270),
    strip.background = element_rect(fill = "grey90", color = "black"),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid.minor = element_blank(),        # remove minor lines
    axis.title.y = element_text(margin = margin(r = 8)),  # space between y labels and facet strips
    strip.placement = "outside"                # keep strips outside axes
  )



print(final_plot)
# ggsave("AllTaxa_Cox_Recovery.png", final_plot, width = 15, height = 9, dpi = 300)


################################################################################
### TABLE OF YEAR OF RECOVERY FULL SET OF PLOTS

library(openxlsx)

#--------------------------------------------------
# Function to extract recovery time from survival
#--------------------------------------------------

get_recovery_time <- function(time, surv, threshold = 0.95) {
  
  df <- data.frame(
    time = time,
    recovered = 1 - surv
  ) %>%
    distinct(time, .keep_all = TRUE) %>%
    arrange(time)
  
  # Remove time = 0 artifact
  df <- df[df$time > 0, ]
  
  # Enforce monotonicity (important for CI)
  df$recovered <- cummax(df$recovered)
  
  # If threshold never reached
  if (max(df$recovered, na.rm = TRUE) < threshold) {
    return(NA_real_)
  }
  
  # First crossing
  idx <- which(df$recovered >= threshold)[1]
  
  df$time[idx]
}


#--------------------------------------------------
# Summarise recovery times
#--------------------------------------------------

recovery_table <- all_plot_data %>%
  
  group_by(Taxon, clim, forest_cat) %>%
  
  summarise(
    
    Recovery95_mean =
      get_recovery_time(time, surv, 0.95),
    
    Recovery95_slowest =
      get_recovery_time(time, upper, 0.95),
    
    Recovery95_fastest =
      get_recovery_time(time, lower, 0.95),
    
    .groups = "drop"
  )

recovery_table


#--------------------------------------------------
# Export to Excel
#--------------------------------------------------

write.xlsx(
  recovery_table,
  file = file.path(
    "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output - Copy",
    "Recovery_time_by_Taxa_Climate_ForestType.xlsx"
  ),
  sheetName = "Recovery_times",
  overwrite = TRUE
)



###########################
        THE END
###########################


