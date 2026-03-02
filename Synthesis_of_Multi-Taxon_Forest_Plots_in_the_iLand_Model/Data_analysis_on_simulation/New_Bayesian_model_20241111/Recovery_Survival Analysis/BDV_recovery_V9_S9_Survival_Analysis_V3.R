               # # SURVIVAL TEST V7 #
  

library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# --- Data ---
Recovery_wide_df <- read.csv("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output - Copy/recovery_analysis_by_taxon_climate_wide_xCompare.csv")
Recovery_wide_df

management_colors <- c(
  "Native Broadleaves" = "chocolate",
  "Non-Native Coniferous" = "darkgreen"
)

all_taxa <- c("BEETLES", "BRYOPHYTES", "LICHENS", "MACROFUNGI", "MOTHS")
climates <- c("CLIM_1961_1990", "CLIM_1991_2020")

all_plot_data <- data.frame()
p_value_annotations <- data.frame()

for (current_taxon in all_taxa) {
  taxon_sym <- sym(current_taxon)
  
  for (climate_full in climates) {
    colname <- paste0(current_taxon, "_", climate_full)
    climate_display <- gsub("CLIM_", "Climate Series ", climate_full)
    climate_display <- gsub("_", " - ", climate_display)
    
    if (!(colname %in% colnames(Recovery_wide_df))) next
    
    temp_df <- Recovery_wide_df %>%
      select(plotID, forest_cat, StandAge, !!taxon_sym := !!sym(colname)) %>%
      mutate(
        recovered = ifelse(is.na(!!taxon_sym), 0, 1),
        time = ifelse(is.na(!!taxon_sym), 266, !!taxon_sym),
        Taxon = current_taxon,
        Climate = climate_display
      ) %>%
      filter(!is.na(forest_cat))
    
    if (n_distinct(temp_df$forest_cat) < 2 || nrow(temp_df) < 5 || all(temp_df$recovered == 0)) next
    
    surv_object <- Surv(time, recovered) ~ forest_cat
    fit_check <- tryCatch(surv_fit(surv_object, data = temp_df), error = function(e) NULL)
    
    if (!is.null(fit_check) && length(fit_check$strata) > 1 && any(fit_check$n.event > 0)) {
      sdiff <- tryCatch(survdiff(surv_object, data = temp_df), error = function(e) NULL)
      if (!is.null(sdiff) && !is.na(sdiff$chisq) && sdiff$chisq >= 0) {
        p_val <- 1 - pchisq(sdiff$chisq, df = length(sdiff$n) - 1)
        p_val_label <- paste0("Log-rank p = ", format.pval(p_val, digits = 3))
        p_value_annotations <- bind_rows(
          p_value_annotations,
          data.frame(Taxon = current_taxon, Climate = climate_display, p_value_label = p_val_label)
        )
      }
    }
    
    cox_model <- coxph(Surv(time, recovered) ~ forest_cat * StandAge, data = temp_df)
    mean_age_nb <- mean(filter(temp_df, forest_cat == "Native Broadleaves")$StandAge, na.rm = TRUE)
    mean_age_nnc <- mean(filter(temp_df, forest_cat == "Non-Native Coniferous")$StandAge, na.rm = TRUE)
    
    fit_nb <- survfit(cox_model, newdata = data.frame(forest_cat = "Native Broadleaves", StandAge = mean_age_nb))
    fit_nnc <- survfit(cox_model, newdata = data.frame(forest_cat = "Non-Native Coniferous", StandAge = mean_age_nnc))
    
    all_plot_data <- bind_rows(
      all_plot_data,
      surv_summary(fit_nb) %>% mutate(forest_cat = "Native Broadleaves", Taxon = current_taxon, Climate = climate_display),
      surv_summary(fit_nnc) %>% mutate(forest_cat = "Non-Native Coniferous", Taxon = current_taxon, Climate = climate_display)
    )
  }
}

# --- Factor ordering ---
all_plot_data$Taxon <- factor(all_plot_data$Taxon, levels = all_taxa)
p_value_annotations$Taxon <- factor(p_value_annotations$Taxon, levels = all_taxa)

# --- Lock recovery curves to be monotonic ---
all_plot_data <- all_plot_data %>%
  group_by(Taxon, Climate, forest_cat) %>%
  arrange(time, .by_group = TRUE) %>%
  mutate(
    surv         = 1 - cummax(1 - surv),   # lock central curve
    lower        = 1 - cummax(1 - lower),  # lock lower bound
    upper        = 1 - cummax(1 - upper)   # lock upper bound
  ) %>%
  ungroup()

# --- Plot ---
final_plot <- ggplot(all_plot_data, aes(x = time, y = 1 - surv, color = forest_cat, fill = forest_cat)) +
  geom_step(size = 1.2) +
  geom_ribbon(aes(ymin = 1 - upper, ymax = 1 - lower), alpha = 0.3, color = NA) +
  scale_color_manual(values = management_colors) +
  scale_fill_manual(values = management_colors) +
  labs(
    title = "Individual Plots Convergence to Old-Growth Forest Biodiversity by Forest Types",
    x = "Simulation Year",
    y = "Proportion of biodiversity restored plots",
    color = "Forest Type", fill = "Forest Type"
  ) +
  ylim(0, 1.25) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", size = 0.5) +
  facet_grid(Climate ~ Taxon, switch = "y") +
  geom_text(data = p_value_annotations, aes(x = 10, y = 1.20, label = p_value_label),
            inherit.aes = FALSE, hjust = 0, size = 3.5, fontface = "italic") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    strip.text.x = element_text(face = "bold", size = 12),
    strip.text.y = element_text(face = "bold", size = 12, angle = 270),
    strip.placement = "outside",
    strip.background = element_rect(fill = "grey90", color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

print(final_plot)

#ggsave("Individual_Forest_Plots_Recovery.png", final_plot, width = 15, height = 9, dpi = 300)
