ALMOST RUBBISH THE GOOD ONE IN THE PREVIOUS         # # SURVIVAL TEST V8 #


library(survival)
library(survminer)
library(dplyr)

# Reshape to include both climates
bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, StandAge,
         BRYOPHYTES_CLIM_1961_1990, BRYOPHYTES_CLIM_1991_2020) %>%
  tidyr::pivot_longer(
    cols = starts_with("BRYOPHYTES_CLIM"),
    names_to = "clim",
    values_to = "year"
  ) %>%
  mutate(
    # clean climate label
    clim = ifelse(grepl("1961_1990", clim), "1961_1990", "1991_2020"),
    recovered = ifelse(is.na(year), 0, 1),
    time = ifelse(is.na(year), 266, year),
    surv_obj = Surv(time, recovered)
  )

# Fit Cox model: forest_cat (groups), StandAge, StandAge², climate
cox_bryo <- coxph(
  surv_obj ~ forest_cat + StandAge + I(StandAge^2) + clim,
  data = bryo_surv
)
summary(cox_bryo)

# Survival curves by forest_cat × climate at mean StandAge
new_data <- expand.grid(
  forest_cat = levels(factor(bryo_surv$forest_cat)),
  clim = unique(bryo_surv$clim),
  StandAge = mean(bryo_surv$StandAge, na.rm = TRUE)
)

fit_bryo <- survfit(cox_bryo, newdata = new_data)

# Plot survival (not yet recovered)
ggsurvplot(
  fit_bryo,
  data = bryo_surv,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Simulation Year",
  ylab = "Proportion not yet recovered",
  title = "BRYOPHYTES Recovery by Forest Type and Climate",
  legend.title = "Forest × Climate",
  ggtheme = theme_minimal()
)


____________________________________________________



# --- Forest Type (Cox Model with climate + quadratic StandAge) Section ---
plots_forest_type <- list() # Re-initialize for each taxon

for (current_taxon in all_taxa) {
  taxon <- current_taxon
  message(paste0("\n--- Processing Cox Model for Taxon: ", taxon, " ---"))
  
  # Collect the two climate columns for this taxon
  taxon_cols <- grep(paste0("^", taxon, "_CLIM"), colnames(Recovery_wide_df), value = TRUE)
  if (length(taxon_cols) == 0) {
    warning(paste0("No climate columns found for taxon '", taxon, "'. Skipping Forest Type Cox model."))
    next
  }
  
  # Reshape wide → long (taxon across both climates)
  temp_df <- Recovery_wide_df %>%
    select(plotID, forest_cat, StandAge, all_of(taxon_cols)) %>%
    tidyr::pivot_longer(
      cols = all_of(taxon_cols),
      names_to = "clim",
      values_to = "year"
    ) %>%
    mutate(
      clim = dplyr::case_when(
        grepl("1961_1990", clim) ~ "1961_1990",
        grepl("1991_2020", clim) ~ "1991_2020",
        TRUE ~ clim
      ),
      recovered = ifelse(is.na(year), 0, 1),
      time = ifelse(is.na(year), 266, year),
      surv_obj = Surv(time, recovered)
    ) %>%
    filter(!is.na(forest_cat)) # remove NAs in forest_cat
  
  # Check minimum data requirements
  if (nrow(temp_df) < 5 || n_distinct(temp_df$forest_cat) < 2 || all(temp_df$recovered == 0)) {
    warning(paste0("Insufficient data for Cox model for ", taxon, ". Skipping."))
    next
  }
  
  # --- Fit Cox model with StandAge² and climate ---
  cox_model <- coxph(
    surv_obj ~ forest_cat + StandAge + I(StandAge^2) + clim,
    data = temp_df
  )
  message(paste0("Cox model fit for ", taxon, ":"))
  print(summary(cox_model))
  
  # --- Prepare new data for prediction ---
  mean_age <- mean(temp_df$StandAge, na.rm = TRUE)
  new_data <- expand.grid(
    forest_cat = levels(factor(temp_df$forest_cat)),
    clim = unique(temp_df$clim),
    StandAge = mean_age
  )
  
  fit <- survfit(cox_model, newdata = new_data)
  
  # --- Plot predicted survival curves ---
  plot_title <- paste0(taxon, " Recovery by Forest Type × Climate (at mean StandAge)")
  plots_forest_type[[taxon]] <- ggsurvplot(
    fit,
    data = temp_df,
    fun = "event", # proportion recovered
    pval = TRUE,
    conf.int = TRUE,
    xlab = "Simulation Year",
    ylab = "Proportion recovered",
    title = plot_title,
    legend.title = "Forest × Climate",
    ggtheme = theme_minimal()
  )
}

# --- Print all Cox model plots ---
for (taxon in names(plots_forest_type)) {
  print(plots_forest_type[[taxon]]$plot + theme(legend.position = "bottom"))
}

































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

