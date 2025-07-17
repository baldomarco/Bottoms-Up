library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(ggpubr)

# --- Configuration ---
# Make sure this path is correct for your system
Recovery_wide_df <- read.csv("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/recovery_analysis_by_taxon_climate_wide_xCompare.csv")

management_colors <- c(
  "Native Broadleaves" = "chocolate",
  "Non-Native Coniferous" = "darkgreen"
)

# Define all 5 taxa
all_taxa <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")
climates <- c("CLIM_1961_1990", "CLIM_1991_2020")

# --- Loop through each taxon ---
for (current_taxon in all_taxa) {
  taxon <- current_taxon # Assign the current taxon to the 'taxon' variable for existing code to use
  
  message(paste0("\n--- Processing Taxon: ", taxon, " ---"))
  
  # --- AgeClass Plots Section ---
  plots_ageclass <- list() # Re-initialize for each taxon
  
  for (climate in climates) {
    colname <- paste0(taxon, "_", climate)
    
    # Check if the column exists in the dataframe
    if (!(colname %in% colnames(Recovery_wide_df))) {
      warning(paste0("Column '", colname, "' not found for taxon '", taxon, "' in climate '", climate, "'. Skipping AgeClass plot."))
      next # Skip to the next iteration if column doesn't exist
    }
    
    temp_df <- Recovery_wide_df %>%
      select(plotID, forest_cat, StandAge, !!sym(colname)) %>%
      mutate(
        recovered = ifelse(is.na(!!sym(colname)), 0, 1),
        time = ifelse(is.na(!!sym(colname)), 266, !!sym(colname)),
        AgeClass = case_when(
          StandAge <= 50 ~ "Young",
          StandAge > 50 & StandAge <= 100 ~ "Middle",
          StandAge > 100 ~ "Old",
          TRUE ~ NA_character_
        ),
        AgeClass = factor(AgeClass, levels = c("Young", "Middle", "Old"))
      ) %>% filter(!is.na(AgeClass)) # Filter out NA AgeClass entries
    
    # Skip if temp_df has no valid data after filtering
    if(nrow(temp_df) == 0 || all(is.na(temp_df$time))) {
      warning(paste0("No valid data for AgeClass analysis for ", taxon, " in ", climate, ". Skipping plot."))
      next
    }
    
    fit_ageclass <- survfit(Surv(time, recovered) ~ AgeClass, data = temp_df)
    summary(fit_ageclass)
    
    plots_ageclass[[climate]] <- ggsurvplot(
      fit_ageclass,
      data = temp_df,
      fun = "event", # To evaluate the proportion of recovered
      pval = TRUE, # log-rank test H0 (null hypothesis): All groups have the same survival (or recovery) function H1 (alternative): At least one group differs from the others
      pval.coord = c(10, 0.98), # Coordinates for p-value for AgeClass plots
      conf.int = TRUE,
      risk.table = TRUE,
      risk.table.title = "Number at risk",
      xlab = "Simulation Year",
      ylab = "Proportion recovered",
      title = paste0(taxon, " Recovery by AgeClass - Climate ", gsub("CLIM_", "", climate)),
      # palette = c("deepskyblue", "orange", "forestgreen"), # Keep commented as in your original code
      ggtheme = theme_minimal()
    )
  }
  
  # Combine AgeClass plots for the current taxon if both climate plots were generated
  if (length(plots_ageclass) == 2) {
    combined_ageclass_plot <- ggarrange(
      plots_ageclass[[1]]$plot + theme(legend.position = "bottom"),
      plots_ageclass[[2]]$plot + theme(legend.position = "bottom"),
      ncol = 2,
      common.legend = TRUE,
      legend = "bottom"
    )
    print(combined_ageclass_plot)
    # Optional: Save the combined plot to a file
    # ggsave(paste0(taxon, "_AgeClass_Recovery_Combined.png"), combined_ageclass_plot, width = 12, height = 7)
  } else if (length(plots_ageclass) == 1) {
    message(paste0("Only one AgeClass plot generated for ", taxon, ". Printing it directly."))
    print(plots_ageclass[[1]]$plot + theme(legend.position = "bottom"))
  } else {
    message(paste0("No AgeClass plots generated for ", taxon, "."))
  }
  
  # --- Forest Type (Cox Model) Plots Section ---
  plots_forest_type <- list() # Re-initialize for each taxon
  
  for (climate in climates) {
    colname <- paste0(taxon, "_", climate)
    
    # Check if the column exists in the dataframe
    if (!(colname %in% colnames(Recovery_wide_df))) {
      warning(paste0("Column '", colname, "' not found for taxon '", taxon, "' in climate '", climate, "'. Skipping Forest Type plot."))
      next # Skip to the next iteration if column doesn't exist
    }
    
    # Prepare data for the current climate
    bryo_surv_climate <- Recovery_wide_df %>%
      select(plotID, forest_cat, StandAge, !!sym(colname)) %>%
      mutate(
        recovered = ifelse(is.na(!!sym(colname)), 0, 1),
        time = ifelse(is.na(!!sym(colname)), 266, !!sym(colname))
      )
    
    # Filter out rows where forest_cat is NA, which might happen if you have incomplete data
    bryo_surv_climate <- bryo_surv_climate %>% filter(!is.na(forest_cat))
    
    # Ensure there are at least two forest categories for comparison
    if (n_distinct(bryo_surv_climate$forest_cat) < 2) {
      warning(paste0("Not enough distinct 'forest_cat' levels for Cox model for ", taxon, " in ", climate, ". Skipping plot."))
      next
    }
    
    # Ensure there's enough data for the Cox model
    if(nrow(bryo_surv_climate) < 5 || all(is.na(bryo_surv_climate$time))) { # Arbitrary minimum, adjust as needed
      warning(paste0("Insufficient data for Cox model for ", taxon, " in ", climate, ". Skipping plot."))
      next
    }
    
    # Fit a single Cox model including both forest_cat and StandAge
    cox_model <- coxph(Surv(time, recovered) ~ forest_cat * StandAge, data = bryo_surv_climate)
    
    message(paste0("\n--- Cox Model Summary for ", taxon, " - Climate ", gsub("CLIM_", "", climate), " ---"))
    cox_summary <- summary(cox_model)
    print(cox_summary)
    
    # Extract p-value for the forest_cat variable
    p_value_forest_cat <- NA_real_ # Initialize to NA
    p_value_text <- ""
    
    # Check if the coefficient for the factor level exists
    if ("forest_catNon-Native Coniferous" %in% rownames(cox_summary$coefficients)) {
      p_value_forest_cat <- cox_summary$loglik
      p_value_text <- paste0("p = ", format.pval(p_value_forest_cat, digits = 3))
    } else {
      message("Could not find 'forest_catNon-Native Coniferous' coefficient. Check model or data for this taxon/climate.")
      p_value_text <- "p = N/A"
    }
    
    # Calculate mean StandAge for each forest category within the current climate's data
    # Filter for non-NA StandAge before calculating mean
    mean_standage_nb <- mean(filter(bryo_surv_climate, forest_cat == "Native Broadleaves")$StandAge, na.rm = TRUE)
    mean_standage_nnc <- mean(filter(bryo_surv_climate, forest_cat == "Non-Native Coniferous")$StandAge, na.rm = TRUE)
    
    # Check if means are valid (not NaN, which can happen if a category is empty)
    if (is.nan(mean_standage_nb) || is.nan(mean_standage_nnc)) {
      warning(paste0("Mean StandAge not calculable for one or more forest categories for ", taxon, " in ", climate, ". Skipping plot."))
      next
    }
    
    # Predict recovery curves for each forest type at their respective mean StandAge
    fit_nb <- survfit(cox_model, newdata = data.frame(forest_cat = "Native Broadleaves", StandAge = mean_standage_nb))
    fit_nnc <- survfit(cox_model, newdata = data.frame(forest_cat = "Non-Native Coniferous", StandAge = mean_standage_nnc))
    
    # Convert to data frames for plotting
    df_nb <- surv_summary(fit_nb) %>% mutate(forest_cat = "Native Broadleaves")
    df_nnc <- surv_summary(fit_nnc) %>% mutate(forest_cat = "Non-Native Coniferous")
    
    fit_df <- bind_rows(df_nb, df_nnc)
    
    # Create the ggplot for the current climate
    plots_forest_type[[climate]] <- ggplot(fit_df, aes(x = time, y = 1 - surv, color = forest_cat, fill = forest_cat)) +
      geom_step(size = 1.2) +
      geom_ribbon(aes(ymin = 1 - upper, ymax = 1 - lower), alpha = 0.3, color = NA) +
      scale_color_manual(values = management_colors) +
      scale_fill_manual(values = management_colors) +
      labs(
        title = paste0(taxon, " Recovery by Forest Type - Climate ", gsub("CLIM_", "", climate)),
        subtitle = "Predicted at mean StandAge for each forest type",
        x = "Simulation Year",
        y = "Proportion recovered",
        color = "Forest Type",
        fill = "Forest Type"
      ) +
      # Add the p-value text to the plot (corrected variable name)
      annotate("text", x = 10, y = 1.1, label = p_value_text, hjust = 0, size = 5) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") # Ensure legend is at the bottom for consistent arrangement
  }
  
  # Combine Forest Type plots for the current taxon if both climate plots were generated
  if (length(plots_forest_type) == 2) {
    combined_forest_type_plot <- ggarrange(
      plots_forest_type[[1]],
      plots_forest_type[[2]],
      ncol = 2,
      common.legend = TRUE,
      legend = "bottom"
    )
    print(combined_forest_type_plot)
    # Optional: Save the combined plot to a file
    # ggsave(paste0(taxon, "_ForestType_Recovery_Combined.png"), combined_forest_type_plot, width = 12, height = 7)
  } else if (length(plots_forest_type) == 1) {
    message(paste0("Only one Forest Type plot generated for ", taxon, ". Printing it directly."))
    print(plots_forest_type[[1]])
  } else {
    message(paste0("No Forest Type plots generated for ", taxon, "."))
  }
}

message("\n--- All taxa processed! ---")

