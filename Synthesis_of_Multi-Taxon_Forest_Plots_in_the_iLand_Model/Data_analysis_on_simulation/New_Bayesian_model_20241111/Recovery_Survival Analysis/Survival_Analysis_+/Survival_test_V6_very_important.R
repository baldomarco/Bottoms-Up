         VERY IMPORTANT!!                # # SURVIVAL TEST V6 #
  


library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(cowplot)

# --- Install and Load showtext/sysfonts if you haven't already ---
 install.packages("showtext")
 install.packages("sysfonts")
library(showtext)
library(sysfonts)

# --- Register Aptos Font ---
tryCatch({
  font_add(family = "Aptos", 
           regular = "C:/Windows/Fonts/aptos.ttf",  # Example path for Aptos Regular
           bold = "C:/Windows/Fonts/aptosb.ttf",    # Example path for Aptos Bold
           italic = "C:/Windows/Fonts/aptosi.ttf",  # Example path for Aptos Italic
           bolditalic = "C:/Windows/Fonts/aptosbi.ttf" # Example path for Aptos Bold Italic
  )
  showtext_auto()
  message("Aptos font loaded and showtext enabled.")
}, error = function(e) {
  warning(paste("Could not load Aptos font. Please ensure it is installed on your system and the file paths in font_add() are correct. Error:", e$message))
  message("Proceeding with default R font.")
})


# --- Configuration ---
Recovery_wide_df <- read.csv("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output - Copy/recovery_analysis_by_taxon_climate_wide_xCompare.csv")
Recovery_wide_df
         
management_colors <- c(
  "Native Broadleaves" = "chocolate",
  "Non-Native Coniferous" = "darkgreen"
)

# Define all 5 taxa (ordered for columns)
all_taxa <- c("BEETLES", "BRYOPHYTES", "LICHENS", "MACROFUNGI", "MOTHS")
climates <- c("CLIM_1961_1990", "CLIM_1991_2020") # Ordered for rows

# --- Initialize data frames to store all plot data and p-value annotations ---
all_plot_data <- data.frame()
p_value_annotations <- data.frame()

# --- Loop through each taxon and climate to generate data for plotting ---
for (current_taxon in all_taxa) {
  taxon_sym <- sym(current_taxon)
  
  for (climate_full in climates) {
    colname <- paste0(current_taxon, "_", climate_full)
    
    # Format climate label for display
    climate_display_label <- gsub("CLIM_", "Climate Series ", climate_full)
    climate_display_label <- gsub("_", " - ", climate_display_label)
    
    if (!(colname %in% colnames(Recovery_wide_df))) {
      warning(paste0("Column '", colname, "' not found. Adding placeholder for ", current_taxon, " in ", climate_display_label))
      p_value_annotations <- bind_rows(p_value_annotations, 
                                       data.frame(Taxon = current_taxon, 
                                                  Climate = climate_display_label, 
                                                  p_value_label = "No data",
                                                  x = 10, y = 1.20))
      next 
    }
    
    temp_df <- Recovery_wide_df %>%
      select(plotID, forest_cat, StandAge, !!taxon_sym := !!sym(colname)) %>%
      mutate(
        recovered = ifelse(is.na(!!taxon_sym), 0, 1),
        time = ifelse(is.na(!!taxon_sym), 266, !!taxon_sym),
        Taxon = current_taxon,
        Climate = climate_display_label
      ) %>% 
      filter(!is.na(forest_cat))
    
    if (n_distinct(temp_df$forest_cat) < 2 || nrow(temp_df) < 5 || all(temp_df$recovered == 0)) {
      warning(paste0("Insufficient or unrecoverable data for survival analysis for ", current_taxon, " in ", climate_full, ". Adding placeholder for p-value."))
      p_value_annotations <- bind_rows(p_value_annotations, 
                                       data.frame(Taxon = current_taxon, 
                                                  Climate = climate_display_label, 
                                                  p_value_label = "Insufficient data",
                                                  x = 10, y = 1.20))
      next
    }
    
    # --- Perform Log-Rank Test for p-value ---
    surv_object <- Surv(time, recovered) ~ forest_cat
    p_value_text <- "Log-rank p = N/A"
    
    fit_check_strata <- tryCatch(surv_fit(surv_object, data = temp_df),
                                 error = function(e) { NULL })
    
    if (!is.null(fit_check_strata) && length(fit_check_strata$strata) > 1 && any(fit_check_strata$n.event > 0)) {
      sdiff <- tryCatch(survdiff(surv_object, data = temp_df), 
                        error = function(e) { 
                          warning(paste0("Error in survdiff for ", current_taxon, " ", climate_full, ": ", e$message))
                          NULL 
                        })
      if (!is.null(sdiff) && !is.na(sdiff$chisq) && sdiff$chisq >= 0) {
        p_value_logrank <- 1 - pchisq(sdiff$chisq, df = length(sdiff$n) - 1)
        p_value_text <- paste0("Log-rank p = ", format.pval(p_value_logrank, digits = 3))
      } else if (!is.null(sdiff) && is.na(sdiff$chisq)) {
        p_value_text <- "Log-rank p = NA (test failed)"
      }
    } else {
      p_value_text <- "Log-rank p = N/A (single group or no events)"
    }
    
    p_value_annotations <- bind_rows(p_value_annotations, 
                                     data.frame(Taxon = current_taxon, 
                                                Climate = climate_display_label, 
                                                p_value_label = p_value_text,
                                                x = 10, y = 1.20))
    
    # --- Fit a Cox model to get predicted curves (adjusted for StandAge) ---
    cox_model <- coxph(Surv(time, recovered) ~ forest_cat * StandAge, data = temp_df)
    
    mean_standage_nb <- mean(filter(temp_df, forest_cat == "Native Broadleaves")$StandAge, na.rm = TRUE)
    mean_standage_nnc <- mean(filter(temp_df, forest_cat == "Non-Native Coniferous")$StandAge, na.rm = TRUE)
    
    if (is.nan(mean_standage_nb) || is.nan(mean_standage_nnc)) {
      warning(paste0("Mean StandAge not calculable for one or more forest categories for ", current_taxon, " in ", climate_full, ". Skipping curve prediction."))
      next
    }
    
    fit_nb <- survfit(cox_model, newdata = data.frame(forest_cat = "Native Broadleaves", StandAge = mean_standage_nb))
    fit_nnc <- survfit(cox_model, newdata = data.frame(forest_cat = "Non-Native Coniferous", StandAge = mean_standage_nnc))
    
    df_nb <- surv_summary(fit_nb) %>% mutate(forest_cat = "Native Broadleaves")
    df_nnc <- surv_summary(fit_nnc) %>% mutate(forest_cat = "Non-Native Coniferous")
    
    current_curves_df <- bind_rows(df_nb, df_nnc) %>%
      mutate(Taxon = current_taxon, Climate = climate_display_label)
    
    all_plot_data <- bind_rows(all_plot_data, current_curves_df)
  }
}

message("\n--- All data prepared. Now generating faceted plot. ---")

# Ensure factors are ordered correctly for faceting
all_plot_data$Taxon <- factor(all_plot_data$Taxon, levels = all_taxa)
ordered_climates <- unique(p_value_annotations$Climate[order(p_value_annotations$Climate)])
all_plot_data$Climate <- factor(all_plot_data$Climate, levels = ordered_climates)

p_value_annotations$Taxon <- factor(p_value_annotations$Taxon, levels = all_taxa)
p_value_annotations$Climate <- factor(p_value_annotations$Climate, levels = ordered_climates)


# Create the combined ggplot using facets
final_plot <- ggplot(all_plot_data, aes(x = time, y = 1 - surv, color = forest_cat, fill = forest_cat)) +
  geom_step(size = 1.2) +
  geom_ribbon(aes(ymin = 1 - upper, ymax = 1 - lower), alpha = 0.3, color = NA) +
  scale_color_manual(values = management_colors) +
  scale_fill_manual(values = management_colors) +
  labs(
    title = "Individual Forest Plots Recovery by Forest Types",
    x = "Simulation Year",
    y = "Proportion recovered",
    color = "Forest Type",
    fill = "Forest Type"
  ) +
  ylim(0, 1.25) +
  
  # Removed switch = "y" to keep strips on the left
  facet_grid(Climate ~ Taxon, 
             labeller = labeller(Climate = label_value, Taxon = label_value)) + 
  
  geom_text(data = p_value_annotations, aes(x = 10, y = 1.20, label = p_value_label),
            inherit.aes = FALSE, 
            hjust = 0, size = 3.5, fontface = "italic", family = "Aptos") +
  
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Aptos"), 
    
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    
    # Strip text appearance (facet labels)
    strip.text.x = element_text(face = "bold", size = 12), # Taxon titles (columns)
    # THIS IS THE KEY CHANGE: angle = 90 for default left vertical strip text
    strip.text.y = element_text(face = "bold", size = 12, angle = -90), # Climate titles (rows) - parallel to y-axis
    strip.placement = "outside", # Keep outside for more plot area
    strip.background = element_rect(fill = "grey90", color = "black"),
    
    # Axis titles and text
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

print(final_plot)
ggsave("Individual_Forest_Plots_Recovery_Aptos_LeftVerticalStrips.png", final_plot, width = 15, height = 9, dpi = 300)

message("\n--- Final plot with Aptos font and vertical left strips generated and saved! ---")