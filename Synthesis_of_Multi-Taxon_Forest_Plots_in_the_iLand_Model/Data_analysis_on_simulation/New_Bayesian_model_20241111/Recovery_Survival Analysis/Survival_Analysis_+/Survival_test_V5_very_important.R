            VERY IMPORTANT!!                # # SURVIVAL TEST V5 #

library(showtext)
library(sysfonts) # Both are needed to make a different font used in the graphics


# Check if Aptos is found by sysfonts
font_families_google() # Aptos is NOT a Google Font, so this won't work
font_families() # This will list fonts sysfonts can detect on your system

# If 'Aptos' or 'Aptos Serif' is in font_families(), you can use it directly:
# font_add(family = "Aptos", regular = "path/to/Aptos-Regular.ttf", bold = "path/to/Aptos-Bold.ttf") # If you need different weights
# Usually, if it's already in your system, `showtext` can find it by name.

# Example for Windows:
# You'll need to find the exact paths. Typically in C:\Windows\Fonts
# e.g., "C:/Windows/Fonts/Aptos.ttf" or "C:/Windows/Fonts/Aptos-Regular.ttf"

# You should identify the full font name from your system.
# It's usually "Aptos" for regular, "Aptos Bold" for bold etc.
# Find the actual file names for 'Regular', 'Bold', 'Italic' versions of Aptos.
# For demonstration, let's assume we add a "regular" and "bold" variant.
# NOTE: You must replace "path/to/Aptos-Regular.ttf" with the actual path on YOUR system.

# First, try to see if it's already recognized:
# If you know the exact font name as seen by R (e.g., from font_families() or a font viewer)
# you can sometimes just set theme(text = element_text(family = "Aptos"))
# But to be safe and use specific weights:

# Example: if you have Aptos-Regular.ttf and Aptos-Bold.ttf
# If you have only one .ttf file for the main font, you can just use `regular`.
font_add(family = "Aptos", 
         regular = "C:/Windows/Fonts/aptos.ttf", # Adjust this path for your system
         bold = "C:/Windows/Fonts/aptosb.ttf",   # Adjust this path for your system, if a bold file exists
         italic = "C:/Windows/Fonts/aptosi.ttf", # Adjust for italic
         bolditalic = "C:/Windows/Fonts/aptosbi.ttf" # Adjust for bold italic
)
# If you only have one file, for example, for the regular weight:
# font_add(family = "Aptos", regular = "C:/Windows/Fonts/aptos.ttf") 

# If you're on a Mac/Linux, paths would look like "/Library/Fonts/Aptos.ttf" or "~/.fonts/Aptos.ttf"


showtext_auto()


library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(ggpubr) # For text_grob (not directly used for main plot, but good to have)
library(cowplot) # For ggdraw() if you want to use it for the main title wrapper

# --- Install and Load showtext/sysfonts if you haven't already ---
# install.packages("showtext")
# install.packages("sysfonts")
library(showtext)
library(sysfonts)

# --- Register Aptos Font ---
# IMPORTANT: You need to have the Aptos font files on your system.
# The paths below are examples for Windows. Adjust them to your actual font file locations.
# If you only have the regular Aptos.ttf, use only the 'regular' argument.
# You can usually find the font files in C:\Windows\Fonts (Windows) or /Library/Fonts (macOS).
# The exact filenames might vary (e.g., "aptos.ttf", "Aptos-Regular.ttf", "aptosb.ttf" for bold).

# List system fonts to find the exact name (optional, for debugging)
# font_families() 
# You might see names like "Aptos", "Aptos Bold", "Aptos Italic" etc.
# If so, you might not even need font_add if showtext_auto() picks them up.
# But for explicit control, it's better to add them.

tryCatch({
  # This is a common way to load system fonts by name. showtext often finds them if installed.
  # This tries to load "Aptos" as a font family. If Aptos is installed on your system,
  # showtext should be able to pick up its regular, bold, italic variants automatically
  # if they are part of the "Aptos" font family.
  
  # A more explicit way if you have the TTF files and want to define specific weights:
  # Replace these paths with the actual paths to your Aptos font files.
  font_add(family = "Aptos", 
           regular = "C:/Windows/Fonts/aptos.ttf",  # Example path for Aptos Regular
           bold = "C:/Windows/Fonts/aptosb.ttf",    # Example path for Aptos Bold
           italic = "C:/Windows/Fonts/aptosi.ttf",  # Example path for Aptos Italic
           bolditalic = "C:/Windows/Fonts/aptosbi.ttf" # Example path for Aptos Bold Italic
  )
  
  # If you only have one .ttf file for the main font, e.g., "aptos.ttf":
  # font_add(family = "Aptos", regular = "C:/Windows/Fonts/aptos.ttf")
  
  showtext_auto() # Enable showtext for all subsequent plots
  message("Aptos font loaded and showtext enabled.")
}, error = function(e) {
  warning(paste("Could not load Aptos font. Please ensure it is installed on your system and the file paths in font_add() are correct. Error:", e$message))
  message("Proceeding with default R font.")
  # If Aptos can't be loaded, the plot will use the default font.
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
  taxon_sym <- sym(current_taxon) # Convert string to symbol for tidy evaluation
  
  for (climate_full in climates) {
    colname <- paste0(current_taxon, "_", climate_full)
    
    # Format climate label for display
    climate_display_label <- gsub("CLIM_", "Climate Series ", climate_full)
    climate_display_label <- gsub("_", " - ", climate_display_label) # Add spaces around hyphen
    
    # Check if the column exists in the dataframe
    if (!(colname %in% colnames(Recovery_wide_df))) {
      warning(paste0("Column '", colname, "' not found. Adding placeholder for ", current_taxon, " in ", climate_display_label))
      
      # Add placeholder data for p-value annotations to keep the grid structure
      p_value_annotations <- bind_rows(p_value_annotations, 
                                       data.frame(Taxon = current_taxon, 
                                                  Climate = climate_display_label, 
                                                  p_value_label = "No data",
                                                  x = 10, y = 1.20)) # Ensure consistent y for p-value
      next # Skip to the next climate iteration
    }
    
    # Prepare data for the current taxon and climate
    temp_df <- Recovery_wide_df %>%
      select(plotID, forest_cat, StandAge, !!taxon_sym := !!sym(colname)) %>% # Rename the dynamic column
      mutate(
        recovered = ifelse(is.na(!!taxon_sym), 0, 1),
        time = ifelse(is.na(!!taxon_sym), 266, !!taxon_sym),
        Taxon = current_taxon,
        Climate = climate_display_label # Use formatted climate label
      ) %>% 
      filter(!is.na(forest_cat)) # Remove rows where forest_cat is NA
    
    # --- Check for sufficient data for survival analysis ---
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
    p_value_text <- "Log-rank p = N/A" # Default if test fails or not applicable
    
    fit_check_strata <- tryCatch(surv_fit(surv_object, data = temp_df),
                                 error = function(e) { NULL })
    
    if (!is.null(fit_check_strata) && length(fit_check_strata$strata) > 1 && any(fit_check_strata$n.event > 0)) {
      sdiff <- tryCatch(survdiff(surv_object, data = temp_df), 
                        error = function(e) { 
                          warning(paste0("Error in survdiff for ", current_taxon, " ", climate_full, ": ", e$message))
                          NULL 
                        })
      if (!is.null(sdiff) && !is.na(sdiff$chisq) && sdiff$chisq >= 0) { # chisq can be 0, leading to p=1
        p_value_logrank <- 1 - pchisq(sdiff$chisq, df = length(sdiff$n) - 1)
        p_value_text <- paste0("Log-rank p = ", format.pval(p_value_logrank, digits = 3))
      } else if (!is.null(sdiff) && is.na(sdiff$chisq)) {
        p_value_text <- "Log-rank p = NA (test failed)"
      }
    } else {
      p_value_text <- "Log-rank p = N/A (single group or no events)"
    }
    
    # Store p-value for annotation
    p_value_annotations <- bind_rows(p_value_annotations, 
                                     data.frame(Taxon = current_taxon, 
                                                Climate = climate_display_label, 
                                                p_value_label = p_value_text,
                                                x = 10, y = 1.20)) # Consistent y for p-value
    
    # --- Fit a Cox model to get predicted curves (adjusted for StandAge) ---
    cox_model <- coxph(Surv(time, recovered) ~ forest_cat * StandAge, data = temp_df)
    
    mean_standage_nb <- mean(filter(temp_df, forest_cat == "Native Broadleaves")$StandAge, na.rm = TRUE)
    mean_standage_nnc <- mean(filter(temp_df, forest_cat == "Non-Native Coniferous")$StandAge, na.rm = TRUE)
    
    if (is.nan(mean_standage_nb) || is.nan(mean_standage_nnc)) {
      warning(paste0("Mean StandAge not calculable for one or more forest categories for ", current_taxon, " in ", climate_full, ". Skipping curve prediction."))
      next # Skip adding curves for this combination
    }
    
    # Predict recovery curves for each forest type at their respective mean StandAge
    fit_nb <- survfit(cox_model, newdata = data.frame(forest_cat = "Native Broadleaves", StandAge = mean_standage_nb))
    fit_nnc <- survfit(cox_model, newdata = data.frame(forest_cat = "Non-Native Coniferous", StandAge = mean_standage_nnc))
    
    # Convert to data frames for plotting
    df_nb <- surv_summary(fit_nb) %>% mutate(forest_cat = "Native Broadleaves")
    df_nnc <- surv_summary(fit_nnc) %>% mutate(forest_cat = "Non-Native Coniferous")
    
    # Combine curves for the current taxon/climate
    current_curves_df <- bind_rows(df_nb, df_nnc) %>%
      mutate(Taxon = current_taxon, Climate = climate_display_label) # Use formatted climate label
    
    # Append to the main data frame for faceting
    all_plot_data <- bind_rows(all_plot_data, current_curves_df)
  }
}

message("\n--- All data prepared. Now generating faceted plot. ---")

# Ensure factors are ordered correctly for faceting
all_plot_data$Taxon <- factor(all_plot_data$Taxon, levels = all_taxa)
# Sort climates by year for proper display order
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
    title = "Individual Forest Plots Recovery by Forest Types", # Main title
    x = "Simulation Year",
    y = "Proportion recovered",
    color = "Forest Type", # Legend title for color
    fill = "Forest Type" # Legend title for fill
  ) +
  ylim(0, 1.25) + # Consistent y-axis for all panels, slightly higher for p-value
  
  # Facet grid: rows = Climate (on right), columns = Taxon
  facet_grid(Climate ~ Taxon, 
             switch = "y", # Put Climate strips on the right
             labeller = labeller(Climate = label_value, Taxon = label_value)) +
  
  # Add p-value annotations for each panel
  geom_text(data = p_value_annotations, aes(x = 10, y = 1.20, label = p_value_label), # x=10 as starting point, adjust as needed
            inherit.aes = FALSE, 
            hjust = 0, size = 3.5, fontface = "italic", family = "Aptos") + # Apply Aptos here
  
  theme_minimal(base_size = 12) +
  theme(
    # Apply Aptos to all text elements in the plot
    text = element_text(family = "Aptos"), 
    
    legend.position = "bottom", # Common legend at the bottom
    legend.title = element_text(face = "bold", size = 12), # Make legend title bold
    
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)), # Main title styling
    
    # Strip text appearance (facet labels)
    strip.text.x = element_text(face = "bold", size = 12), # Taxon titles (columns)
    strip.text.y = element_text(face = "bold", size = 12, angle = 270), # Climate titles (rows) - rotate for vertical
    strip.placement = "outside", # Place strips outside axes, giving more room for plots
    strip.background = element_rect(fill = "grey90", color = "black"), # Background for strip titles
    
    # Axis titles and text
    axis.title.x = element_text(size = 12, margin = margin(t = 10)), # Space above x-axis title
    axis.title.y = element_text(size = 12, margin = margin(r = 10)), # Space to the right of y-axis title
    axis.text.x = element_text(angle = 45, hjust = 1), # Angle x-axis labels
    
    panel.spacing = unit(0.5, "lines"), # Space between panels
    panel.border = element_rect(color = "black", fill = NA, size = 0.5) # Add a border around each panel
  )

print(final_plot)
ggsave("Individual_Forest_Plots_Recovery_Aptos.png", final_plot, width = 15, height = 9, dpi = 300)

message("\n--- Final plot with Aptos font generated and saved! ---")