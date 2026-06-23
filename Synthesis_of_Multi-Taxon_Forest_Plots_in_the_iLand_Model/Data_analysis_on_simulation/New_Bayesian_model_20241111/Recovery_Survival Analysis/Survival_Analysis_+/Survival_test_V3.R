                                 # SURVIVAL TEST V3 #

library(tidyverse)
library(survival)
library(survminer)
library(cowplot)

Recovery_wide_df <- read.csv("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output - Copy/recovery_analysis_by_taxon_climate_wide_xCompare.csv")
Recovery_wide_df

# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
#pdf(paste0(dataroot, "Survival Analysis Proportion of Recovered by Stand Age Calsses.pdf"), height=9, width=16)


# Define the taxa and climate periods
taxa <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")
climate_periods <- c("1961_1990", "1991_2020")

# Reshape the data to a long format for easier iteration
long_df <- Recovery_wide_df %>%
  pivot_longer(
    cols = ends_with(c("_1961_1990", "_1991_2020")), # Select columns ending with these patterns
    names_to = c("Taxon", "ClimatePeriod"),         # Create new columns for taxon and climate period
    names_pattern = "(.*)_CLIM_(.*)",              # Extract parts using regex: (Taxon)_CLIM_(Period)
    values_to = "Value"                            # The actual climate value goes here
  ) %>%
  # Ensure ClimatePeriod is a factor with a specific order for consistent plotting
  mutate(ClimatePeriod = factor(ClimatePeriod, levels = climate_periods))

# --- 2. Define a function to generate survival plots and summary statistics for each combination ---

#' Generates a Kaplan-Meier survival plot and calculates summary statistics
#' for a specific taxon and climate period.
#'
#' @param data A data frame in long format, containing 'StandAge', 'Value',
#'   'Taxon', and 'ClimatePeriod'.
#' @param current_taxon The name of the taxon to analyze (e.g., "BRYOPHYTES").
#' @param current_climate_period The climate period to analyze (e.g., "1961_1990").
#' @return A list containing the ggsurvplot object and a data frame of summary statistics,
#'   or NULL if not enough data.
generate_survival_plot <- function(data, current_taxon, current_climate_period) {
  # Filter data for the specific taxon and climate period
  filtered_data <- data %>%
    filter(Taxon == current_taxon, ClimatePeriod == current_climate_period) %>%
    # Prepare 'recovered' and 'time' variables for survival analysis
    # 'recovered' is 1 if a value is present, 0 if NA (not yet recovered/observed)
    # 'time' is the observed value, or 266 if not recovered (assuming 266 is max observation time)
    mutate(
      recovered = ifelse(is.na(Value), 0, 1),
      time = ifelse(is.na(Value), 266, Value)
    )
  
  # Apply theoretical AgeClass definitions
  filtered_data <- filtered_data %>%
    mutate(
      AgeClass = case_when(
        StandAge <= 50 ~ "Young",
        StandAge > 50 & StandAge <= 100 ~ "Middle",
        StandAge > 100 ~ "Old"
      ),
      AgeClass = factor(AgeClass, levels = c("Young", "Middle", "Old")) # Ensure factor levels for consistent order
    )
  
  # Check if there is enough data for survival analysis
  # At least one event (recovered = 1) is needed to fit a meaningful survival curve
  if (nrow(filtered_data) == 0 || sum(filtered_data$recovered) == 0) {
    message(paste("Skipping plot for", current_taxon, current_climate_period,
                  ": Not enough data or no recovery events for survival analysis."))
    return(list(plot = NULL, summary = NULL))
  }
  
  # Calculate mean/median recovery time per AgeClass
  summary_stats <- filtered_data %>%
    filter(recovered == 1) %>% # Only consider actual recovery events for mean/median time
    group_by(AgeClass) %>%
    summarise(
      Mean_Recovery_Time = mean(time, na.rm = TRUE),
      Median_Recovery_Time = median(time, na.rm = TRUE),
      N_Recovered = n()
    ) %>%
    ungroup() %>%
    mutate(
      Taxon = current_taxon,
      ClimatePeriod = current_climate_period
    ) %>%
    select(Taxon, ClimatePeriod, AgeClass, Mean_Recovery_Time, Median_Recovery_Time, N_Recovered)
  
  # Fit Kaplan-Meier survival model
  fit <- survfit(Surv(time, recovered) ~ AgeClass, data = filtered_data)
  
  # Generate survival plot using ggsurvplot
  plot_title <- paste0(current_taxon, " Recovery (", current_climate_period, ")")
  p <- ggsurvplot(
    fit,
    data = filtered_data,
    fun = "event",       # This plots proportion recovered
    pval = TRUE,         # Display p-value from log-rank test
    conf.int = TRUE,     # Show confidence intervals
    risk.table = TRUE,   # Display risk table below the plot
    xlab = "Time (Years)",
    ylab = "Proportion recovered", # Updated label for 'fun = "event"'
    title = plot_title,
    ggtheme = theme_minimal(), # Use a minimal theme
    legend.title = "Age Class" # Set legend title
  )
  
  # ggsurvplot returns a list; the actual ggplot object is in the 'plot' element
  return(list(plot = p$plot, summary = summary_stats))
}

# --- 3. Generate plots and collect summary statistics for all combinations of taxa and climate periods ---

all_plots_list <- list()
all_summaries_list <- list()

for (taxon in taxa) {
  taxon_plots <- list()
  for (period in climate_periods) {
    # Generate the plot and get summary for the current taxon and climate period
    result <- generate_survival_plot(long_df, taxon, period)
    if (!is.null(result$plot)) {
      taxon_plots[[period]] <- result$plot
    } else {
      taxon_plots[[period]] <- NULL # Store NULL if plot could not be generated
    }
    if (!is.null(result$summary)) {
      all_summaries_list[[paste0(taxon, "_", period)]] <- result$summary
    }
  }
  all_plots_list[[taxon]] <- taxon_plots
}

# --- 4. Arrange and display plots for each taxon in a 1x2 panel ---

# This loop will print each combined plot to your R graphics device
for (taxon_name in names(all_plots_list)) {
  plot_1961_1990 <- all_plots_list[[taxon_name]][["1961_1990"]]
  plot_1991_2020 <- all_plots_list[[taxon_name]][["1991_2020"]]
  
  # Check if both plots exist for the current taxon
  if (!is.null(plot_1961_1990) && !is.null(plot_1991_2020)) {
    # Combine the two plots side-by-side
    combined_plot <- plot_grid(
      plot_1961_1990,
      plot_1991_2020,
      ncol = 2,
      align = "h", # Align horizontally
      axis = "tblr", # Align axes
      rel_widths = c(1, 1) # Equal width for both plots
    )
    
    # Add a main title for the combined plot (optional, as individual plots have titles)
    # title <- ggdraw() + draw_label(paste0(taxon_name, " Recovery by Climate Period"), fontface = 'bold')
    # print(plot_grid(title, combined_plot, ncol = 1, rel_heights = c(0.1, 1)))
    print(combined_plot) # Print the combined plot
  } else if (!is.null(plot_1961_1990)) {
    # If only the first period plot exists, print it
    print(plot_1961_1990)
  } else if (!is.null(plot_1991_2020)) {
    # If only the second period plot exists, print it
    print(plot_1991_2020)
  } else {
    message(paste("No plots were generated for taxon:", taxon_name, "due to insufficient data."))
  }
}

# --- 5. Combine and display all summary statistics ---
final_summary_table <- bind_rows(all_summaries_list)

# The 'final_summary_table' now contains all mean/median recovery times
# You can print it to the console or save it to a file
print(final_summary_table)
