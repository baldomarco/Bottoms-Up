################          MARCO BALDO - 2025.07.04        ######################
################         YEAR OF RECOVERY TAXA BDV        ######################
################      MERGED TABLE FOR SURVIVAL ANALYSIS  ######################
################          ARTICLE BALDO ET AL. 2025       ######################
################################################################################

library(tidyr)
library(dplyr)
library(stringr)
library(openxlsx)

# CLIMATE SERIES 1961 - 1990
BDV_recovery_filtered_266_1961_1990 <- readRDS("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/old_for_rec_analysis_and_table/BDV_recovery_filtered_266_1961_1990.rds")

# CLIMATE SERIES 1991 - 2020
BDV_recovery_filtered_266_1991_2020 <- readRDS("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/old_for_rec_analysis_and_table/BDV_recovery_filtered_266_1991_2020.rds")

# Set your working directory to the folder where the CSV files are
setwd("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/")

# Now, read the CSV files directly by their names
recovery_long_1961_1990 <- read.csv("recovery_long_1961_1990_266Y.csv")
recovery_long_1991_2020 <- read.csv("recovery_long_1991_2020_266Y.csv")

# You can check the first few rows of each dataframe to ensure they loaded correctly
head(recovery_long_1961_1990)
head(recovery_long_1991_2020)


# 1. Add a 'Climate' column to each dataframe
recovery_long_1961_1990_clim <- recovery_long_1961_1990 %>%
  mutate(Climate = "CLIM_1961_1990")

recovery_long_1991_2020_clim <- recovery_long_1991_2020 %>%
  mutate(Climate = "CLIM_1991_2020")

# 2. Combine the two dataframes
# Use bind_rows to stack them, ensuring all columns are aligned.
# If column names differ slightly (e.g., 'age' vs 'StandAge'), you'll need to unify them
# or ensure `bind_rows` handles it (it usually fills with NA if a column is missing in one df).
# Based on your image and head() output, it seems 'age' is the column name in your data,
# but your image shows 'StandAge'. Let's assume 'StandAge' is the desired final name.
# We'll rename 'age' to 'StandAge' in both dataframes before binding if 'age' is present.

# Check column names and rename 'age' to 'StandAge' if necessary
if ("age" %in% names(recovery_long_1961_1990_clim)) {
  recovery_long_1961_1990_clim <- recovery_long_1961_1990_clim %>%
    rename(StandAge = age)
}
if ("age" %in% names(recovery_long_1991_2020_clim)) {
  recovery_long_1991_2020_clim <- recovery_long_1991_2020_clim %>%
    rename(StandAge = age)
}

# Now bind rows
recovery_long_combined <- bind_rows(recovery_long_1961_1990_clim, recovery_long_1991_2020_clim)

# 3. Filter for Medians (for non-MOTHS) and Q3 for Moths, and extract taxa/quartile
recovery_long_filtered <- recovery_long_combined %>%
  mutate(
    # Extract quartile (e.g., Q1, Median, Q3)
    # This regex correctly handles 'Per_above_' prefix by only looking for Q1|Median|Q3
    quartile = str_extract(taxa_threshold, "Q1|Median|Q3"),
    # Extract taxa (e.g., BRYOPHYTES, MOTHS)
    # This uses a positive lookbehind to get the text after Q1_, Median_, or Q3_
    taxa = str_extract(taxa_threshold, "(?<=Q1_|Median_|Q3_)[A-Za-z]+")
  ) %>%
  filter(
    (taxa != "MOTHS" & quartile == "Median") | # Keep Median for all taxa EXCEPT Moths
      (taxa == "MOTHS" & quartile == "Q3")       # Keep Q3 ONLY for Moths
  ) %>%
  # Select and reorder columns for clarity, matching your desired output structure
  select(plotID, forest_cat, YoR, StandAge, taxa_threshold, Climate, taxa, quartile)

# Display the head of the filtered and combined dataframe
cat("Head of the combined and filtered dataframe:\n")
head(recovery_long_filtered)

# --- Assuming `recovery_long_filtered` has been created from the previous steps ---
# (i.e., the code block from the last turn that filters and creates 'taxa' and 'quartile' columns)
# If you are running this in a new R session, please run the code from the previous turn
# that generates `recovery_long_filtered` first.

# Create the wide format dataframe
recovery_wide <- recovery_long_filtered %>%
  # We need to ensure that the 'Climate' column is consistent across rows that will be pivoted
  # for the same plotID, StandAge. Since we already added it earlier, it should be fine.
  # The 'names_from' will be the 'taxa' column, and 'values_from' will be the 'YoR' column.
  pivot_wider(
    id_cols = c(plotID, forest_cat, StandAge, Climate), # These columns will remain as identifying columns
    names_from = taxa,     # The 'taxa' values will become new column names
    values_from = YoR      # The 'YoR' values will populate the new columns
  )

# Display the head of the new wide format dataframe
cat("Head of the wide format dataframe:\n")
head(recovery_wide)

# --- Save both tables as CSV files ---

# Define the output directory (replace with your desired path if different)
# For consistency, let's use the same output directory structure you provided earlier.
output_directory <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"

# Create the directory if it doesn't exist (optional but good practice)
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

# Define file paths
long_file_path <- file.path(output_directory, "recovery_analysis_by_taxon_long_xGraph2.csv")
wide_file_path <- file.path(output_directory, "recovery_analysis_by_taxon_wide_xGraph2.csv")

# Save the long format table
write.csv(recovery_long_filtered, long_file_path, row.names = FALSE)
cat(paste0("\nLong format table saved to: ", long_file_path, "\n"))

# Save the wide format table
write.csv(recovery_wide, wide_file_path, row.names = FALSE)
cat(paste0("Wide format table saved to: ", wide_file_path, "\n"))


#-------------------------------------------------------------------------------
# CREATE THE FINAL TABLE FOR THE SURVIVAL ANALYSIS (NEXT SCRIPT)
# Prepare data for pivoting: Create a cleaner climate label
recovery_long_for_pivot <- recovery_long_filtered %>%
  mutate(
    clean_climate = str_replace_all(Climate, "CLIM_", "") # e.g., 1961_1990, 1991_2020
  )

# Pivot to the desired wide format with taxa-first grouping
recovery_climate_wide_compare <- recovery_long_for_pivot %>%
  pivot_wider(
    id_cols = c(plotID, forest_cat, StandAge), # These columns form the unique rows
    names_from = c(taxa, clean_climate),       # Order of combination: Taxa then Climate
    values_from = YoR,                        # The YoR values for each combination
    names_glue = "{taxa}_{clean_climate}"     # Format the column names as Taxa_Climate
  )

# Define the desired order of taxa (assuming this is consistent for all climate periods)
taxa_order <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")

# Define the climate periods in their desired order
climate_periods <- c("1961_1990", "1991_2020")

# Generate the exact sequence of column names for the desired order (Taxa then Climate)
desired_taxa_climate_cols <- c()
for (taxa_name in taxa_order) {
  for (period in climate_periods) {
    desired_taxa_climate_cols <- c(desired_taxa_climate_cols, paste0(taxa_name, "_", period))
  }
}

# Reorder the columns in the wide dataframe
# Keep the id_cols first, then append the desired taxa_climate columns
recovery_climate_wide_compare_ordered <- recovery_climate_wide_compare %>%
  select(plotID, forest_cat, StandAge, all_of(desired_taxa_climate_cols))


# Display the head of the new wide format dataframe
cat("Head of the combined taxa-climate wide dataframe (taxa grouped, then by climate period):\n")
head(recovery_climate_wide_compare_ordered)

# --- Save the table as CSV and XLSX files ---

# Define the output directory
output_directory <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/old_for_rec_analysis_and_table"

# Create the directory if it doesn't exist
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

# Define file paths
csv_file_path <- file.path(output_directory, "recovery_analysis_by_taxon_climate_wide_xCompare2.csv")
xlsx_file_path <- file.path(output_directory, "recovery_analysis_by_taxon_climate_wide_xCompare2.xlsx")

# Save as CSV
write.csv(recovery_climate_wide_compare_ordered, csv_file_path, row.names = FALSE)
cat(paste0("\nCombined wide table saved to: ", csv_file_path, "\n"))

# Save as XLSX
write.xlsx(recovery_climate_wide_compare_ordered, xlsx_file_path, rowNames = FALSE)
cat(paste0("Combined wide table saved to: ", xlsx_file_path, "\n"))

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# NOW LETS MAKE THE RECOVERY ANALYSIS GRAPHS PAIRED!

library(ggplot2)
library(gridExtra) # For grid.arrange
library(grid)      # For textGrob, gpar
library(cowplot)   # For get_legend

# --- Global Aesthetic Definitions ---

# Forest Category colors
management_colors <- c(
  "Native Broadleaves" = "#FF8247",
  "Non-Native Coniferous" = "darkolivegreen"
)

# Shapes and linetypes
forest_shapes <- c("Native Broadleaves" = 16, "Non-Native Coniferous" = 17)
climate_linetypes <- c("CLIM_1961_1990" = "solid", "CLIM_1991_2020" = "dashed")
taxa_list_order <- c("BEETLES", "BRYOPHYTES", "LICHENS", "MACROFUNGI", "MOTHS")

# --- The modified plot_combined_taxa function ---
plot_combined_taxa <- function(df, taxa_list_param, min_y_value = 0) {
  
  df <- df %>%
    mutate(
      taxa = factor(str_extract(taxa_threshold, "(?<=Q1_|Median_|Q3_)[A-Za-z]+"), levels = taxa_list_order),
      quartile = str_extract(taxa_threshold, "Q1|Median|Q3"),
      Climate = factor(Climate, levels = c("CLIM_1961_1990", "CLIM_1991_2020"))
    ) %>%
    filter(
      (taxa != "MOTHS" & quartile == "Median" & taxa %in% taxa_list_param) |
        (taxa == "MOTHS" & quartile == "Q3" & taxa %in% taxa_list_param)
    )
  
  base_theme <- theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 6),
      legend.position = "bottom"
    )
  
  # Scatter plots
  scatter_std_noleg <- lapply(taxa_list_order, function(taxa_name) {
    d <- df %>% filter(taxa == taxa_name)
    ggplot(d, aes(x = StandAge, y = YoR, color = forest_cat, shape = forest_cat, linetype = Climate)) +
      geom_point(alpha = 0.5, size = 2.5) +
      geom_smooth(method = "lm", se = FALSE, size = 1.2, span = 0.7,
                  aes(group = interaction(forest_cat, Climate))) +
      labs(title = taxa_name, x = "Stand Age", y = "Year of Recovery (YoR)") +
      scale_color_manual(values = management_colors) +
      scale_shape_manual(values = forest_shapes) +
      scale_linetype_manual(values = climate_linetypes) +
      scale_y_continuous(limits = c(min_y_value, NA)) +
      base_theme
  })
  
  scatter_inv_noleg <- lapply(taxa_list_order, function(taxa_name) {
    d <- df %>% filter(taxa == taxa_name)
    ggplot(d, aes(x = YoR, y = StandAge, color = forest_cat, shape = forest_cat, linetype = Climate)) +
      geom_point(alpha = 0.5, size = 2.5) +
      geom_smooth(method = "lm", se = FALSE, size = 1.2, span = 0.7,
                  aes(group = interaction(forest_cat, Climate))) +
      labs(title = taxa_name, x = "Year of Recovery (YoR)", y = "Stand Age") +
      scale_color_manual(values = management_colors) +
      scale_shape_manual(values = forest_shapes) +
      scale_linetype_manual(values = climate_linetypes) +
      scale_x_continuous(limits = c(min_y_value, NA)) +
      base_theme
  })
  
  # Box plot
  
  boxplots_noleg <- lapply(taxa_list_order, function(taxa_name) {
    d <- df %>% filter(taxa == taxa_name)
    ggplot(d, aes(x = Climate, y = YoR, fill = forest_cat)) +
      geom_boxplot(alpha = 0.7, position = position_dodge(0.8),
                   outlier.shape = 21, outlier.size = 1.5, outlier.color = "black") +
      labs(title = taxa_name, x = "", y = "Year of Recovery (YoR)") +
      scale_fill_manual(values = management_colors) +
      scale_y_continuous(limits = c(min_y_value, NA)) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, size = 9),
        axis.title = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 12),
        legend.position = "bottom"
      )
  })
  
  # Shared legends
  scatter_legend <- cowplot::get_legend(
    scatter_std_noleg[[1]] + theme(legend.position = "bottom") +
      guides(color = guide_legend(title = "Forest Category"),
             shape = guide_legend(title = "Forest Category"),
             linetype = guide_legend(title = "Climate Period"))
  )
  
  box_legend <- cowplot::get_legend(
    ggplot(df, aes(x = Climate, y = YoR, fill = forest_cat)) +
      geom_boxplot() +
      scale_fill_manual(values = management_colors, name = "Forest Category") +
      theme_minimal() +
      theme(legend.position = "bottom")
  )
  
  # Convert to grobs
  to_grob_row <- function(plot_list) {
    grobs <- lapply(seq_along(plot_list), function(i) {
      p <- plot_list[[i]]
      if (i != 1) {
        p <- p + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
      }
      p + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
      ggplotGrob(p)
    })
    arrangeGrob(grobs = grobs, nrow = 1)
  }
  
  box_grid <- to_grob_row(boxplots_noleg)
  scatter_std_grid <- to_grob_row(scatter_std_noleg)
  scatter_inv_grid <- to_grob_row(scatter_inv_noleg)
  
  # Titles
  title_box <- textGrob("Year of Recovery by Climate and Forest Types", gp = gpar(fontsize = 14, fontface = "bold"))
  title_std <- textGrob("Stand Age vs Year of Recovery (YoR) from Management Cessation", gp = gpar(fontsize = 14, fontface = "bold"))
  title_inv <- textGrob("Year of Recovery (YoR) vs Stand Age from Management Cessation", gp = gpar(fontsize = 14, fontface = "bold"))
  
  # Final combined panels
  combined_panel_1 <- grid.arrange(
    title_box, 
    box_grid, 
    box_legend,
    title_std, 
    scatter_std_grid, 
    scatter_legend,
    ncol = 1,
    heights = c(0.08, 1, 0.15, 0.08, 1, 0.15)
  )
  
  combined_panel_2 <- grid.arrange(
    title_box, box_grid, box_legend,
    title_inv, scatter_inv_grid, scatter_legend,
    ncol = 1,
    heights = c(0.08, 1, 0.15, 0.08, 1, 0.15)
  )
  
  list(combined_panel_1 = combined_panel_1, combined_panel_2 = combined_panel_2)
}


# Required data setup
taxa_list_for_plots <- c("BEETLES", "BRYOPHYTES", "LICHENS", "MACROFUNGI", "MOTHS")

# Example call
plot_results <- plot_combined_taxa(recovery_long_combined, taxa_list_for_plots, min_y_value = 0)

# Display plots
print(plot_results$combined_panel_1)
print(plot_results$combined_panel_2)

################################################################################
######################            THE END                 ######################
################################################################################