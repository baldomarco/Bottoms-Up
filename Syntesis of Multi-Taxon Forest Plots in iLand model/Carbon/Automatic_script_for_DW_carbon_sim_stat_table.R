# Load necessary libraries
library(dplyr)
library(readxl)
library(writexl)

# Function to create directory if it does not exist
create_directory <- function(directory) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
}

# Function to create data frames for each type of carbon simulation
create_carbon_df <- function(column_name, selected_data, plot_id, selected_years) {
  data.frame(
    plotID = plot_id,
    first_rot = selected_data[[column_name]][selected_data$year == selected_years[1]],
    second_rot = selected_data[[column_name]][selected_data$year == selected_years[2]],
    third_rot = selected_data[[column_name]][selected_data$year == selected_years[3]]
  ) %>% mutate(average = (first_rot + second_rot + third_rot) / 3)
}

# Define a function to process each plot
process_plot <- function(plot_id, run_id, plot_data, dataroot) {
  # Filter the data for the specific plot
  plot_filtered <- plot_data %>% filter(run == run_id)
  
  # Determine the year intervals based on the maximum year
  max_year <- max(plot_filtered$year, na.rm = TRUE)
  selected_years <- if (max_year >= 401) c(160, 320, 480) else c(120, 240, 360)
  
  # Filter and sort the selected years
  selected_data <- plot_filtered %>% filter(year %in% selected_years) %>% arrange(year)
  
  # List of columns to process
  columns_to_process <- c("snags_c", "total_AG_DW_C_sim", "total_DW_C_sim", "total_alive_C_sim", "total_C_sim", "total_stem_C_sim")
  
  # Directory names based on columns
  dir_names <- c("snags_fun", "AG_DW_C_sim_fun", "deadwood_fun", "total_alive_C_sim_fun", "total_carbon_fun", "total_stem_C_sim_fun")
  
  # Process each column and write to corresponding directory
  for (i in seq_along(columns_to_process)) {
    df <- create_carbon_df(columns_to_process[i], selected_data, plot_id, selected_years)
    subdir <- file.path(dataroot, dir_names[i])
    create_directory(subdir)
    write_xlsx(df, file.path(subdir, paste0(columns_to_process[i], "_table_plot_", plot_id, ".xlsx")))
  }
}

# Define the main function to process all plots
process_all_plots <- function(plot_variables_all, plot_ids, run_ids, dataroot) {
  for (i in seq_along(plot_ids)) {
    process_plot(plot_ids[i], run_ids[i], plot_variables_all, dataroot)
  }
}

# Define the root directory
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/New folder/"

# Example plot IDs and run IDs (replace with actual values)
plot_ids <- c("L1_03", "L1_07", "L1_10", "L1_13", "L1_17", "L1_27", "L1_31", "L1_33", "L1_34", "L1_36", "L1_38", "L1_43",
              "L1_44", "L1_48", "L2_01", "L2_05", "L2_06", "L2_09", "L2_13", "L2_14", "L2_16", "L2_17", "L2_18", "L2_20",
              "L2_26", "L2_32", "L2_33", "L2_38", "L2_44", "L3_02", "L3_12", "L3_18", "L3_23", "L3_24", "L3_26", "L3_27",
              "L3_31", "L3_32", "L3_34", "L3_37", "L3_38", "L3_41", "L3_42", "L4_02", "L4_04", "L4_10", "L4_11", "L4_16",
              "L4_17", "L4_18", "L4_20", "L4_23", "L4_25", "L4_26", "L4_27", "L4_30", "L4_32", "L4_39", "L4_42", "L5_01",
              "L5_05", "L5_12", "L5_18", "L5_19", "L5_21", "L5_25", "L5_28", "L5_32", "L5_33", "L5_37", "L5_38", "L6_01",
              "L6_02", "L6_03", "L6_04", "L6_11", "L6_14", "L6_15", "L6_17", "L6_19", "L6_21")

run_ids <- c("DB_CZ_JH1_L1XL1_03_plot.sqlite", "DB_CZ_JH1_L1XL1_07_plot.sqlite", "DB_CZ_JH1_L1XL1_10_plot.sqlite",
             "DB_CZ_JH1_L1XL1_13_plot.sqlite", "DB_CZ_JH1_L1XL1_17_plot.sqlite", "DB_CZ_JH1_L1XL1_27_plot.sqlite",
             "DB_CZ_JH1_L1XL1_31_plot.sqlite", "DB_CZ_JH1_L1XL1_33_plot.sqlite", "DB_CZ_JH1_L1XL1_34_plot.sqlite",
             "DB_CZ_JH1_L1XL1_36_plot.sqlite", "DB_CZ_JH1_L1XL1_38_plot.sqlite", "DB_CZ_JH1_L1XL1_43_plot.sqlite",
             "DB_CZ_JH1_L1XL1_44_plot.sqlite", "DB_CZ_JH1_L1XL1_48_plot.sqlite", "DB_CZ_JH1_L2XL2_01_plot.sqlite",
             "DB_CZ_JH1_L2XL2_05_plot.sqlite", "DB_CZ_JH1_L2XL2_06_plot.sqlite", "DB_CZ_JH1_L2XL2_09_plot.sqlite",
             "DB_CZ_JH1_L2XL2_13_plot.sqlite", "DB_CZ_JH1_L2XL2_14_plot.sqlite", "DB_CZ_JH1_L2XL2_16_plot.sqlite",
             "DB_CZ_JH1_L2XL2_17_plot.sqlite", "DB_CZ_JH1_L2XL2_18_plot.sqlite", "DB_CZ_JH1_L2XL2_20_plot.sqlite",
             "DB_CZ_JH1_L2XL2_26_plot.sqlite", "DB_CZ_JH1_L2XL2_32_plot.sqlite", "DB_CZ_JH1_L2XL2_33_plot.sqlite",
             "DB_CZ_JH1_L2XL2_38_plot.sqlite", "DB_CZ_JH1_L2XL2_44_plot.sqlite", "DB_CZ_JH1_L3XL3_02_plot.sqlite",
             "DB_CZ_JH1_L3XL3_12_plot.sqlite", "DB_CZ_JH1_L3XL3_18_plot.sqlite", "DB_CZ_JH1_L3XL3_23_plot.sqlite",
             "DB_CZ_JH1_L3XL3_24_plot.sqlite", "DB_CZ_JH1_L3XL3_26_plot.sqlite", "DB_CZ_JH1_L3XL3_27_plot.sqlite",
             "DB_CZ_JH1_L3XL3_31_plot.sqlite", "DB_CZ_JH1_L3XL3_32_plot.sqlite", "DB_CZ_JH1_L3XL3_34_plot.sqlite",
             "DB_CZ_JH1_L3XL3_37_plot.sqlite", "DB_CZ_JH1_L3XL3_38_plot.sqlite", "DB_CZ_JH1_L3XL3_41_plot.sqlite",
             "DB_CZ_JH1_L3XL3_42_plot.sqlite", "DB_CZ_JH1_L4XL4_02_plot.sqlite", "DB_CZ_JH1_L4XL4_04_plot.sqlite",
             "DB_CZ_JH1_L4XL4_10_plot.sqlite", "DB_CZ_JH1_L4XL4_11_plot.sqlite", "DB_CZ_JH1_L4XL4_16_plot.sqlite",
             "DB_CZ_JH1_L4XL4_17_plot.sqlite", "DB_CZ_JH1_L4XL4_18_plot.sqlite", "DB_CZ_JH1_L4XL4_20_plot.sqlite",
             "DB_CZ_JH1_L4XL4_23_plot.sqlite", "DB_CZ_JH1_L4XL4_25_plot.sqlite", "DB_CZ_JH1_L4XL4_26_plot.sqlite",
             "DB_CZ_JH1_L4XL4_27_plot.sqlite", "DB_CZ_JH1_L4XL4_30_plot.sqlite", "DB_CZ_JH1_L4XL4_32_plot.sqlite",
             "DB_CZ_JH1_L4XL4_39_plot.sqlite", "DB_CZ_JH1_L4XL4_42_plot.sqlite", "DB_CZ_JH1_L5XL5_01_plot.sqlite",
             "DB_CZ_JH1_L5XL5_05_plot.sqlite", "DB_CZ_JH1_L5XL5_12_plot.sqlite", "DB_CZ_JH1_L5XL5_18_plot.sqlite",
             "DB_CZ_JH1_L5XL5_19_plot.sqlite", "DB_CZ_JH1_L5XL5_21_plot.sqlite", "DB_CZ_JH1_L5XL5_25_plot.sqlite",
             "DB_CZ_JH1_L5XL5_28_plot.sqlite", "DB_CZ_JH1_L5XL5_32_plot.sqlite", "DB_CZ_JH1_L5XL5_33_plot.sqlite",
             "DB_CZ_JH1_L5XL5_37_plot.sqlite", "DB_CZ_JH1_L5XL5_38_plot.sqlite", "DB_CZ_JH1_L6XL6_01_plot.sqlite",
             "DB_CZ_JH1_L6XL6_02_plot.sqlite", "DB_CZ_JH1_L6XL6_03_plot.sqlite", "DB_CZ_JH1_L6XL6_04_plot.sqlite",
             "DB_CZ_JH1_L6XL6_11_plot.sqlite", "DB_CZ_JH1_L6XL6_14_plot.sqlite", "DB_CZ_JH1_L6XL6_15_plot.sqlite",
             "DB_CZ_JH1_L6XL6_17_plot.sqlite", "DB_CZ_JH1_L6XL6_19_plot.sqlite", "DB_CZ_JH1_L6XL6_21_plot.sqlite")

# Process all plots
process_all_plots(plot_variables_all, plot_ids, run_ids, dataroot)

# Function to combine all Excel files in a directory into a single data frame
combine_excel_files <- function(directory) {
  excel_files <- list.files(path = directory, pattern = "\\.xlsx$", full.names = TRUE)
  list_of_dfs <- lapply(excel_files, read_xlsx)
  bind_rows(list_of_dfs)
}

# Combine data frames for each type and write the combined files
types <- c("deadwood_fun", "total_carbon_fun", "AG_DW_C_sim_fun", "total_alive_C_sim_fun", "total_stem_C_sim_fun", "snags_fun")

for (type in types) {
  combined_df <- combine_excel_files(file.path(dataroot, type))
  write_xlsx(combined_df, file.path(dataroot, type, paste0(type, "_all.xlsx")))
}

