###############     MARCO BALDO, MSC 20/06/2025     ##############
#       WRITE THE NAMES OF THE PROJECT FILES INTO A SINGLE .bat FILE.   ########


#-------------------------------------------------------------------------------
# folder path
folder_path <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment - Copy - Copy/"

# List all .xml files 
xml_files <- list.files(path = folder_path, pattern = "\\.xml$", full.names = FALSE)

# Create the name of the batch file to write the project file in the folder
bat_file <- file.path(folder_path, "Bottom_UP_run_all.bat")

# Create lines for the .bat file
lines <- paste0("ilandc.exe ", xml_files, " 600")

# Write the lines to the .bat file
writeLines(lines, bat_file)

cat("Batch file written to:", bat_file)
#-------------------------------------------------------------------------------

# FILE BACK UP INTO: 
#         DELL COMPUTER,
#         HARD DISK SanDisk 
#         GitHub folder Project_file