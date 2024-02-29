# Marco Baldo 
# 17/12/2023
# baldo@fld.czu.cz 

library(dplyr)
library(readxl)
library(writexl)

# Import the required tables
age_L1_10 <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/plot_init/Jenik/final_table_imp/Plot L1_10 TreeAgesDerivedfromYieldTables.xlsx")
init_L1_10 <- read.table("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/init/L1_10_init.txt", header = TRUE, sep = ";")

# Look at them
str(age_L1_10)
head(age_L1_10)

str(init_L1_10)
head(init_L1_10)


#-------------------------------------------------------------------------------
# Manipulate the age dataframe to have only what we are interest to know = age and multiply by 4 to have the same number of lines that we have in init (remind we are synthesise information from forest field samples at 50mx50m are in iLand model at 100mx100m area, that's why we repeat the trees x4)

filtered_age_L1_10 <- age_L1_10[, 12]

# Replicate each value four times
rep_age <- rep(filtered_age_L1_10$`mean age`, each = 4)

# Round just for the integers
round_rep_age <- as.integer(round(as.numeric(rep_age)))

# Create a new dataframe with the replicated values in a single column
new_age_L1_10 <- data.frame(round_rep_age)

# Print the new dataframe
print(new_age_L1_10)

mean(new_age_L1_10$round_rep_age)
#-------------------------------------------------------------------------------
# Combind columns between the init_L1_10 dataframe and age
init_L1_10 <- bind_cols(init_L1_10, age = new_age_L1_10$round_rep_age)

# Write the updated data to the same path

write.table(init_L1_10, 
            file = "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/init/L1_10_init_age.txt",
            append = FALSE, 
            quote = FALSE, 
            sep = " ", 
            eol = "\n", 
            na = "NA",
            dec = ".", 
            row.names = FALSE, 
            col.names = TRUE)

