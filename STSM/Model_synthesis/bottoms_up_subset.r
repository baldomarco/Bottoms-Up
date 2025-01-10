#                                       Dr. Marco Baldo, MSc
# 
#                                27/03/2023  Sapienza - University of Rome


# to convert a list of coordinates from angular to metric units in R
# To convert a list of coordinates from angular to metric units in R
# To set a smaller value for cellsize, you can modify the cellsize argument in the rasterize() function. For example, to set cellsize to 0.1, you can use the following 

# Load the database
load("C:/_R/R_bottoms_up/db/alldata (1).RData")     # Bottoms-Up data

speciesrichnessdata_95_CZ <- readRDS("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/R/stsm_roma/official/speciesrichnessdata_95_CZ.rds")
C_stock_total_CZ <- readRDS("C:/_R/R_bottoms_up/db/C_stock_total_CZ.rds")
dataBIRD_CZ <- readRDS("C:/_R/R_bottoms_up/db/dataBIRD_CZ.rds")
dataBRYO_CZ <- readRDS("C:/_R/R_bottoms_up/db/dataBRYO_CZ.rds")
dataFU_CZ <- readRDS("C:/_R/R_bottoms_up/db/dataFU_CZ.rds")
dataLI_CZ <- readRDS("C:/_R/R_bottoms_up/db/dataLI_CZ.rds")
dataSB_CZ <- readRDS("C:/_R/R_bottoms_up/db/dataSB_CZ.rds")
dataVP_CZ <- readRDS("C:/_R/R_bottoms_up/db/dataVP_CZ.rds")
deadwood_unit_CZ <- readRDS("C:/_R/R_bottoms_up/db/deadwood_unit_CZ.rds")
plotdescdata_CZ <- readRDS("C:/_R/R_bottoms_up/db/plotdescdata_CZ.rds")
standtrees_unit_CZ <- readRDS("C:/_R/R_bottoms_up/db/standtrees_unit_CZ.rds")
chelsa_CZ <- readRDS("C:/_R/R_bottoms_up/db/chelsa_CZ.rds")
datasoil_CZ <- readRDS("C:/_R/R_bottoms_up/db/datasoil_CZ.rds")

#-------------------------------------------------------------------------------
# Create the framework for make the plot list from the central DB
# First select only CZ site (CZ_JH1)

# Subset the db for the CZ_JH1 group of sites
CZ_JH1 <- subset(treedata, file == "Raw_data_structure20200515_CZ_JH1.xlsx")

# Check it
print(CZ_JH1)
str(CZ_JH1)
list(CZ_JH1)

unique_key <- unique(CZ_JH1$keyID)
length(unique_key)

unique_key <- unique(dataFU_CZ$keyID)
length(unique_key)

unique_key <- unique(dataBRYO_CZ$keyID)
length(unique_key)

unique_key <- unique(dataLI_CZ$keyID)
length(unique_key)

unique_key <- unique(dataSB_CZ$keyID)
length(unique_key)

unique_key <- unique(dataBIRD_CZ$keyID)
length(unique_key)

unique_key <- unique(dataVP_CZ$keyID)
length(unique_key)

# Define the IDs you want to select
target_IDs_cat6 <- c("L1_03", "L1_13", "L1_18","L1_22","L1_24","L1_26","L1_33","L1_34","L1_48","L4_06","L4_09","L4_10","L4_16","L4_25","L4_31","L4_33","L5_25")
target_IDs_cat7 <- c("L2_05", "L2_06", "L2_09","L2_13","L2_14","L2_16","L2_18","L2_20","L2_27","L2_30","L2_33","L2_34","L3_02","L3_06","L3_09","L3_10","L3_12","L3_16","L3_24","L3_26","L3_31","L3_32","L3_42","L4_04","L4_26")


# Select rows where plotID matches target IDs
beech_forest_6 <- CZ_JH1[CZ_JH1$plotID %in% target_IDs_cat6, ]
beech_forest_7 <- CZ_JH1[CZ_JH1$plotID %in% target_IDs_cat7, ]

# Print the subset of data
print(beech_forest_6)
print(beech_forest_7)

unique_sites <- unique(beech_forest_6$keyID)
print(unique_sites)

unique_sites <- unique(beech_forest_7$keyID)
print(unique_sites)

# Create the database I need for all of them
# Select rows where plotID matches target IDs using keyIDs

target_cat6 <- c("CZ_JH1_L1XL1_13","CZ_JH1_L1XL1_18", "CZ_JH1_L1XL1_22", "CZ_JH1_L1XL1_24",
                       "CZ_JH1_L1XL1_26", "CZ_JH1_L1XL1_33", "CZ_JH1_L1XL1_34", "CZ_JH1_L1XL1_03",
                       "CZ_JH1_L1XL1_48", "CZ_JH1_L4XL4_10", "CZ_JH1_L4XL4_16", "CZ_JH1_L4XL4_25",
                       "CZ_JH1_L4XL4_31", "CZ_JH1_L4XL4_33" ,"CZ_JH1_L4XL4_06" ,"CZ_JH1_L4XL4_09",
                       "CZ_JH1_L5XL5_25")

target_cat7 <- c("CZ_JH1_L2XL2_13", "CZ_JH1_L2XL2_14", "CZ_JH1_L2XL2_16", "CZ_JH1_L2XL2_18",
                       "CZ_JH1_L2XL2_20", "CZ_JH1_L2XL2_27", "CZ_JH1_L2XL2_30", "CZ_JH1_L2XL2_33",
                       "CZ_JH1_L2XL2_34", "CZ_JH1_L2XL2_05", "CZ_JH1_L2XL2_06", "CZ_JH1_L2XL2_09",
                       "CZ_JH1_L3XL3_10", "CZ_JH1_L3XL3_12", "CZ_JH1_L3XL3_16", "CZ_JH1_L3XL3_24",
                       "CZ_JH1_L3XL3_26", "CZ_JH1_L3XL3_02", "CZ_JH1_L3XL3_31", "CZ_JH1_L3XL3_32",
                       "CZ_JH1_L3XL3_42", "CZ_JH1_L3XL3_06", "CZ_JH1_L3XL3_09", "CZ_JH1_L4XL4_26",
                       "CZ_JH1_L4XL4_04")

target_cat67 <- c("CZ_JH1_L1XL1_13","CZ_JH1_L1XL1_18", "CZ_JH1_L1XL1_22", "CZ_JH1_L1XL1_24",
                                 "CZ_JH1_L1XL1_26", "CZ_JH1_L1XL1_33", "CZ_JH1_L1XL1_34", "CZ_JH1_L1XL1_03",
                                 "CZ_JH1_L1XL1_48", "CZ_JH1_L4XL4_10", "CZ_JH1_L4XL4_16", "CZ_JH1_L4XL4_25",
                                 "CZ_JH1_L4XL4_31", "CZ_JH1_L4XL4_33" ,"CZ_JH1_L4XL4_06" ,"CZ_JH1_L4XL4_09",
                                 "CZ_JH1_L5XL5_25","CZ_JH1_L2XL2_13", "CZ_JH1_L2XL2_14", "CZ_JH1_L2XL2_16", "CZ_JH1_L2XL2_18",
                 "CZ_JH1_L2XL2_20", "CZ_JH1_L2XL2_27", "CZ_JH1_L2XL2_30", "CZ_JH1_L2XL2_33",
                 "CZ_JH1_L2XL2_34", "CZ_JH1_L2XL2_05", "CZ_JH1_L2XL2_06", "CZ_JH1_L2XL2_09",
                 "CZ_JH1_L3XL3_10", "CZ_JH1_L3XL3_12", "CZ_JH1_L3XL3_16", "CZ_JH1_L3XL3_24",
                 "CZ_JH1_L3XL3_26", "CZ_JH1_L3XL3_02", "CZ_JH1_L3XL3_31", "CZ_JH1_L3XL3_32",
                 "CZ_JH1_L3XL3_42", "CZ_JH1_L3XL3_06", "CZ_JH1_L3XL3_09", "CZ_JH1_L4XL4_26",
                 "CZ_JH1_L4XL4_04")


# Select Carbon 
beech_forest_6_C <- C_stock_total_CZ[C_stock_total_CZ$keyID %in% target_cat6, ]
beech_forest_7_C <- C_stock_total_CZ[C_stock_total_CZ$keyID %in% target_cat7, ]
beech_forest_67_C <- C_stock_total_CZ[C_stock_total_CZ$keyID %in% target_cat67, ]

# Select birds
beech_forest_6_BIRD <- dataBIRD_CZ[dataBIRD_CZ$keyID %in% target_cat6, ]
beech_forest_7_BIRD <- dataBIRD_CZ[dataBIRD_CZ$keyID %in% target_cat7, ]
beech_forest_67_BIRD <- dataBIRD_CZ[dataBIRD_CZ$keyID %in% target_cat67, ]


# Select bryophytes
beech_forest_6_BRYO <- dataBRYO_CZ[dataBRYO_CZ$keyID %in% target_cat6, ]
beech_forest_7_BRYO <- dataBRYO_CZ[dataBRYO_CZ$keyID %in% target_cat7, ]
beech_forest_67_BRYO <- dataBRYO_CZ[dataBRYO_CZ$keyID %in% target_cat67, ]

# Select fungi
beech_forest_6_FU <- dataFU_CZ[dataFU_CZ$keyID %in% target_cat6, ]
beech_forest_7_FU <- dataFU_CZ[dataFU_CZ$keyID %in% target_cat7, ]
beech_forest_67_FU <- dataFU_CZ[dataFU_CZ$keyID %in% target_cat67, ]

# Select lichens
beech_forest_6_LI <- dataLI_CZ[dataLI_CZ$keyID %in% target_cat6, ]
beech_forest_7_LI <- dataLI_CZ[dataLI_CZ$keyID %in% target_cat7, ]
beech_forest_67_LI <- dataLI_CZ[dataLI_CZ$keyID %in% target_cat67, ]

# Select saproxilic beetles
beech_forest_6_SB <- dataSB_CZ[dataSB_CZ$keyID %in% target_cat6, ]
beech_forest_7_SB <- dataSB_CZ[dataSB_CZ$keyID %in% target_cat7, ]
beech_forest_67_SB <- dataSB_CZ[dataSB_CZ$keyID %in% target_cat67, ]

# Select vascular plants
beech_forest_6_VP <- dataVP_CZ[dataVP_CZ$keyID %in% target_cat6, ]
beech_forest_7_VP <- dataVP_CZ[dataVP_CZ$keyID %in% target_cat7, ]
beech_forest_67_VP <- dataVP_CZ[dataVP_CZ$keyID %in% target_cat67, ]

# Select deadwood_unit 
beech_forest_6_deadwood <- deadwood_unit_CZ[deadwood_unit_CZ$keyID %in% target_cat6, ]
beech_forest_7_deadwood <- deadwood_unit_CZ[deadwood_unit_CZ$keyID %in% target_cat7, ]
beech_forest_67_deadwood <- deadwood_unit_CZ[deadwood_unit_CZ$keyID %in% target_cat67, ]

# Select species
unique_sp <- unique(beech_forest_6_BIRD$genspe)
length(unique_sp)

unique_sp <- unique(beech_forest_6_BRYO$genspe)
length(unique_sp)

unique_sp <- unique(beech_forest_6_LI$genspe)
length(unique_sp)

unique_sp <- unique(beech_forest_6_FU$genspe)
length(unique_sp)

unique_sp <- unique(beech_forest_6_SB$genspe)
length(unique_sp)

unique_sp <- unique(beech_forest_6_VP$genspe)
length(unique_sp)

#-------------------------------------------------------------------------------
# Mutate NA values in column 'treevol' into 0 to make the sum of volumes per species. The missing values or the zero volume are related to trees with a dbh minor of 7 cm. Only in rare cases they are associated also to trees with a dbh major than 7 cm and the maximum case is a Tilia cordata of 11.25 cm

library(dplyr)

CZ_JH1$treevol <- ifelse(is.na(CZ_JH1$treevol), 0, CZ_JH1$treevol)

# Convert non-numeric values to NA
CZ_JH1$treevol <- as.numeric(CZ_JH1$treevol)

# Summarize tree volume by plot ID (if is giving an error just restart the R session, it is due a conflict between R packages with dplyr)
volume_m3 <- CZ_JH1 %>%
  group_by(keyID) %>%
  summarize(volume_m3 = sum(treevol, na.rm = TRUE))

#-------------------------------------------------------------------------------
# Create the mean of the dbh as a df

# Subset the db for the CZ_JH1 group of sites
CZ_JH1 <- subset(treedata, file == "Raw_data_structure20200515_CZ_JH1.xlsx")

is.numeric(CZ_JH1$treedb)

# Convert non-numeric values to NA
CZ_JH1$treedb <- as.numeric(CZ_JH1$treedb)

# Summarize tree volume by plot ID
mean_dbh_cm <- CZ_JH1 %>%
  group_by(keyID) %>%
  summarize(mean_dbh_cm = mean(treedb, na.rm = TRUE))

#-------------------------------------------------------------------------------

is.numeric(plotdescdata_CZ$staage)

# Convert non-numeric values to NA
plotdescdata_CZ$staage <- as.numeric(plotdescdata_CZ$staage)

stand_age <- c(5, 182, 107, 5, 90, 122, 99, 144, 187, 200, 187, 76, 79, 117, 169, 80, 29, 21, 91, 88, 98,
              5, 168, 125, 21, 175, 134, 146, 94, 175, 180, 5, 50, 315, 315, 106, 315, 195, 114, 117, 148, 269,
              244, 269, 85, 244, 113, 103, 144, 54, 133, 63, 82, 5, 87, 5, 124, 27, 147, 109, 54, 14, 156,
              156, 156, 25, 78, 106, 121, 78, 110, 111, 110, 121, 5, 194, 104, 194, 5, 77, 78, 5, 133, 86,
              132, 27, 103, 173, 29, 123, 133, 5, 15, 21, 92, 92, 52, 182, 182, 182, 77, 83, 92, 112, 92, 122)

age <- data.frame(plotdescdata_CZ$keyID, stand_age)

#-------------------------------------------------------------------------------
# Check if the 'integers' column contains only integers
if (all(sapply(CZ_JH1$plotID, is.integer))) {
  print("The 'integers' column contains only integers.")
} else {
  print("The 'integers' column contains non-integer values.")
}

# Check if the 'numerics' column contains only numeric values
if (all(sapply(CZ_JH1$treevol, is.numeric))) {
  print("The 'numerics' column contains only numeric values.")
} else {
  print("The 'numerics' column contains non-numeric values.")
}
#-------------------------------------------------------------------------------
# CREATE SHANNON VARIABLE - NOT READY YET
library(tidyverse)

H_data_sp<- CZ_JH1 %>% group_by(plotID, treesp) %>% summarize(VOL=(treevol), DBH=(treedb), HT=(treeht), CD=(crowndep))
H_data_sp <- H_data_sp %>% drop_na(VOL) # to use this function you need tidyverse

is.na(H_data_sp)
is.numeric(H_data_sp$VOL)
is.numeric(H_data_sp$DBH)
is.numeric(H_data_sp$HT)
is.numeric(H_data_sp$CD)

# In case of zero values
s <-landscape %>% group_by(year) %>% filter(volume_m3>0 & year>0) %>% summarise(n=n())   # number of species in each year  (added the filter to count non-zero volumes, now it is okay)
print(S$n)


t<-annual.spec.data %>% right_join(annual.data, by="year")  %>% 
  mutate(prop.VOL=VOL/VOL.tot, prop.BA=BA/BA.tot, prop.count=count/count.tot) %>% 
  filter(prop.VOL>0)  # here I also filtering them to have only records with m3>0

#https://www.statology.org/shannon-diversity-index/
#https://www.statology.org/shannon-diversity-index-calculator/

# Shannon diversity index (SDI): this is already Shannon.... that extra step by dividing the by S$n is making equitability index based on the link above.
# so maybe we can make shannon based on BA, VOL and number of trees.... (? can discuss or save all 3 and will see...)

H.BA<-t %>% group_by(year) %>% summarize(H=-1*sum(prop.BA*log(prop.BA)))
H.VOL<-t %>% group_by(year) %>% summarize(H=-1*sum(prop.VOL*log(prop.VOL)))
H.count<-t %>% group_by(year) %>% summarize(H=-1*sum(prop.count*log(prop.count)))   # here I just put the proportion of number of trees
set.panel(3,1)
plot(H.BA, main= "H.BA")
plot(H.VOL, main= "H.vol")
plot(H.count, main="H.count")


#-------------------------------------------------------------------------------
# Data manipulation to harmonize the matrix for the biodiversity analysis of the system (PRESENCE - ABSENCE)
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/plots/species_df/"

# Define a vector of data frame names
original_df_names <- c("beech_forest_6_BIRD", "beech_forest_7_BIRD", "beech_forest_6_BRYO","beech_forest_7_BRYO","beech_forest_6_FU", "beech_forest_7_FU","beech_forest_6_LI", "beech_forest_7_LI","beech_forest_6_SB","beech_forest_7_SB","beech_forest_6_VP","beech_forest_7_VP")

for (df_name in original_df_names) {
  # Get the original data frame
  original_df <- get(df_name)
  
  # Remove columns not needed
  original_df <- original_df[, !(colnames(original_df) %in% c("siteID", "taxon"))]
  
  # Get unique plot IDs and species names
  plot_ids <- unique(original_df$keyID)
  species_names <- unique(original_df$genspe)
  
  # Create a new data frame with plot IDs in the first column
  new_df <- data.frame(plotID = plot_ids)
  
  # Add one column for each unique species
  for (species_name in species_names) {
    # Check if the species is present in each plot
    presence <- rep(0, length(plot_ids))
    for (i in 1:length(plot_ids)) {
      if (any(original_df$keyID == plot_ids[i] & original_df$genspe == species_name)) {
        presence[i] <- 1
      }
    }
    
    # Add the presence column to the new data frame
    new_df[[species_name]] <- presence
  }
  
  # Save the new data frame as a CSV file with a similar name to the original
  new_file <- paste0(dataroot, df_name, "_new.csv")
  write.csv(new_df, new_file, row.names = FALSE)
  
  # Create a data frame with columns A and B removed
  removed_cols_df <- original_df[, !(colnames(original_df) %in% c("siteID", "taxon"))]
  
  # Save the removed columns data frame as a CSV file with the same name as the original
  removed_cols_file <- paste0(dataroot, df_name, "_removed_cols.csv")
  write.csv(removed_cols_df, removed_cols_file, row.names = TRUE)
  
  # Rename the data frames in R and assign to global environment
  new_df_name <- paste0(df_name, "_new")
  removed_cols_name <- paste0(df_name, "_removed_cols")
  
  assign(new_df_name, new_df, envir = .GlobalEnv)
  assign(removed_cols_name, removed_cols_df, envir = .GlobalEnv)
}

#-------------------------------------------------------------------------------
# Edit the speciesrichnessdata_95_CZ in the way to explore the first linear relationships

Cz_Birds_sr <-  speciesrichnessdata_95_CZ %>% filter(taxon == "Birds")
str(Cz_Birds_sr)

Cz_Tracheophyta_sr <-  speciesrichnessdata_95_CZ %>% filter(taxon == "Tracheophyta")
str(Cz_Tracheophyta_sr)

Cz_Lichens_sr <-  speciesrichnessdata_95_CZ %>% filter(taxon == "Lichens")
str(Cz_Lichens_sr)

Cz_Bryophytes_sr <-  speciesrichnessdata_95_CZ %>% filter(taxon == "Bryophytes")
str(Cz_Bryophytes_sr)

Cz_Fungi_sr <-  speciesrichnessdata_95_CZ %>% filter(taxon == "Fungi")
str(Cz_Fungi_sr)

Cz_Beetles_sr <-  speciesrichnessdata_95_CZ %>% filter(taxon == "Beetles")
str(Cz_Beetles_sr)

# To harmonized the dataset for Carbon pools
str(C_stock_total_CZ)

# Being different I am going to edit cleaning the missing plots before to use the Pearson Correlation Matrix

# First catch the mismatching 
# Print mismatching names
diff <- setdiff(Cz_Fungi_sr$keyID, C_stock_total_CZ$keyID)
diff

# "CZ_JH1_L1XL1_02" "CZ_JH1_L1XL1_09" "CZ_JH1_L2XL2_04" "CZ_JH1_L2XL2_21"
# "CZ_JH1_L3XL3_33" "CZ_JH1_L3XL3_36" "CZ_JH1_L5XL5_04"

# Remove rows in taxon species richness in plots where are not present carbon stocks outputs

Cz_Birds_sr_c <- Cz_Birds_sr %>% 
  filter(! (keyID %in% c("CZ_JH1_L1XL1_02", "CZ_JH1_L1XL1_09", "CZ_JH1_L2XL2_04", "CZ_JH1_L2XL2_21", "CZ_JH1_L3XL3_33", "CZ_JH1_L3XL3_36", "CZ_JH1_L5XL5_04")))

Cz_Beetles_sr_c <- Cz_Beetles_sr %>% 
  filter(! (keyID %in% c("CZ_JH1_L1XL1_02", "CZ_JH1_L1XL1_09", "CZ_JH1_L2XL2_04", "CZ_JH1_L2XL2_21", "CZ_JH1_L3XL3_33", "CZ_JH1_L3XL3_36", "CZ_JH1_L5XL5_04")))

Cz_Fungi_sr_c <- Cz_Fungi_sr %>% 
  filter(! (keyID %in% c("CZ_JH1_L1XL1_02", "CZ_JH1_L1XL1_09", "CZ_JH1_L2XL2_04", "CZ_JH1_L2XL2_21", "CZ_JH1_L3XL3_33", "CZ_JH1_L3XL3_36", "CZ_JH1_L5XL5_04")))

Cz_Lichens_sr_c <- Cz_Lichens_sr %>% 
  filter(! (keyID %in% c("CZ_JH1_L1XL1_02", "CZ_JH1_L1XL1_09", "CZ_JH1_L2XL2_04", "CZ_JH1_L2XL2_21", "CZ_JH1_L3XL3_33", "CZ_JH1_L3XL3_36", "CZ_JH1_L5XL5_04")))

Cz_Tracheophyta_sr_c <- Cz_Tracheophyta_sr %>% 
  filter(! (keyID %in% c("CZ_JH1_L1XL1_02", "CZ_JH1_L1XL1_09", "CZ_JH1_L2XL2_04", "CZ_JH1_L2XL2_21", "CZ_JH1_L3XL3_33", "CZ_JH1_L3XL3_36", "CZ_JH1_L5XL5_04")))

#-------------------------------------------------------------------------------

plot(Cz_Beetles_sr$observed_richness, Cz_Fungi_sr$observed_richness)
x <- lm(Cz_Beetles_sr$observed_richness ~ Cz_Fungi_sr$observed_richness)
summary(x)

plot(Cz_Birds_sr$observed_richness, Cz_Beetles_sr$observed_richness)
lm(Cz_Birds_sr$observed_richness ~ Cz_Beetles_sr$observed_richness)

df <- data.frame(Cz_Birds_sr[,2], Cz_Birds_sr[,4], Cz_Beetles_sr[,4], Cz_Fungi_sr[,4],Cz_Lichens_sr[,4], Cz_Tracheophyta_sr[,4])
names(df) <- c("siteID","Birds","Beetles","Fungi","Lichens","Tracheophyta")

df1 <- data.frame(Cz_Beetles_sr$observed_richness,Cz_Fungi_sr$observed_richness)
df2 <- data.frame(Cz_Birds_sr$observed_richness,Cz_Beetles_sr$observed_richness)
df3 <- data.frame(Cz_Tracheophyta_sr$observed_richness,Cz_Beetles_sr$observed_richness)
df4 <- data.frame(Cz_Tracheophyta_sr$observed_richness,Cz_Fungi_sr$observed_richness)
df5 <- data.frame(Cz_Tracheophyta_sr$observed_richness,Cz_Lichens_sr$observed_richness)
df6 <- data.frame(Cz_Lichens_sr$observed_richness,Cz_Fungi_sr$observed_richness)

library(ggplot2)
library(GGally)

a1 <- ggplot(df1, aes(Cz_Beetles_sr$observed_richness,Cz_Fungi_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Fungi obs. rich.")) + geom_point() + stat_smooth(method = "lm", formula = y ~ x)
a1 <- ggplot(df1, aes(Cz_Beetles_sr$observed_richness,Cz_Fungi_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Fungi obs. rich.")) + geom_point() + stat_smooth(method = "loess", formula = y ~ x)
a1 <- ggplot(df1, aes(Cz_Beetles_sr$observed_richness,Cz_Fungi_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Fungi obs. rich.")) + geom_point() + stat_smooth(method = "gam", formula = y ~ x)

a1 <- ggplot(df2, aes(Cz_Beetles_sr$observed_richness,Cz_Birds_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Birds obs. rich.")) + geom_point() + stat_smooth(method = "lm", formula = y ~ x)
a1 <- ggplot(df2, aes(Cz_Beetles_sr$observed_richness,Cz_Birds_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Birds obs. rich.")) + geom_point() + stat_smooth(method = "loess", formula = y ~ x)
a1 <- ggplot(df2, aes(Cz_Beetles_sr$observed_richness,Cz_Birds_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Birds obs. rich.")) + geom_point() + stat_smooth(method = "gam", formula = y ~ x)

a1 <- ggplot(df3, aes(Cz_Tracheophyta_sr$observed_richness,Cz_Fungi_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Birds obs. rich.")) + geom_point() + stat_smooth(method = "lm", formula = y ~ x)
a1 <- ggplot(df3, aes(Cz_Tracheophyta_sr$observed_richness,Cz_Fungi_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Birds obs. rich.")) + geom_point() + stat_smooth(method = "loess", formula = y ~ x)
a1 <- ggplot(df3, aes(Cz_Tracheophyta_sr$observed_richness,Cz_Fungi_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Birds obs. rich.")) + geom_point() + stat_smooth(method = "gam", formula = y ~ x)

a1 <- ggplot(df4, aes(Cz_Beetles_sr$observed_richness,Cz_Tracheophyta_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Birds obs. rich.")) + geom_point() + stat_smooth(method = "lm", formula = y ~ x)
a1 <- ggplot(df4, aes(Cz_Beetles_sr$observed_richness,Cz_Tracheophyta_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Birds obs. rich.")) + geom_point() + stat_smooth(method = "loess", formula = y ~ x)
a1 <- ggplot(df4, aes(Cz_Beetles_sr$observed_richness,Cz_Tracheophyta_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Birds obs. rich.")) + geom_point() + stat_smooth(method = "gam", formula = y ~ x)

a1 <- ggplot(df5, aes(Cz_Tracheophyta_sr$observed_richness,Cz_Lichens_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Birds obs. rich.")) + geom_point() + stat_smooth(method = "lm", formula = y ~ x)
a1 <- ggplot(df5, aes(Cz_Tracheophyta_sr$observed_richness,Cz_Lichens_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Birds obs. rich.")) + geom_point() + stat_smooth(method = "loess", formula = y ~ x)
a1 <- ggplot(df5, aes(Cz_Tracheophyta_sr$observed_richness,Cz_Lichens_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Birds obs. rich.")) + geom_point() + stat_smooth(method = "gam", formula = y ~ x)

a1 <- ggplot(df6, aes(Cz_Fungi_sr$observed_richness,Cz_Lichens_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Birds obs. rich.")) + geom_point() + stat_smooth(method = "lm", formula = y ~ x)
a1 <- ggplot(df6, aes(Cz_Fungi_sr$observed_richness,Cz_Lichens_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Birds obs. rich.")) + geom_point() + stat_smooth(method = "loess", formula = y ~ x)
a1 <- ggplot(df6, aes(Cz_Fungi_sr$observed_richness,Cz_Lichens_sr$observed_richness), xlab = ("Beetles obs. rich."), ylab = ("Birds obs. rich.")) + geom_point() + stat_smooth(method = "gam", formula = y ~ x)

lm_bt <- lm(df6)
summary(lm_bt)

# Load GGally
library(GGally)

ggpairs(df) # to costumise https://stackoverflow.com/questions/55657386/how-to-display-coloured-group-correlations-with-scale-colour-manual-in-ggpairs

# Define custom color palette using colors from the volcano palette
volcano_palette <- c("#162A59", "#2F4F4F", "#A9A9A9", "#F5DEB3", "#CD853F", "#8B0000")

# small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

pm <- ggpairs(df, columns = 2:6, ggplot2::aes(colour = siteID))
p_(pm)

# Libraries
install.packages("hrbrthemes")
install.packages("viridis")

library(tidyverse)
library(hrbrthemes)
library(viridis)


df %>%
  ggplot( aes(x=siteID, y=Birds, fill=siteID)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Birds boxplot per site with jitter") +
  ylab("Number of species")+
  xlab("")


df %>%
  ggplot( aes(x=siteID, y=Beetles, fill=siteID)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Beetles boxplot per site with jitter") +
  ylab("Number of species")+
  xlab("")


df %>%
  ggplot( aes(x=siteID, y=Fungi, fill=siteID)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Fungi boxplot per site with jitter") +
  ylab("Number of species")+
  xlab("")


df %>%
  ggplot( aes(x=siteID, y=Lichens, fill=siteID)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Lichenss boxplot per site with jitter") +
  ylab("Number of species")+
  xlab("")

df %>%
  ggplot( aes(x=siteID, y=Tracheophyta, fill=siteID)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Tracheophyta boxplot per site with jitter") +
  ylab("Number of species")+
  xlab("")

speciesrichnessdata_95_CZ %>%
  ggplot( aes(x=taxon, y=observed_richness, fill=siteID)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Multi-taxa richness boxplot per site with jitter") +
  ylab("Number of species")+
  xlab("")

# Violin basic

speciesrichnessdata_95_CZ  %>%
  ggplot( aes(x=taxon, y=observed_richness, fill=siteID)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart on CZ multi-taxon biodiversity distribution") +
  ylab("Number of species")+
  xlab("")

df  %>%
  ggplot( aes(x=siteID, y=Birds, fill=siteID)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart on CZ aves biodiversity distribution") +
  ylab("Number of species")+
  xlab("")


df  %>%
  ggplot( aes(x=siteID, y=Beetles, fill=siteID)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart on CZ Saproxilic Coleoptera biodiversity distribution") +
  ylab("Number of species")+
  xlab("")


df  %>%
  ggplot( aes(x=siteID, y=Fungi, fill=siteID)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart on CZ fungi biodiversity distribution") +
  ylab("Number of species")+
  xlab("")


df  %>%
  ggplot( aes(x=siteID, y=Lichens, fill=siteID)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart on CZ Lichens biodiversity distribution") +
  ylab("Number of species")+
  xlab("")


df  %>%
  ggplot( aes(x=siteID, y=Tracheophyta, fill=siteID)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart on CZ Tracheophyta biodiversity distribution") +
  ylab("Number of species")+
  xlab("")

Cz_Bryophytes_sr %>%
  ggplot( aes(x=siteID, y=observed_richness, fill=siteID)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart on CZ Tracheophyta biodiversity distribution") +
  ylab("Number of species")+
  xlab("")

#-------------------------------------------------------------------------------
# Propose the same scheme but for the forest ecosystem below to the category 6 and 7 Beech forests

# Select only the plots in the category beech forests
beech_forest_6_SR <- speciesrichnessdata_95_CZ[speciesrichnessdata_95_CZ$keyID %in% target_cat6, ]
beech_forest_7_SR <- speciesrichnessdata_95_CZ[speciesrichnessdata_95_CZ$keyID %in% target_cat7, ]
beech_forest_67_SR <- speciesrichnessdata_95_CZ[speciesrichnessdata_95_CZ$keyID %in% target_cat67, ]

# filter per taxa and forest category 6
Cz_Birds_sr_6 <-  beech_forest_6_SR %>% filter(taxon == "Birds")
Cz_Beetles_sr_6 <-  beech_forest_6_SR %>% filter(taxon == "Beetles")
Cz_Fungi_sr_6 <-  beech_forest_6_SR %>% filter(taxon == "Fungi")
Cz_Lichens_sr_6 <-  beech_forest_6_SR %>% filter(taxon == "Lichens")
Cz_Tracheophyta_sr_6 <-  beech_forest_6_SR %>% filter(taxon == "Tracheophyta")

# filter per taxa and forest category 7
Cz_Birds_sr_7 <-  beech_forest_7_SR %>% filter(taxon == "Birds")
Cz_Beetles_sr_7 <-  beech_forest_7_SR %>% filter(taxon == "Beetles")
Cz_Fungi_sr_7 <-  beech_forest_7_SR %>% filter(taxon == "Fungi")
Cz_Lichens_sr_7 <-  beech_forest_7_SR %>% filter(taxon == "Lichens")
Cz_Tracheophyta_sr_7 <-  beech_forest_7_SR %>% filter(taxon == "Tracheophyta")

# filter per taxa and forest category 7
Cz_Birds_sr_67 <-  beech_forest_67_SR %>% filter(taxon == "Birds")
Cz_Beetles_sr_67 <-  beech_forest_67_SR %>% filter(taxon == "Beetles")
Cz_Fungi_sr_67 <-  beech_forest_67_SR %>% filter(taxon == "Fungi")
Cz_Lichens_sr_67 <-  beech_forest_67_SR %>% filter(taxon == "Lichens")
Cz_Tracheophyta_sr_67 <-  beech_forest_67_SR %>% filter(taxon == "Tracheophyta")

#-------------------------------------------------------------------------------
df_cat6 <- data.frame(Cz_Birds_sr_6[,4], Cz_Beetles_sr_6[,4], Cz_Fungi_sr_6[,4],Cz_Lichens_sr_6[,4], Cz_Tracheophyta_sr_6[,4])
names(df_cat6) <- c("Birds","Beetles","Fungi","Lichens","Tracheophyta")

df_cat6_var <- data.frame(Cz_Birds_sr_6[,8], Cz_Birds_sr_6[,4], Cz_Beetles_sr_6[,4], Cz_Fungi_sr_6[,4],Cz_Lichens_sr_6[,4], Cz_Tracheophyta_sr_6[,4])
names(df_cat6_var) <- c("siteID","Birds","Beetles","Fungi","Lichens","Tracheophyta")

df_cat7 <- data.frame(Cz_Birds_sr_7[,4], Cz_Beetles_sr_7[,4], Cz_Fungi_sr_7[,4],Cz_Lichens_sr_7[,4], Cz_Tracheophyta_sr_7[,4])
names(df_cat7) <- c("Birds","Beetles","Fungi","Lichens","Tracheophyta")

df_cat7_var <- data.frame(Cz_Birds_sr_7[,3], Cz_Birds_sr_7[,4], Cz_Beetles_sr_7[,4], Cz_Fungi_sr_7[,4],Cz_Lichens_sr_7[,4], Cz_Tracheophyta_sr_7[,4])
names(df_cat7_var) <- c("siteID","Birds","Beetles","Fungi","Lichens","Tracheophyta")

df_cat67 <- data.frame(Cz_Birds_sr_67[,4], Cz_Beetles_sr_67[,4], Cz_Fungi_sr_67[,4],Cz_Lichens_sr_67[,4], Cz_Tracheophyta_sr_67[,4])
names(df_cat67) <- c("Birds","Beetles","Fungi","Lichens","Tracheophyta")

df_cat67_var <- data.frame(Cz_Birds_sr_67[,3], Cz_Birds_sr_67[,4], Cz_Beetles_sr_67[,4], Cz_Fungi_sr_67[,4],Cz_Lichens_sr_67[,4], Cz_Tracheophyta_sr_67[,4])
names(df_cat67_var) <- c("siteID","Birds","Beetles","Fungi","Lichens","Tracheophyta")

#-------------------------------------------------------------------------------
# GGPAIRS analysis of Pearson's Correlation Matrix

ggpairs(df_cat6) # to constumization https://stackoverflow.com/questions/55657386/how-to-display-coloured-group-correlations-with-scale-colour-manual-in-ggpairs
ggpairs(df_cat7)
ggpairs(df_cat67)

# small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

# ERROR BECAUSE NOT ENOUGHT PLOTS PER VERY SITES
pm <- ggpairs(df_cat7_var, columns = 2:6, ggplot2::aes(colour = siteID))
p_(pm)


###################################
# DATAFRAME WITH SCALED RICHNESS see lines 229

df_sr_scaled <- data.frame(Cz_Birds_sr[,2], Cz_Birds_sr[,6], Cz_Beetles_sr[,6], Cz_Fungi_sr[,6],Cz_Lichens_sr[,6], Cz_Tracheophyta_sr[,6])
names(df_sr_scaled) <- c("siteID","Birds","Beetles","Fungi","Lichens","Tracheophyta")


# small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

pm <- ggpairs(df_sr_scaled, columns = 2:6, ggplot2::aes(colour = siteID))

# labs(x = "add this line", y = "to write the labels")

p_(pm)


#-------------------------------------------------------------------------------
# DATAFRAME WITH DEADWOOD AND CARBON POOLS see lines 229 - 236

# OBSERVED RICHNESS
df_sr_c <- data.frame(Cz_Birds_sr_c[,2], 
                      Cz_Birds_sr_c[,4], 
                      Cz_Beetles_sr_c[,4], 
                      Cz_Fungi_sr_c[,4],
                      Cz_Lichens_sr_c[,4], 
                      Cz_Tracheophyta_sr_c[,4], 
                      C_stock_total_CZ[,2],
                      C_stock_total_CZ[,3],
                      C_stock_total_CZ[,4])

names(df_sr_c) <- c("siteID","Birds","Beetles","Fungi","Lichens","Tracheophyta","CSSA","CSLD","CSSD")

# SCALED RICHNESS
df_sr_scaled_c <- data.frame(Cz_Birds_sr_c[,2], 
                             Cz_Birds_sr_c[,6], 
                             Cz_Beetles_sr_c[,6], 
                             Cz_Fungi_sr_c[,6],
                             Cz_Lichens_sr_c[,6], 
                             Cz_Tracheophyta_sr_c[,6],
                             C_stock_total_CZ[,2],
                             C_stock_total_CZ[,3],
                             C_stock_total_CZ[,4])

names(df_sr_scaled_c) <- c("siteID","Birds","Beetles","Fungi","Lichens","Tracheophyta","CSSA","CSLD","CSSD")

#-------------------------------------------------------------------------------
# small function to display plots only if it is interactive
p_ <- GGally::print_if_interactive

pm <- ggpairs(df_sr_c, columns = 2:9, ggplot2::aes(colour = siteID))
p_(pm)

ggpairs(df_sr_c) # Use it to plot also box plots

# Create a scatter plot matrix
ggpairs(df_sr_c, aes(colour = siteID)) # the same of above but with colors

#-------------------------------------------------------------------------------
# small function to display plots only if it is interactive
p_ <- GGally::print_if_interactive

pm <- ggpairs(df_sr_scaled_c, columns = 2:9, ggplot2::aes(colour = siteID))
p_(pm)

ggpairs(df_sr_scaled_c) # Use it to plot also box plots

# Create a scatter plot matrix
ggpairs(df_sr_scaled_c, aes(colour = siteID)) # the same of above but with colors


#-------------------------------------------------------------------------------
# Simple possible example
library(GGally)

# Create a scatter plot matrix
ggpairs(iris, aes(colour = species))

# Add x and y labels
ggpairs(iris, aes(colour = Species)) +
  labs(x = "Sepal Length", y = "Petal Length")

#-------------------------------------------------------------------------------
# Same procedure of line 229 but for bryophytes instead of carbon stocks

diff2 <- setdiff(Cz_Fungi_sr_c$keyID, Cz_Bryophytes_sr$keyID)
diff2

# "CZ_JH1_L1XL1_10" "CZ_JH1_L1XL1_43" "CZ_JH1_L5XL5_25" "CZ_JH1_L5XL5_28"
# "CZ_JH1_L5XL5_37" "CZ_JH1_L6XL6_11" "CZ_JH1_L6XL6_17"

# Remove rows in taxon species richness in plots where are not present carbon stocks outputs

Cz_Birds_sr_c_br <- Cz_Birds_sr_c %>% 
  filter(! (keyID %in% c("CZ_JH1_L1XL1_10", "CZ_JH1_L1XL1_43", "CZ_JH1_L5XL5_25", "CZ_JH1_L5XL5_28", "CZ_JH1_L5XL5_37", "CZ_JH1_L6XL6_11", "CZ_JH1_L6XL6_17")))

Cz_Beetles_sr_c_br <- Cz_Beetles_sr_c %>% 
  filter(! (keyID %in% c("CZ_JH1_L1XL1_10", "CZ_JH1_L1XL1_43", "CZ_JH1_L5XL5_25", "CZ_JH1_L5XL5_28", "CZ_JH1_L5XL5_37", "CZ_JH1_L6XL6_11", "CZ_JH1_L6XL6_17")))

Cz_Fungi_sr_c_br <- Cz_Fungi_sr_c %>% 
  filter(! (keyID %in% c("CZ_JH1_L1XL1_10", "CZ_JH1_L1XL1_43", "CZ_JH1_L5XL5_25", "CZ_JH1_L5XL5_28", "CZ_JH1_L5XL5_37", "CZ_JH1_L6XL6_11", "CZ_JH1_L6XL6_17")))

Cz_Lichens_sr_c_br <- Cz_Lichens_sr_c %>% 
  filter(! (keyID %in% c("CZ_JH1_L1XL1_10", "CZ_JH1_L1XL1_43", "CZ_JH1_L5XL5_25", "CZ_JH1_L5XL5_28", "CZ_JH1_L5XL5_37", "CZ_JH1_L6XL6_11", "CZ_JH1_L6XL6_17")))

Cz_Tracheophyta_sr_c_br <- Cz_Tracheophyta_sr_c %>% 
  filter(! (keyID %in% c("CZ_JH1_L1XL1_10", "CZ_JH1_L1XL1_43", "CZ_JH1_L5XL5_25", "CZ_JH1_L5XL5_28", "CZ_JH1_L5XL5_37", "CZ_JH1_L6XL6_11", "CZ_JH1_L6XL6_17")))

C_stock_total_CZ_br <- C_stock_total_CZ %>%
  filter(! (keyID %in% c("CZ_JH1_L1XL1_10", "CZ_JH1_L1XL1_43", "CZ_JH1_L5XL5_25", "CZ_JH1_L5XL5_28", "CZ_JH1_L5XL5_37", "CZ_JH1_L6XL6_11", "CZ_JH1_L6XL6_17")))

# Make Bryophytes working
diff2 <- setdiff(Cz_Bryophytes_sr$keyID, Cz_Fungi_sr_c_br$keyID)
diff2

# "CZ_JH1_L2XL2_04" "CZ_JH1_L2XL2_21" "CZ_JH1_L3XL3_33"

Cz_Bryophytes_sr_c_br <- Cz_Bryophytes_sr %>% 
  filter(! (keyID %in% c("CZ_JH1_L2XL2_04", "CZ_JH1_L2XL2_21", "CZ_JH1_L3XL3_33")))

#-------------------------------------------------------------------------------
# Make the new df for bryophytes matching

# OBSERVED RICHNESS
df_sr_c_br <- data.frame(Cz_Birds_sr_c_br[,2], 
                      Cz_Birds_sr_c_br[,4], 
                      Cz_Beetles_sr_c_br[,4], 
                      Cz_Fungi_sr_c_br[,4],
                      Cz_Lichens_sr_c_br[,4],
                      Cz_Bryophytes_sr_c_br[,4],
                      Cz_Tracheophyta_sr_c_br[,4], 
                      C_stock_total_CZ_br[,2],
                      C_stock_total_CZ_br[,3],
                      C_stock_total_CZ_br[,4])

names(df_sr_c_br) <- c("siteID","Birds","Beetles","Fungi","Lichens","Bryophytes","Tracheophyta","CSSA","CSLD","CSSD")

# SCALED RICHNESS
df_sr_scaled_c_br <- data.frame(Cz_Birds_sr_c_br[,2], 
                             Cz_Birds_sr_c_br[,6], 
                             Cz_Beetles_sr_c_br[,6], 
                             Cz_Fungi_sr_c_br[,6],
                             Cz_Lichens_sr_c_br[,6],
                             Cz_Bryophytes_sr_c_br[,6],
                             Cz_Tracheophyta_sr_c_br[,6],
                             C_stock_total_CZ_br[,2],
                             C_stock_total_CZ_br[,3],
                             C_stock_total_CZ_br[,4])

names(df_sr_scaled_c_br) <- c("siteID","Birds","Beetles","Fungi","Lichens","Bryophytes","Tracheophyta","CSSA","CSLD","CSSD")

#-------------------------------------------------------------------------------
# small function to display plots only if it is interactive
p_ <- GGally::print_if_interactive

pm <- ggpairs(df_sr_c_br, columns = 2:10, ggplot2::aes(colour = siteID))
p_(pm)

ggpairs(df_sr_c_br) # Use it to plot also box plots

# Create a scatter plot matrix
ggpairs(df_sr_c_br, aes(colour = siteID)) # the same of above but with colors

#-------------------------------------------------------------------------------
# small function to display plots only if it is interactive
p_ <- GGally::print_if_interactive

pm <- ggpairs(df_sr_scaled_c_br, columns = 2:10, ggplot2::aes(colour = siteID))
p_(pm)

ggpairs(df_sr_scaled_c_br) # Use it to plot also box plots

# Create a scatter plot matrix
ggpairs(df_sr_scaled_c_br, aes(colour = siteID)) # the same of above but with colors


#-------------------------------------------------------------------------------
# Same procedure of line 177 but for vol, dbh and age instead of carbon stocks

diff3 <- setdiff(volume_m3$keyID, Cz_Bryophytes_sr_c_br$keyID)
diff3

# "CZ_JH1_L1XL1_10" "CZ_JH1_L1XL1_43" "CZ_JH1_L5XL5_25" "CZ_JH1_L5XL5_28"
# "CZ_JH1_L5XL5_37" "CZ_JH1_L6XL6_11" "CZ_JH1_L6XL6_17"

# Remove rows in taxon species richness in plots where are not present carbon stocks outputs

volume_m3_str <- volume_m3 %>% 
  filter(! (keyID %in% c("CZ_JH1_L1XL1_02", "CZ_JH1_L1XL1_09", "CZ_JH1_L2XL2_04", "CZ_JH1_L2XL2_21", "CZ_JH1_L3XL3_33", "CZ_JH1_L3XL3_36", "CZ_JH1_L5XL5_04", "CZ_JH1_L1XL1_10", "CZ_JH1_L1XL1_43", "CZ_JH1_L5XL5_25", "CZ_JH1_L5XL5_28", "CZ_JH1_L5XL5_37", "CZ_JH1_L6XL6_11", "CZ_JH1_L6XL6_17")))

mean_dbh_cm_str <- mean_dbh_cm %>% 
  filter(! (keyID %in% c("CZ_JH1_L1XL1_02", "CZ_JH1_L1XL1_09", "CZ_JH1_L2XL2_04", "CZ_JH1_L2XL2_21", "CZ_JH1_L3XL3_33", "CZ_JH1_L3XL3_36", "CZ_JH1_L5XL5_04", "CZ_JH1_L1XL1_10", "CZ_JH1_L1XL1_43", "CZ_JH1_L5XL5_25", "CZ_JH1_L5XL5_28", "CZ_JH1_L5XL5_37", "CZ_JH1_L6XL6_11", "CZ_JH1_L6XL6_17")))

age_str <- age %>% 
  filter(! (plotdescdata_CZ.keyID %in% c("CZ_JH1_L1XL1_02", "CZ_JH1_L1XL1_09", "CZ_JH1_L2XL2_04", "CZ_JH1_L2XL2_21", "CZ_JH1_L3XL3_33", "CZ_JH1_L3XL3_36", "CZ_JH1_L5XL5_04","CZ_JH1_L1XL1_10", "CZ_JH1_L1XL1_43", "CZ_JH1_L5XL5_25", "CZ_JH1_L5XL5_28", "CZ_JH1_L5XL5_37", "CZ_JH1_L6XL6_11", "CZ_JH1_L6XL6_17")))

#-------------------------------------------------------------------------------
# Make the new df for bryophytes matching

# OBSERVED RICHNESS
df_sr_c_br_str <- data.frame(Cz_Birds_sr_c_br[,2], 
                         Cz_Birds_sr_c_br[,4], 
                         Cz_Beetles_sr_c_br[,4], 
                         Cz_Fungi_sr_c_br[,4],
                         Cz_Lichens_sr_c_br[,4],
                         Cz_Bryophytes_sr_c_br[,4],
                         Cz_Tracheophyta_sr_c_br[,4], 
                         C_stock_total_CZ_br[,2],
                         C_stock_total_CZ_br[,3],
                         C_stock_total_CZ_br[,4],
                         volume_m3_str[,2],
                         mean_dbh_cm_str[,2],
                         age_str[,2])

names(df_sr_c_br_str) <- c("siteID","Birds","Beetles","Fungi","Lichens","Bryophytes","Tracheophyta","CSSA","CSLD","CSSD", "Volume [m3]", "Mean DBH", "Stand Age")

# SCALED RICHNESS
df_sr_scaled_c_br_str <- data.frame(Cz_Birds_sr_c_br[,2], 
                                Cz_Birds_sr_c_br[,6], 
                                Cz_Beetles_sr_c_br[,6], 
                                Cz_Fungi_sr_c_br[,6],
                                Cz_Lichens_sr_c_br[,6],
                                Cz_Bryophytes_sr_c_br[,6],
                                Cz_Tracheophyta_sr_c_br[,6],
                                C_stock_total_CZ_br[,2],
                                C_stock_total_CZ_br[,3],
                                C_stock_total_CZ_br[,4],
                                volume_m3_str[,2],
                                mean_dbh_cm_str[,2],
                                age_str[,2])

names(df_sr_scaled_c_br_str) <- c("siteID","Birds","Beetles","Fungi","Lichens","Bryophytes","Tracheophyta","CSSA","CSLD","CSSD","Volume [m3]", "Mean DBH", "Stand Age")

#-------------------------------------------------------------------------------
# small function to display plots only if it is interactive
p_ <- GGally::print_if_interactive

pm <- ggpairs(df_sr_c_br_str, columns = 2:13, ggplot2::aes(colour = siteID))
p_(pm)

ggpairs(df_sr_c_br_str) # Use it to plot also box plots

# Create a scatter plot matrix
ggpairs(df_sr_c_br_str, aes(colour = siteID)) # the same of above but with colors

#-------------------------------------------------------------------------------
# small function to display plots only if it is interactive
p_ <- GGally::print_if_interactive

pm <- ggpairs(df_sr_scaled_c_br_str, columns = 2:13, ggplot2::aes(colour = siteID))
p_(pm)

ggpairs(df_sr_scaled_c_br_str) # Use it to plot also box plots

# Create a scatter plot matrix
ggpairs(df_sr_scaled_c_br_str, aes(colour = siteID)) # the same of above but with colors


#-------------------------------------------------------------------------------
# DATAFRAME needed for the Climate request

# CLIMATE REQUEST DATAFRAME
CLIM_DATA_REQUEST <- datasoil_CZ[,1:6]

# Save the new data frame as a CSV file with a similar name to the original
path <- paste0(dataroot, "CLIM_DATA_REQUEST.csv")
write.csv(CLIM_DATA_REQUEST, path)

#-------------------------------------------------------------------------------
# Continue te stat analysis

library(ggplot2)
library(GGally)

a1 <- ggplot(df_sr_c_br_str, aes(x= Birds,y= `Stand Age`), xlab = ("Birds obs. rich.k"), ylab = ("Stand Age obs. rich.")) + geom_point() + stat_smooth(method = "lm", formula = y ~ x)
a1

# BIRDS AND BEETLES
ggplot(df_sr_c_br_str, aes(x = Birds, y = Beetles, color = siteID)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Birds obs. rich. [alpha div.]", y = "Saproxilic Beetle [alpha div.]")

# BIRDS AND LICHENS
ggplot(df_sr_c_br_str, aes(x = Birds, y = Lichens, color = siteID)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Birds obs. rich. [alpha div.]", y = "Lichens [alpha div.]")

# BIRDS AND Stand Age
ggplot(df_sr_c_br_str, aes(x = Birds, y = `Stand Age`, color = siteID)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Birds obs. rich. [alpha div.]", y = "Stand Age [year]")

# BIRDS AND STAND AGE
plot(df_sr_c_br_str$Birds, df_sr_c_br_str$`Stand Age`)
x <- lm(df_sr_c_br_str$Birds ~ df_sr_c_br_str$`Stand Age`)
summary(x)

# BIRDS AND LICHENS
ggplot(df_sr_c_br_str, aes(x = Birds, y = Lichens, color = siteID)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Birds obs. rich. [alpha div.]", y = "Lichens [alpha div.]")

# BIRDS AND Tracheophyta
ggplot(df_sr_c_br_str, aes(x = Birds, y = Tracheophyta, color = siteID)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Birds obs. rich. [alpha div.]", y = "Tracheophyta [alpha div.]")

# BIRDS AND Tracheophyta
ggplot(df_sr_c_br_str, aes(x = Birds, y = Tracheophyta, color = siteID)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Birds obs. rich. [alpha div.]", y = "Tracheophyta [alpha div.]")

# BEETLS AND Mean DBH
ggplot(df_sr_c_br_str, aes(x = Beetles, y = `Mean DBH`, color = siteID)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Beetles obs. rich. [alpha div.]", y = "Mean DBH [cm]")

# BEETLS AND Mean DBH
ggplot(df_sr_c_br_str, aes(x = Beetles, y = `Mean DBH`, color = siteID)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Beetles obs. rich. [alpha div.]", y = "Mean DBH [cm]")

# BIRDS AND STAND AGE
plot(df_sr_c_br_str$Beetles, df_sr_c_br_str$`Mean DBH`)
x <- lm(df_sr_c_br_str$Birds ~ df_sr_c_br_str$`Mean DBH`)
summary(x)

# FUNGI AND BRYOPHYTES
ggplot(df_sr_c_br_str, aes(x = Bryophytes, y = Fungi, color = siteID)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Bryophytes obs. rich. [alpha div.]", y = "Fungi obs. rich. [alpha div.]")

# FUNGI AND CSSA
ggplot(df_sr_c_br_str, aes(x = CSSA, y = Fungi, color = siteID)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Carbon Stock Stand Alive [kg]", y = "Fungi obs. rich. [alpha div.]")

# FUNGI AND CSSA
ggplot(df_sr_c_br_str, aes(x = CSSA, y = Fungi, color = siteID)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Carbon Stock Stand Alive [kg]", y = "Fungi obs. rich. [alpha div.]")

# FUNGI AND CSLD
ggplot(df_sr_c_br_str, aes(x = CSLD, y = Fungi)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Carbon Stock Lying Dead [kg]", y = "Fungi obs. rich. [alpha div.]")

# FUNGI AND CSLD
ggplot(df_sr_c_br_str, aes(x = CSLD, y = Fungi)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Carbon Stock Lying Dead [kg]", y = "Fungi obs. rich. [alpha div.]")

# FUNGI AND CSSD
ggplot(df_sr_c_br_str, aes(x = CSLD, y = Fungi, color = siteID)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Carbon Stock Stand Dead [kg]", y = "Fungi obs. rich. [alpha div.]")

# BIRDS AND STAND AGE
plot(df_sr_c_br_str$Fungi, df_sr_c_br_str$CSLD)
x <- lm(df_sr_c_br_str$Fungi ~ df_sr_c_br_str$CSLD)
summary(x)

# FUNGI AND CSLD
ggplot(df_sr_c_br_str, aes(x = CSLD, y = Fungi, color = siteID)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Carbon Lying Stand Dead [kg]", y = "Fungi obs. rich. [alpha div.]")

# FUNGI and CSSD but different fitting

#1
ggplot(df_sr_c_br_str, aes(x = CSLD, y = Fungi)) +
  geom_point(aes(color = siteID)) +
  stat_smooth(data = filter(df_sr_c_br_str, !is.na(Fungi)), method = "loess", formula = y ~ x, se = TRUE) +
  labs(x = "Carbon Lying Stand Dead [kg]", y = "Fungi obs. rich. [alpha div.]")

#2
ggplot(df_sr_c_br_str, aes(x = CSLD, y = Fungi)) +
  geom_point(aes(color = siteID)) +
  stat_smooth(data = filter(df_sr_c_br_str, !is.na(Fungi)), method = "loess", formula = y ~ x, se = FALSE) + # in this case is not plotting the variance
  labs(x = "Carbon Lying Stand Dead [kg]", y = "Fungi obs. rich. [alpha div.]")

# 2b This can be used to: In this updated code, I've used stat_smooth with the loess method. The span parameter controls the degree of smoothing, and I've set it to 1/3 to approximate 3 degrees of freedom. Additionally, I've included degree = 1 to fit a linear (first-degree) polynomial for the lowess smoother.Please note that the degree argument is specific to the loess method and does not represent the number of degrees of freedom in the same sense as other smoothing methods. Adjusting the span parameter is more appropriate for controlling the effective degrees of freedom in a lowess smoother.

ggplot(df_sr_c_br_str, aes(x = CSLD, y = Fungi)) +
  geom_point(aes(color = siteID)) +
  stat_smooth(data = filter(df_sr_c_br_str, !is.na(Fungi)), method = "loess", formula = y ~ x, se = TRUE, span = 1/3, degree = 1) +
  labs(x = "Carbon Lying Stand Dead [kg]", y = "Fungi obs. rich. [alpha div.]")

#3
ggplot(df_sr_c_br_str, aes(x = CSLD, y = Fungi)) +
  geom_point(aes(color = siteID)) +
  stat_smooth(data = filter(df_sr_c_br_str, !is.na(Fungi)), method = "lm", formula = y ~ x, se = TRUE) + # using Linear regression line (method = "lm"): This method fits a straight line to the data points using linear regression. It is suitable for exploring linear relationships between variables.
  labs(x = "Carbon Lying Stand Dead [kg]", y = "Fungi obs. rich. [alpha div.]")

#4
ggplot(df_sr_c_br_str, aes(x = CSLD, y = Fungi)) +
  geom_point(aes(color = siteID)) +
  stat_smooth(data = filter(df_sr_c_br_str, !is.na(Fungi)), method = "", formula = y ~ x, se = TRUE) + # using Lowess smoother (method = "lowess" or method = "smooth"): This method fits a smooth curve to the data points using locally weighted scatterplot smoothing. It is useful for identifying non-linear relationships.
  labs(x = "Carbon Lying Stand Dead [kg]", y = "Fungi obs. rich. [alpha div.]")

#5
ggplot(df_sr_c_br_str, aes(x = CSLD, y = Fungi)) +
  geom_point(aes(color = siteID)) +
  stat_smooth(data = filter(df_sr_c_br_str, !is.na(Fungi)), method = "lm", formula = y ~ poly(x, degree = 6), se = TRUE) + # using Polynomial regression (method = "lm" + geom_smooth(method = "lm", formula = y ~ poly(x, degree))): This approach fits a polynomial curve to the data points using polynomial regression. The degree of the polynomial can be adjusted to capture different levels of complexity.
  labs(x = "Carbon Lying Stand Dead [kg]", y = "Fungi obs. rich. [alpha div.]")

#6 GAM

library(ggplot2)
library(mgcv)

# Fit the GAM
gam_model <- gam(Fungi ~ s(CSLD, bs = "cr", k = 6), data = df_sr_c_br_str)

# Generate predictions from the GAM model
df_predictions <- data.frame(CSLD = seq(min(df_sr_c_br_str$CSLD), max(df_sr_c_br_str$CSLD), length.out = 100))
df_predictions$Fungi <- predict(gam_model, newdata = df_predictions)

# Create the scatter plot and overlay the smooth curve # 
ggplot(df_sr_c_br_str, aes(x = CSLD, y = Fungi)) +
  geom_point(aes(color = siteID)) +
  geom_line(data = df_predictions, aes(x = CSLD, y = Fungi), color = "blue") +
  labs(x = "Carbon Lying Stand Dead [kg]", y = "Fungi obs. rich. [alpha div.]")

# To fit a generalized additive model (GAM) with a cubic regression spline, you can use the gam function from the mgcv package and overlay the smooth curve on the scatter plot using geom_line
# In this code, we first fit the GAM model using the gam function and the formula Fungi ~ s(CSLD, bs = "cr", k = 6). Then, we generate predictions from the model using the predict function and create a data frame df_predictions to store the predictions for plotting purposes.
# We then create the scatter plot using geom_point and overlay the smooth curve using geom_line with the df_predictions data frame.

library(ggplot2)
library(mgcv)

# Fit the GAM
gam_model <- gam(Fungi ~ s(CSLD, bs = "cr", k = 6), data = df_sr_c_br_str)

# Generate predictions and confidence interval from the GAM model
df_predictions <- data.frame(CSLD = seq(min(df_sr_c_br_str$CSLD), max(df_sr_c_br_str$CSLD), length.out = 100))
df_predictions$Fungi <- predict(gam_model, newdata = df_predictions, se.fit = TRUE)$fit
df_predictions$se <- predict(gam_model, newdata = df_predictions, se.fit = TRUE)$se.fit
df_predictions$lower <- df_predictions$Fungi - 1.96 * df_predictions$se
df_predictions$upper <- df_predictions$Fungi + 1.96 * df_predictions$se

# Merge with the original data frame to include siteID
df_predictions <- merge(df_predictions, df_sr_c_br_str[, c("CSLD", "siteID")], by = "CSLD")

# Create the scatter plot, smooth curve, and variance/confidence interval
ggplot(df_sr_c_br_str, aes(x = CSLD, y = Fungi, color = siteID)) +
  geom_point() +
  geom_line(data = df_predictions, aes(x = CSLD, y = Fungi), color = "black") +
  geom_ribbon(data = df_predictions, aes(x = CSLD, ymin = lower, ymax = upper), fill = "gray", alpha = 0.3) +
  labs(x = "Carbon Stock Stand Dead [kg]", y = "Fungi obs. rich. [alpha div.]")



#--------------------------------------------------------------------------------
# BRYOPHYTES AND STAND AGE
ggplot(df_sr_c_br_str, aes(x = Bryophytes, y = `Stand Age`, color = siteID)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Bryophytes obs. rich. [alpha div.]", y = "Satnd Age [year]")

# BRYOPHITES AND LICHENS
ggplot(df_sr_c_br_str, aes(x = Bryophytes, y = Lichens, color = siteID)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Bryophytes obs. rich. [alpha div.]", y = "Lichens obs. rich. [alpha div.]")

# BRYOPHITES AND LICHENS
ggplot(df_sr_c_br_str, aes(x = Bryophytes, y = Lichens)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Bryophytes obs. rich. [alpha div.]", y = "Lichens obs. rich. [alpha div.]")

# BRYOPHITES AND LICHENS
ggplot(df_sr_c_br_str, aes(x = Lichens, y = CSSD, color = siteID)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Lichens obs. rich. [alpha div.]", y = "Carbon Stock Stand Deadwood")

# BRYOPHITES AND LICHENS
ggplot(df_sr_c_br_str, aes(x = Lichens, y = CSSD, color = siteID)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Lichens obs. rich. [alpha div.]", y = "Carbon Stock Stand Deadwood")

# BRYOPHITES AND LICHENS
ggplot(df_sr_c_br_str, aes(x = Lichens, y = `Stand Age`, color = siteID)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Lichens obs. rich. [alpha div.]", y = "Carbon Stock Stand Deadwood")

# BRYOPHITES AND LICHENS
ggplot(df_sr_c_br_str, aes(x = Lichens, y = `Stand Age`, color = siteID)) +
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x) +
  labs(x = "Lichens obs. rich. [alpha div.]", y = "Carbon Stock Stand Deadwood")

#
a1 <- ggplot(df_sr_c_br_str, aes(df_sr_c_br_str$Birds,df_sr_c_br_str$`Stand Age`), xlab = ("Beetles obs. rich."), ylab = ("Fungi obs. rich.")) + geom_point() + stat_smooth(method = "loess", formula = y ~ x)
a1

a1 <- ggplot(df_sr_c_br_str, aes(df_sr_c_br_str$Birds,df_sr_c_br_str$`Stand Age`), xlab = ("Beetles obs. rich."), ylab = ("Fungi obs. rich.")) + geom_point() + stat_smooth(method = "gam", formula = y ~ x)
a1

# BIRDS AND STAND AGE
plot(df_sr_c_br_str$Birds, df_sr_c_br_str$`Stand Age`)
x <- lm(df_sr_c_br_str$Birds ~ df_sr_c_br_str$`Stand Age`)
summary(x)

# BRYOPHYTES AND STAND AGE
plot(df_sr_c_br_str$Bryophytes, df_sr_c_br_str$`Stand Age`)
x <- lm(df_sr_c_br_str$Bryophytes ~ df_sr_c_br_str$`Stand Age`)
summary(x)

# BRYOPHYTES AND LICHENS
plot(df_sr_c_br_str$Bryophytes, df_sr_c_br_str$Lichens)
lines(c(0,), c(0,100))

x <- lm(df_sr_c_br_str$Bryophytes ~ df_sr_c_br_str$Lichens)
summary(x)



#-------------------------------------------------------------------------------------------------------
# remotes::install_github("phytoclast/vegnasis", dependencies = FALSE)
library(vegnasis)
library(ggplot2)


#Plot Plant Profiles
veg.raw <-  vegnasis::nasis.veg
veg <- clean.veg(veg.raw)
veg.select <- subset(veg,  grepl('2022MI165021.P',plot))
plants <- grow_plants(veg.select)


# Northern Michigan hardwood forest 
veg.select <- subset(veg,  grepl('2022MI165002.P',plot))
plants <- grow_plants(veg.select)

veg_profile_plot(plants)

# Transformed Y axis
veg_profile_plot(plants, 'sqrt', 5)


# Northern Michigan mixed forest
veg.select <- subset(veg,  grepl('2022MI165023.P',plot))
plants <- grow_plants(veg.select)

veg_profile_plot(plants, unit='m',  skycolor = 'white', fadecolor = 'lightgray', gridalpha = 0.1, groundcolor = 'darkgray')

# Northern Michigan pine forest (Override default tree colors and shapes.)

veg.select <- subset(veg,  grepl('2022MI165021.P',plot))

taxon <- c('Acer rubrum', 'Pinus resinosa')
crfill <- c(NA,"#80991A")
stfill <- c('gray',"#B36666")
crshape <- c(NA,'conifer2')
override <- data.frame(taxon=taxon,stfill=stfill,crfill=crfill,crshape=crshape)
veg.select <- veg.select |> left_join(override)

plants <- grow_plants(veg.select)

veg_profile_plot(plants) # Structure of northern Michigan pine forest. 

# Washington conifer forest

veg.select <- subset(veg,  grepl('2021WA031024',plot))
plants <- grow_plants(veg.select)
veg_profile_plot(plants, unit='m',  skycolor = 'white', fadecolor = 'lightgray', gridalpha = 0.1, groundcolor = 'darkgray') # Structure of a Washington conifer forest. 

# Generic oak savanna

# Many parameters can be adjusted such as making the plot longer or changing sky color. Add unknown deciduous shrub with silvery green leaves (e.i. Eleagnus without actually saying it) by specifying habit code “S.BD” and a crown fill color (find a the rgb color hex code that looks right).

#Make up savanna data

thiscolor = rgb(0.6,0.9,0.7)

plot <- c('plot1')
taxon <- c('Quercus macrocarpa','UNK','Pteridium', 'Festuca', 'Andropogon', 'Liatris')
cover <- c(20,5,10,60,10,5)
crown.max <- c(15,4,1,0.6,2,0.4)
crfill <- c(NA,thiscolor,NA,NA,NA,NA)
dbh <- c(60,NA,NA,NA,NA,NA)
habit <- c(NA,'S.BD',NA,NA,NA,NA)
mydata <- data.frame(plot=plot,taxon=taxon, cover=cover, habit=habit, crown.max = crown.max, dbh.max = dbh, crfill=crfill)

veg <- mydata |> pre.fill.veg()
plants <- grow_plants(veg, plength=100) #Grow more plants on a longer 100 m plot (default was 50 m).
veg_profile_plot(plants, unit='m',  skycolor = rgb(0.8,0.98,1), fadecolor = 'lightgray', gridalpha = 0.1, groundcolor = rgb(0.55,0.45,0.2), xlim=c(0,100))
