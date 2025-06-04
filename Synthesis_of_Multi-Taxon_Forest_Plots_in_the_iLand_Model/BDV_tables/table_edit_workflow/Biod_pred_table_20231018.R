# Marco Baldo 25-07-2023
# Create a table of predictors of multi-taxa biodiversity indicators in forest ecosystem of Czechia


# Jenik grid 

library(readxl)


# source data table:
t1<-read_excel("I:/iLand/2023/20230901_Bottoms_Up/plot_init/Jenik/final_table_imp/Bdv_predictors_v2.xlsx")
t2<-read_excel("I:/iLand/2023/20230901_Bottoms_Up/plot_init/Jenik/final_table_imp/Jenyk_BDV_predictors_ENG.xlsx")


excel_sheets(t1) # how many sheets are there and their name in the excel file f.
excel_sheets(t2)

# Continuing merging the plot outputs of the year zero

# Uploading all the database in one

# Load required libraries
library(tidyr)
library(dplyr)
library(RSQLite)

setwd("I:/iLand/2023/20230901_Bottoms_Up/outputs/20231002/output/")

# Path to the directory containing your SQLite databases
dataroot <- "I:/iLand/2023/20230901_Bottoms_Up/outputs/20231002/output/"

# Get a list of all SQLite databases in the directory
# database_files <- list.files(path = dataroot, pattern = ".sqlite", full.names = TRUE)

{# Create an empty list to store data frames
  dfs <- list() # not working for several subset, only one
}


# Create the list of dataframes I want to save

tree <- c()

stand<- c()

lnd<-c()

dys <- c()

carbon <- c()

carbonflow <- c()

bb <-c()

# damage.all<-c()

# landscape_removed <- c()

# management <- ()

variables.all <- c()

#-------------------------------------------------------------------------------
# import the list of files within the folder in dataroot with .sqlite extension

database_files <- list.files(path = dataroot, pattern = ".sqlite", full.names = FALSE)                              # alternative way to select all the databases within a folder select all the file with sqlite format
database_files <- list.files(dataroot, ".sqlite")  

for (i in (1:length(database_files)))  {    # We read in the files in the loop. The "i" is for the x from 1 to i lenght of the dataset of files 
  
  
  # i<-1 # to test but remember to don't run also the }
  
  
  # Name of the database
  file <-paste0(database_files[i])                                    # File to read here the case is always the actual case in the loop
  # "file"= name of the object, "paste0"+ function to create a NAME for a computer path of selection of data/objects
  
  # case<- strsplit(all_v[i],".s")[[1]][1]      # Why we used it in the paper analysis? # explanation of this function is needed
  case<-database_files[i] 
  
  
  # Control
  print(file)
  
  # connect to the database 
  sqlite.driver <- dbDriver("SQLite")
  db1 <- dbConnect(sqlite.driver, dbname = file)  # connect to the file
  tables.in.the.file<-dbListTables(db1)           # explore the tables in the file
  print(tables.in.the.file)
  
  
  #-----------------------------------------------------------------------------
  # LOAD THE DATABASE # Read tables from the database
  
  tree <- dbReadTable(db1, "tree")
  stand <- dbReadTable(db1, "stand")
  landscape <- dbReadTable(db1, "landscape")
  carbon <- dbReadTable(db1, "carbon")
  carbonflow <- dbReadTable(db1, "carbonflow")
  dynamicstand <- dbReadTable(db1, "dynamicstand")
  landscape_removed <- dbReadTable(db1, "landscape_removed")
  barkbeetle <- dbReadTable(db1, "barkbeetle")
  #management <- dbReadTable(con, "management")
  
  dbDisconnect(db1)    # close the file
  
  # TO UNDERSTAND THE OPERATORS %>% AND %IN% HAVE TO STUDY THEM IN DATACAMP AND IN DPLYR CRAN PACKAGES
  
  ab.lnd.v<- data.frame(landscape %>% 
                          group_by(year) %>% 
                          filter(year==0) %>%
                          summarise(tot_volume=sum(volume_m3),living_c=sum(total_carbon_kg), count_ha=sum(count_ha), tot_ba=sum(basal_area_m2),npp=sum(NPP_kg), LAI=sum(LAI), sapling=sum(cohort_count_ha),growth_m3=sum(gwl_m3)))
  
  
  
  
  dynamicstand_1 <-dynamicstand %>% filter(year==0)  
  
  # Collect landscape data FOR CREATE THE VARIABLE LND FOR ALL THE RUNS
  landscape<- (landscape %>% mutate(run=case))
  lnd<-rbind(lnd, landscape)
  
  
  # Collect dynamicstand data FOR CREATE THE VARIABLE WIND FOR ALL THE RUNS
  dynamicstand <-(dynamicstand %>% mutate(run=case)) # consider to put dynamicstand_1 for 300 rows
  dys <-rbind(dys, dynamicstand)
  
  
}

# Crete the LAI vector
LAI <- data.frame (lnd %>%
                     group_by(run) %>%
                     filter(year==0) %>%
                     summarise(tot_volume=sum(volume_m3),living_c=sum(total_carbon_kg), count_ha=sum(count_ha), tot_ba=sum(basal_area_m2),npp=sum(NPP_kg), LAI=sum(LAI), sapling=sum(cohort_count_ha),growth_m3=sum(gwl_m3)))

library(writexl)
write_xlsx (LAI, paste0(dataroot,"CZ_plot_lnd.xlsx"))




library(ggplot2)
library(GGally)
library(corrplot)


tab2 <- read_xlsx("I:/iLand/2023/20230901_Bottoms_Up/plot_init/Jenik/final_table_imp/Bdv_predictors_table_final_20231002.xlsx")

# head(tab1)
head(tab2)

plot(tab2$age, tab2$basal_area)

col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", "cyan", "#007FFF", "blue", "#00007F"))


#---------------------------------- just do the correlation plot all together
a.num<-tab2[,3:18]

# Look them all:
par(mfrow = c(1, 1), pty="m", mar=c(3,3,3,3), oma=c(0,0,0,0))
corrplot.mixed(cor(a.num),upper.col = col4(10),lower.col = "black", mar=c(0,0,0,0), tl.pos = "d")#, diag = "l")


#---------------------------------- just do the correlation plot with the selected variables

ggpairs(a.num)

