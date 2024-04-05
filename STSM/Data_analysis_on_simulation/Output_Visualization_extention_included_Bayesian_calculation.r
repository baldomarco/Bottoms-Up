# Marco Baldo 14-12-2023
# Create iLand simulation results from the Bottoms-Up model plots to analyze the multi-taxa biodiversity indicators in the forest ecosystems of Czechia
# Refer the results and visualization to the pdf within this same GitHub folder: OK_final_20231214_BDV_mng_plot_L1_10_300.pdf
# Computer path -> C:/iLand/2023/20230901_Bottoms_Up/outputs/20231129/OK_final_20231214_BDV_mng_plot_L1_10_300.pdf

# Jenik grid just to control if needed

library(readxl)


# source data table:
t1<-read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/Bdv_predictors_v2.xlsx")
t2<-read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/Jenyk_BDV_predictors_ENG.xlsx")


excel_sheets(t1) # how many sheets are there and their name in the excel file f.
excel_sheets(t2)

# Uploading all the database in one


# BDV predictors by Jenik
BDV_predictors <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/tables_for_stat/Bdv_predictors_table_BayesianMod_results_th_with_elevation_mng_DWC.xlsx")


# Load required libraries
library(tidyr)
library(dplyr)
library(RSQLite)
library(vegan)
library(fields)


#-------------------------------------------------------------------------------
# create a string of average c for iLand output
decstag <- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/DeadwoodDensityGlobal_Katka_corr.xlsx")

# Select and create avg from the table
average_density <- table %>%
  filter(Decay.class == "Average")

# Select and create avg from the table
average_density <- decstag %>%
  select(GenusGlobal, Density_kgm3, C_fraction, C_density_kgm3) %>%
  group_by(GenusGlobal) %>%
  mutate(
    Density_kgm3 = mean(Density_kgm3),
    C_fraction = mean(C_fraction),
    C_density_kgm3 = mean(C_density_kgm3))

average_density <- average_density %>%
  distinct(GenusGlobal, .keep_all = TRUE)

#-------------------------------------------------------------------------------

setwd("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/")

# Path to the directory containing your SQLite databases
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"

# Get a list of all SQLite databases in the directory
# database_files <- list.files(path = dataroot, pattern = ".sqlite", full.names = TRUE)

{# Create an empty list to store data frames
  dfs <- list() # not working for several subset, only one
}


# Create the list of dataframes I want to save

abeStand_scen <- c()

abeStandDetail_scen <- c()

abeStandRemoval_scen <- c()

abeUnit_scen <- c()

bb_scen <- c()

landscape_removed_scen <- c()

tree_scen <- c()

stand_scen <- c()

lnd_scen <-c()

dys_scen <- c()

carbon_scen <- c()

carbonflow_scen <- c()

removals <- c()

damage.all <- c()

H_BA_heterogenity_scen <- c()

landscape_removed_scen_natmor <- c()

# damage.all<-c()

# landscape_removed <- c()

# management <- ()

variables.all <- c()

plot_variables_all <- c()

Bayesian_BDV_model_V1 <- c()

Bayesian_BDV_model_V2 <- c()

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
  db <- dbConnect(sqlite.driver, dbname = file)  # connect to the file
  tables.in.the.file<-dbListTables(db)           # explore the tables in the file
  print(tables.in.the.file)
  
  
  #-----------------------------------------------------------------------------
  # LOAD THE DATABASE # Read tables from the database
  
  abeStand <- dbReadTable(db, "abeStand")
  abeStandDetail <- dbReadTable(db, "abeStandDetail")
  abeStandRemoval <- dbReadTable(db, "abeStandRemoval")
  abeUnit <- dbReadTable(db, "abeUnit")
  barkbeetle <- dbReadTable(db,"barkbeetle")
  carbon <- dbReadTable(db,"carbon")
  carbonflow <- dbReadTable(db, "carbonflow")
  dynamicstand <- dbReadTable(db, "dynamicstand")
  landscape <- dbReadTable(db,"landscape")
  landscape_removed <- dbReadTable(db,"landscape_removed")
  stand <- dbReadTable(db, "stand")
  tree <- dbReadTable(db, "tree")
  
  dbDisconnect(db)    # close the file
  
  # TO UNDERSTAND THE OPERATORS %>% AND %IN% HAVE TO STUDY THEM IN DATACAMP AND IN DPLYR CRAN PACKAGES
  
  #-----------------------------------------------------------------------------
  # CREATE SHANNON VARIABLE
  
  annual.data<-landscape %>% group_by(year) %>% filter(year>0) %>% summarize(VOL.tot=sum(volume_m3), BA.tot=sum(basal_area_m2), count.tot=sum(count_ha))
  annual.spec.data<-landscape %>% group_by(year, species) %>%  filter(year>0) %>% summarize(VOL=(volume_m3), BA=(basal_area_m2), count=sum(count_ha))
  
  print(head(annual.data))
  print(head(annual.spec.data))
  
  S<-landscape %>% group_by(year) %>% filter(volume_m3>0 & year>0) %>% summarise(n=n())   # number of species in each year  (added the filter to count non-zero volumes, now it is okay)
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
  plot(H.BA)
  plot(H.VOL)
  plot(H.count)
  
  #The higher the value of H, the higher the diversity of species in a particular community. 
  #The lower the value of H, the lower the diversity. 
  #A value of H = 0 indicates a community that only has one species.
  
  
  # Shannon Equitability Index (SEI).... maybe we do not need this at all.....
  SEI<-H.BA$H/log(S$n)
  SEI[which(is.na(SEI)==T)]<-0
  
  # Pioneer species proportion based on BA
  
  earlyspecs<-c("lade","acps","frex","cabe","bepe","alin","algl","acca","acpl","soau","soar","coav","alvi","potr","poni","ulgl","saca","rops")
  
  early.spec.prop<-t %>% filter(species %in% earlyspecs) %>% summarize(BA=sum(prop.BA))
  
  esp<-data.frame(year=c(1:max(landscape$year)))
  esp<-esp %>% left_join(early.spec.prop, by="year")
  esp$BA[which(is.na(esp$BA)==T)]<-0
  
  #-----------------------------------------------------------------------------
  # Create Shannon on BA heterogeneity
  
  # Calculate the Shannon diversity index
  H_BA_heterogenity <- tree %>%
    group_by(year, species) %>%
    summarize(shannon_ba_heterog = diversity(basalArea, base = exp(1)))
  
  # Print the resulting dataframe
  print(H_BA_heterogenity)
  
  #-----------------------------------------------------------------------------
  # CREATE THE DATA FRAME FOR ADD VARIABLES ABOUT CARBON IN THE FINAL DATA FRAME
  
  ab.tot.c<- data.frame(carbon %>% 
                          group_by(year) %>% 
                          summarise(tot_carbon=sum(stem_c, branch_c, foliage_c, coarseRoot_c, fineRoot_c, regeneration_c, snags_c, snagsOther_c, downedWood_c, litter_c, soil_c)))
  
  
  #-----------------------------------------------------------------------------
  # CREATE A NEW DATA FRAME TO WORK ON THE SPECIFIC VARIABLES NEEDED 
  
  head(landscape)
  head(dynamicstand)
  
  # TO UNDERSTAND THE OPERATORS %>% AND %IN% HAVE TO STUDY THEM IN DATACAMP AND IN DPLER CRAN PACKAGES
  
  ab.lnd.v<- data.frame(landscape %>% 
                          group_by(year) %>% 
                          filter(year>0) %>%
                          summarise(tot_volume=sum(volume_m3),living_c=sum(total_carbon_kg), count_ha=sum(count_ha), tot_ba=sum(basal_area_m2),npp=sum(NPP_kg), LAI=sum(LAI), sapling=sum(cohort_count_ha),growth_m3=sum(gwl_m3)))
  
  
  dynamicstand_1 <-dynamicstand %>% filter(year>0)                            # THIS IS NEEDED FOR MAKE THE DATA FRAME dynamicstand OF THE SAME SIZE OF THE carbon AND carbonflow. WE DID THE SAME FOR THE SELECTED VARIABLES IN LANDSCAPE
  
  # CREATE THE NEW DATA FRAME FOR VARIABLES 
  
  variables <- data.frame(case=case,
                          year=dynamicstand_1$year,
                          h=dynamicstand_1$height_mean,
                          dbh=dynamicstand_1$dbh_mean,
                          age=dynamicstand_1$age_mean,
                          Rh=carbonflow$Rh,
                          NPP=carbonflow$NPP,
                          GPP=carbonflow$GPP,
                          NEP=carbonflow$NEP,
                          H.count=H.count$H,
                          H.VOL=H.VOL$H,
                          H.BA=H.BA$H,
                          SEI=SEI,
                          esp_BA_prop=esp$BA)
  
  # ADD LANDSCAPE VARIABLES AT V DATA FRAME
  
  variables = inner_join(variables, ab.lnd.v, by="year")
  variables = inner_join(variables, ab.tot.c, by="year")
  
  # variables[which(is.na(damage$wind)==TRUE)] <-0                              # FOR MAKE THE na = 0 !!!! "
  
  head(variables)
  
#-------------------------------------------------------------------------------
# Create tables for all variables of plot bdv predictors
# Create the LAI time series
  
  LAI <- landscape %>% 
    group_by(year) %>%
    summarise(LAI = sum(LAI))%>%
    select(year, LAI)
  
  # EDIT IT FOR THE PREDICTORS BA BROADLEAF - TREE_10_40 - BROAD>40
  
  #-------------------------------------------------------------------------------
  # Calculate the number of trees between 10 and 40 cm
  
  # Specify the range for DBH
  dbh_min <- 10
  dbh_max <- 40
  
  # Calculate the number of trees in each year with DBH between 10 and 40 cm
  # Create a data frame with all unique years to not having missing years
  all_years <- data.frame(year = unique(tree$year))
  
  # Perform left join with the summarization result
  tree_10_40 <- all_years %>%
    left_join(
      tree %>%
        group_by(year) %>%
        filter(dbh >= dbh_min, dbh <= dbh_max) %>%
        summarise(tree_10_40 = n()/4),
      by = "year"
    ) %>%
    replace(is.na(.), 0)
  
  
  #-------------------------------------------------------------------------------
  # To define the species to be removed
  unique_sp <- unique(landscape$species)  # alternative unique_plots <- unique(CZ_JH1[,"plotID"])

  
  species_to_remove <- c("piab", "pisy", "abal",
                         "lade", "psme", "pini", 
                         "pice")
  
  # Use subset to filter the dataframe
  filtered_broadl <- subset(landscape, !species %in% species_to_remove)
  
  # Sum BA for every broadleaf species in every year
  summed_broadl_ba <- filtered_broadl %>%
    group_by(year) %>%
    summarize(basal_area_m2 = sum(basal_area_m2)/4)%>%
    select(year,basal_area_m2)
  
  #-------------------------------------------------------------------------------
  # Calculate the basal area only of the broadleave with a dbh > 40cm
  
  # Create a data frame with all unique years
  all_years <- data.frame(year = unique(tree$year))
  
  # Perform left join with the summarization result
  broadl_40 <- all_years %>%
    left_join(
      tree %>%
        group_by(year) %>%
        filter(dbh > 40 & !species %in% species_to_remove) %>%
        summarise(broadl_40 = n()/4),
      by = "year"
    ) %>%
    replace(is.na(.), 0)
  
  # Make the squared one
  broadl_40_2 <- broadl_40 %>% 
    group_by(year)%>%
    mutate(broadl_40_2 = broadl_40^2)

  #-------------------------------------------------------------------------------
  # Age - comes from the avarage age
  
  stand_age <- tree %>%
    group_by(year) %>%
    summarize(age = mean(age))
  stand_age <- round(stand_age)
  
  
  #-------------------------------------------------------------------------------
  # DW volume - USE snags_c divided by 4 and converted into volume 
  # Alternative use the Carbon instead that volume
  
  #-----------------------------------------------------------------------------
  # CREATE THE DATA FRAME FOR ADD VARIABLES ABOUT CARBON IN THE FINAL DATA FRAME
  
  totalC_kgha_iland <- data.frame(carbon %>% 
                                    group_by(year) %>% 
                                    summarise(totalC_kgha_iland=sum(stem_c, branch_c, foliage_c, coarseRoot_c, fineRoot_c, regeneration_c, snags_c, snagsOther_c, downedWood_c, litter_c, soil_c)))
  
  # DW carbon total
  total_DW_C_kgha <- data.frame(carbon %>% 
                                    group_by(year) %>% 
                                    summarise(total_DW_C_kgha=sum(snags_c, snagsOther_c, downedWood_c)))
  
  # DW carbon total without others snags
  total_DW_C_kgha_2 <- data.frame(carbon %>% 
                                    group_by(year) %>% 
                                    summarise(total_DW_C_kgha_2=sum(snags_c, downedWood_c)))
  
  # Good one
  standing_DW_C <- data.frame(carbon %>% 
                                       group_by(year) %>% 
                                       summarise(standing_DW_C = sum(snags_c))) 
  
  # Create a new row with manually specified values
  new_row_1 <- c(0, 749)          # Add your values accordingly swdC
  new_row_2 <- c(0, 1207)         # Add your values accordingly : original 749+233+225 swdC+other_swdC+yrC
  new_row_3 <- c(0, 283716.5)     # Add your values accordingly to the year 1
  new_row_4 <- c(0, 981)
  # Add the new row at the beginning of the data frame in all Carbon variables
  totalC_kgha_iland <- rbind(new_row_3, totalC_kgha_iland)
  total_DW_C_kgha <- rbind(new_row_2, total_DW_C_kgha)
  standing_DW_C <- rbind(new_row_1, standing_DW_C)
  total_DW_C_kgha_2 <- rbind(new_row_4, total_DW_C_kgha_2)
  
  #-------------------------------------------------------------------------------
  # Merge the data frames Plot L1_10
  plot_L1_10_df_simul <- bind_cols(stand_age, 
                                   standing_DW_C = standing_DW_C$standing_DW_C,
                                   totalC_kgha_iland = totalC_kgha_iland$totalC_kgha_iland,
                                   total_DW_C_kgha = total_DW_C_kgha$total_DW_C_kgha,
                                   total_DW_C_kgha_2 = total_DW_C_kgha_2$total_DW_C_kgha_2,
                                   lai_sim = LAI$LAI,
                                   ba_broadl = summed_broadl_ba$basal_area_m2,
                                   trees_10_40 = tree_10_40$tree_10_40,
                                   broadl_40 = broadl_40$broadl_40,
                                   broadl_40_2 = broadl_40_2$broadl_40_2)
  

  #-------------------------------------------------------------------------------
  # CREATE FUNCTION FOR THE BAYESIAN STATISTICAL MODEL 
  
  # VERSION WITH CHANGING DW VOLUME
  # ADD NECROMSS AND VOLUME DW
  Bayesian_BDV_model_bryophytes_V1 <- plot_L1_10_df_simul %>%
    group_by(year) %>%
    mutate(necromass = total_DW_C_kgha_2*268/135)
  
  Bayesian_BDV_model_bryophytes_V1 <- Bayesian_BDV_model_bryophytes_V1 %>%
    group_by(year) %>%
    mutate(volume_DW = (necromass/268)/4)
  
  # SELECT VARIABLES NEEDED
  Bayesian_BDV_model_bryophytes_V1 <- Bayesian_BDV_model_bryophytes_V1 %>%
    group_by(year) %>%
    select(
      age,
      volume_DW,
      lai_sim,
      ba_broadl,
      trees_10_40,
      broadl_40,
      broadl_40_2)
  
  # CREATE THE Y BDV VALUE PREDICTED
  # SELECT VARIABLES NEEDED
  Bayesian_BDV_model_bryophytes_V1 <- Bayesian_BDV_model_bryophytes_V1 %>%
    group_by(year) %>%
    mutate( 
      BRYO_PRED_RICH_97.5 = 11.13348 + 0.04492*age + 0.13528*volume_DW,
      BRYO_PRED_RICH_50 = 9.29610 + 0.02951*age + 0.07511*volume_DW,
      BRYO_PRED_RICH_2.5 = 7.49163 + 0.01300 *age + 0.01863*volume_DW,
      LICHEN_PRED_RICH_97.5 = 17.75719 + 0.06976*age + 0.27118*volume_DW + -0.20916 *lai_sim + 0.56473*broadl_40 + -0.00052*broadl_40_2,
      LICHEN_PRED_RICH_50 = 13.09615 + 0.03147*age + 0.14759*volume_DW + -1.42724*lai_sim + 0.28198*broadl_40 + -0.00713*broadl_40_2,
      LICHEN_PRED_RICH_2.5 = 8.28928 + 0.00352*age + 0.03030*volume_DW + -2.86803*lai_sim + 0.05974*broadl_40 + -0.02065*broadl_40_2,
      MACROFUNGI_PRED_RICH_97.5 = 102.13053 + 0.39827*age + 1.60687*volume_DW + 5.14782*ba_broadl + 0.14964*trees_10_40,
      MACROFUNGI_PRED_RICH_50 = 86.91990 + 0.22621*age + 1.10588*volume_DW + 2.94625*ba_broadl + 0.06710*trees_10_40,
      MACROFUNGI_RICH_2.5 = 70.10431 + 0.06419*age + 0.57899*volume_DW + 0.80328*ba_broadl + 0.00816*trees_10_40,
      MACROFUNGI_RED_PRED_RICH_97.5 = 122.75816 + 0.51149*age + 3.25139*volume_DW + 7.43658*ba_broadl,
      MACROFUNGI_RED_PRED_RICH_50 = 100.38539 + 0.28783*age + 2.55255*volume_DW + 2.97206*ba_broadl,
      MACROFUNGI_RED_RICH_2.5 = 77.44204 + 0.05421*age + 1.78682*volume_DW + 0.84145*ba_broadl,
      BRYO_PRED_RICH_50_beta1 = 0.02951*age,
      BRYO_PRED_RICH_50_beta2 = 0.07511*volume_DW,
      LICHEN_PRED_RICH_50_beta1 = 0.03147*age,
      LICHEN_PRED_RICH_50_beta2 = 0.14759*volume_DW,
      LICHEN_PRED_RICH_50_beta3 = -1.42724*lai_sim,
      LICHEN_PRED_RICH_50_beta4 = 0.28198*broadl_40,
      LICHEN_PRED_RICH_50_beta5 = -0.00713*broadl_40_2,
      MACROFUNGI_PRED_RICH_50_beta1 = 0.22621*age,
      MACROFUNGI_PRED_RICH_50_beta2 = 1.10588*volume_DW,
      MACROFUNGI_PRED_RICH_50_beta3 = 2.94625*ba_broadl,
      MACROFUNGI_PRED_RICH_50_beta4 = 0.06710*trees_10_40,
      MACROFUNGI_RED_PRED_RICH_50_beta1 = 0.28783*age,
      MACROFUNGI_RED_PRED_RICH_50_beta2 = 2.55255*volume_DW,
      MACROFUNGI_RED_PRED_RICH_50_beta3 = 2.97206*ba_broadl
    )
#-------------------------------------------------------------------------------
  # SECOND WITH STATIC VOLUME EQUAL TO THE INITIAL VALUE
  # ADD VOLUME DW
  Bayesian_BDV_model_bryophytes_V2 <- plot_L1_10_df_simul %>%
    group_by(year) %>%
    mutate(volume_DW = 2.01)
  
  # SELECT VARIABLES NEEDED
  Bayesian_BDV_model_bryophytes_V2 <- Bayesian_BDV_model_bryophytes_V2 %>%
    group_by(year) %>%
    select(
      age,
      volume_DW,
      lai_sim,
      ba_broadl,
      trees_10_40,
      broadl_40,
      broadl_40_2)
  
  # CREATE THE Y BDV VALUE PREDICTED
  Bayesian_BDV_model_bryophytes_V2 <- Bayesian_BDV_model_bryophytes_V2 %>%
    group_by(year) %>%
    mutate( 
      BRYO_PRED_RICH_97.5 = 11.13348 + 0.04492*age + 0.13528*volume_DW,
      BRYO_PRED_RICH_50 = 9.29610 + 0.02951*age + 0.07511*volume_DW,
      BRYO_PRED_RICH_2.5 = 7.49163 + 0.01300 *age + 0.01863*volume_DW,
      LICHEN_PRED_RICH_97.5 = 17.75719 + 0.06976*age + 0.27118*volume_DW + -0.20916 *lai_sim + 0.56473*broadl_40 + -0.00052*broadl_40_2,
      LICHEN_PRED_RICH_50 = 13.09615 + 0.03147*age + 0.14759*volume_DW + -1.42724*lai_sim + 0.28198*broadl_40 + -0.00713*broadl_40_2,
      LICHEN_PRED_RICH_2.5 = 8.28928 + 0.00352*age + 0.03030*volume_DW + -2.86803*lai_sim + 0.05974*broadl_40 + -0.02065*broadl_40_2,
      MACROFUNGI_PRED_RICH_97.5 = 102.13053 + 0.39827*age + 1.60687*volume_DW + 5.14782*ba_broadl + 0.14964*trees_10_40,
      MACROFUNGI_PRED_RICH_50 = 86.91990 + 0.22621*age + 1.10588*volume_DW + 2.94625*ba_broadl + 0.06710*trees_10_40,
      MACROFUNGI_RICH_2.5 = 70.10431 + 0.06419*age + 0.57899*volume_DW + 0.80328*ba_broadl + 0.00816*trees_10_40,
      MACROFUNGI_RED_PRED_RICH_97.5 = 122.75816 + 0.51149*age + 3.25139*volume_DW + 7.43658*ba_broadl,
      MACROFUNGI_RED_PRED_RICH_50 = 100.38539 + 0.28783*age + 2.55255*volume_DW + 2.97206*ba_broadl,
      MACROFUNGI_RED_RICH_2.5 = 77.44204 + 0.05421*age + 1.78682*volume_DW + 0.84145*ba_broadl
    )  
  
  
  #-----------------------------------------------------------------------------
  # CREATE A COLUMN WITH CUMULATIVE MORTALITY IN VOLUME
  
  landscape_removed_natmor <- landscape_removed %>%
    filter(reason == "N") %>%
    group_by(species) %>%
    arrange(year) %>%  # Ensure the data is ordered by year before calculating cumulative sum
    mutate(cumm_mortality_total_carbon = cumsum(total_carbon)) %>%
    ungroup()
  
  
  #-----------------------------------------------------------------------------
  # CREATE THE CALCULATION FOR DAMAGES LOOK LINE 370
  
  landscape.area<-landscape$area[1]                                             # CREATE THE VARIABLE FOR LANDSCAPE AREA          
  
  lnd_volume = landscape %>% group_by(year)  %>%                                # CREATE THE SUMMARIZATION OF THE SPECIES VOLUME PROPORTION TO CREATE A TOTAL LANDSCAPE VOLUME
    summarise(tot_vol = sum(volume_m3),
              .groups = 'drop')
  
  head(lnd_volume)                                                              
  
  # wind and barkbeetle merging:
  
  head(barkbeetle)
  # head(wind)
  
  
  damage <- data.frame(year=barkbeetle$year,                                      # CREATE THE DATA FRAME FOR FOR DAMAGE OF BARKBEETLE
                       barkbeetle=barkbeetle$killedVolume, 
                       case=case)
  
  # ADD WIND IMPACT IN THE DAMAGE DATA FRAME
  # damage<-left_join(damage,wind[,c(1,8)],by=("year"))                           # LEFT_JOIN IS A FUNCTION TO JOIN A VARIABLE IN THIS CASE COLUMN 1 AND 2 AND MANY ROWS BASE ON YEAR VARIABLE NUMBER OF ROWS
  damage<-left_join(damage,lnd_volume,by=("year"))                              # ADD THE LANDSCAPE VOLUME IN THE DAMAGE DATA FRAME
  # colnames(damage)<-c("year","barkbeetle","case","wind","volume")               # GIVE THE NAME AT EVERY VARIABLE
  
  # damage$wind[which(is.na(damage$wind)==TRUE)] <-0                            # FOR MAKE THE na = 0 !!!! "
  
  head(damage)
  
  #-----------------------------------------------------------------------------
  # Make the 3 categories of removals:
  
  activity.names<-unique(abeStandRemoval$activity)                              # here I list all different type of activities
  
  swcuts<- grepl("sw",activity.names)                                           # I look for only which has "sw" this grepl gives TRUE/FALSE
  activity.names.sw<-activity.names[swcuts]                                     # collect the activity names with sw
  activity.names.notsw<-activity.names[!swcuts]                                 # collect the activity names withOUT sw
  
  #print(activity.names.sw)
  #print(activity.names.notsw)
  
  # Here I filter only the listed activity names and calculate thinning/finalcut values for every year 
  # (each line is per ha for a stand, so I scale with the area, sum up all the harvest on the landscape and then divide it with the whole area to get again per ha)
  
  # TO UNDERSTAND THE OPERATORS %>% AND %IN% HAVE TO STUDY THEM IN DATACAMP AND IN DPLER CRAN PACKAGES
  
  ab.regcuts<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.sw)    %>% 
                            group_by(year)   %>%   summarise(volume=sum(volumeThinning*area)/landscape.area, type="regcut", run=case))
  
  ab.finalcuts<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.sw)    %>% 
                              group_by(year)   %>%   summarise(volume=sum(volumeFinal*area)/landscape.area, type="finalcut", run=case))
  
  ab.thinnig<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.notsw)    %>% 
                            group_by(year)   %>%   summarise(volume=sum(volumeThinning*area)/landscape.area, type="thinning", run=case))
  
  ab.salvaged<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.sw)    %>% 
                             group_by(year)   %>%   summarise(volume=sum(volumeSalvaged*area)/landscape.area, type="salvager", run=case))
  
  # CREATE A FOR CYCLE INTO THE FOR CYCLE
  if (length(activity.names[swcuts])==0) {
    
    ab.finalcuts<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.notsw)    %>% 
                                group_by(year)   %>%   summarise(volume=sum(volumeFinal*area)/landscape.area, type="finalcut", run=case))
    
  }                                                                             # CLOSE THE litte FOR CYCLE
  
  
  # Collect abeUnit data FOR CREATE THE VARIABLE ABEUNIT FOR ALL THE RUNS
  abeStand<-(abeStand %>% mutate(run=case))
  abeStand_scen<-rbind(abeStand_scen, abeStand)
  
  # Collect abeUnit data FOR CREATE THE VARIABLE ABEUNIT FOR ALL THE RUNS
  abeStandDetail<-(abeStandDetail %>% mutate(run=case))
  abeStandDetail_scen<-rbind(abeStandDetail_scen, abeStandDetail)
  
  # Collect abeUnit data FOR CREATE THE VARIABLE ABEUNIT FOR ALL THE RUNS
  abeStandRemoval <-(abeStandRemoval %>% mutate(run=case))
  abeStandRemoval_scen<-rbind(abeStandRemoval_scen, abeStandRemoval)
  
  # Collect abeUnit data FOR CREATE THE VARIABLE ABEUNIT FOR ALL THE RUNS
  abeUnit<-(abeUnit %>% mutate(run=case))
  abeUnit_scen<-rbind(abeUnit_scen, abeUnit)
  
  # Collect abeUnit data FOR CREATE THE VARIABLE ABEUNIT FOR ALL THE RUNS
  landscape_removed <-(landscape_removed %>% mutate(run=case))
  landscape_removed_scen<-rbind(landscape_removed_scen, landscape_removed)
  
  # Collect landscape data FOR CREATE THE VARIABLE LND FOR ALL THE RUNS
  landscape <- (landscape %>% mutate(run=case))
  lnd_scen <- rbind(lnd_scen, landscape)
  
  # Collect dynamicstand data FOR CREATE THE VARIABLE WIND FOR ALL THE RUNS
  dynamicstand <-(dynamicstand %>% mutate(run=case))
  dys_scen <-rbind(dys_scen, dynamicstand)
  
  # Collect dynamicstand data FOR CREATE THE VARIABLE WIND FOR ALL THE RUNS
  tree <-(tree %>% mutate(run=case))
  tree_scen <-rbind(tree_scen, tree)
  
  # Collect dynamicstand data FOR CREATE THE VARIABLE WIND FOR ALL THE RUNS
  stand <-(stand %>% mutate(run=case))
  stand_scen <-rbind(stand_scen, stand)
  
  # Collect dynamicstand data FOR CREATE THE VARIABLE WIND FOR ALL THE RUNS
  carbon <-(carbon %>% mutate(run=case))
  carbon_scen <-rbind(carbon_scen, carbon)
  
  # Collect dynamicstand data FOR CREATE THE VARIABLE WIND FOR ALL THE RUNS
  carbonflow <-(carbonflow %>% mutate(run=case))
  carbonflow_scen <-rbind(carbonflow_scen, carbonflow)

  # Collect barkbeetle data FOR CREATE THE VARIABLE BB FOR ALL THE RUNS
  barkbeetle <-(barkbeetle %>% mutate(run=case))
  bb_scen <-rbind(bb_scen, barkbeetle)
  
  # Collect wind data FOR CREATE THE VARIABLE WIND FOR ALL THE RUNS
  # wind <-(wind %>% mutate(run=case))
  # w <-rbind(w, wind)

  # CREATE THE VARIABLE FOR THE DIFFERENT WOOD REMOVAL ACTIVITY
  # HERE IT IS NOT NEEDED TO CHANGE CASE TO RUN BECAUSE ALREADY DONE
  removals<-rbind(removals,ab.regcuts,ab.finalcuts,ab.thinnig,ab.salvaged)
  
  # CREATE THE VARIABLE DAMAGE FOR ALL THE RUNS
  damage <-(damage %>% mutate(run=case))
  damage.all<-rbind(damage.all, damage)                                         # PUT ALL THE DAMAGE RUNS INTO A SINGLE DATAFRAME WITH DIFF CASES TO BE PLOT ALL TOGETHER IN LINE 370
  
  # CREATE THE VARIABLES DF FOR ALL THE RUNS
  variables <-(variables %>% mutate(run=case))
  variables.all<-rbind(variables.all, variables) 
  
  # CREATE THE VARIABLE H of BA per species FOR ALL THE RUNS
  H_BA_heterogenity <-(H_BA_heterogenity %>% mutate(run=case))
  H_BA_heterogenity_scen <- rbind(H_BA_heterogenity_scen, H_BA_heterogenity)
  
  # CREATE THE VARIABLE NATURAL MORTALITY FOR ALL THE RUNS
  landscape_removed_natmor <-(landscape_removed_natmor %>% mutate(run=case))
  landscape_removed_scen_natmor <- rbind(landscape_removed_scen_natmor, landscape_removed_natmor)
  
  # CREATE THE VARIABLES NEEDED IN THE BDV STUDY
  plot_L1_10_df_simul <-(plot_L1_10_df_simul %>% mutate(run=case))
  plot_variables_all <- rbind(plot_variables_all, plot_L1_10_df_simul)
  
  # CREATE THE VARIABLES NEEDED IN THE BDV STUDY - Bayesian_BDV_model_bryophytes_V2
  Bayesian_BDV_model_bryophytes_V1 <-(Bayesian_BDV_model_bryophytes_V1 %>% mutate(run=case))
  Bayesian_BDV_model_V1 <- rbind(Bayesian_BDV_model_V1, Bayesian_BDV_model_bryophytes_V1)
  
  # CREATE THE VARIABLES NEEDED IN THE BDV STUDY - Bayesian_BDV_model_bryophytes_V2
  Bayesian_BDV_model_bryophytes_V2 <-(Bayesian_BDV_model_bryophytes_V2 %>% mutate(run=case))
  Bayesian_BDV_model_V2 <- rbind(Bayesian_BDV_model_V2, Bayesian_BDV_model_bryophytes_V2)
  
}                                                                               # CLOSE THE WHOLE FOR CYCLE

View(plot_variables_all)
View(average_density)
View(Bayesian_BDV_model_bryophytes)
str(Bayesian_BDV_model_bryophytes)
# Create volume conversion


# write excel
writexl::write_xlsx(Bayesian_BDV_model_V1, "C:/iLand/2023/20230901_Bottoms_Up/plot_init/Jenik/final_table_imp/Bayesian_BDV_model_V1_forTomas.xlsx")


# DON'T USE IT
#-------------------------------------------------------------------------------
# CREATE FUNCTION FOR THE BAYESIAN STATISTICAL MODEL 

# ADD NECROMSS AND VOLUME DW
Bayesian_BDV_model_bryophytes_v1 <- plot_variables_all %>%
  group_by(year) %>%
  mutate(necromass = total_DW_C_kgha_2*268/135)

Bayesian_BDV_model_bryophytes_v1 <- Bayesian_BDV_model_bryophytes_v1 %>%
  group_by(year) %>%
  mutate(volume_DW = (necromass/268)/4)

# SELECT VARIABLES NEEDED
Bayesian_BDV_model_bryophytes_v1 <- Bayesian_BDV_model_bryophytes_v1 %>%
  group_by(year) %>%
  select(
    age,
    volume_DW,
    lai_sim,
    ba_broadl,
    trees_10_40,
    broadl_40)

# CREATE THE Y BDV VALUE PREDICTED
# SELECT VARIABLES NEEDED
Bayesian_BDV_model_bryophytes_v1 <- Bayesian_BDV_model_bryophytes_v1 %>%
  group_by(year) %>%
  mutate( 
    BRYO_PRED_RICH_97.5 = 11.13348 + 0.04492*age + 0.13528*volume_DW,
    BRYO_PRED_RICH_50 = 9.29610 + 0.02951*age + 0.07511*volume_DW,
    BRYO_PRED_RICH_2.5 = 7.49163 + 0.01300 *age + 0.01863*volume_DW
    )

writexl::write_xlsx(Bayesian_BDV_model_bryophytes_v1, "C:/iLand/2023/20230901_Bottoms_Up/plot_init/Jenik/final_table_imp/tables_for_stat/Bayesian_BDV_model_bryophytes_fix_dw_vol.xlsx")

# SECOND WITH CHANGING VARIABLES
# ADD VOLUME DW
Bayesian_BDV_model_bryophytes_V2 <- plot_variables_all %>%
  group_by(year) %>%
  mutate(volume_DW = 2.01)

# SELECT VARIABLES NEEDED
Bayesian_BDV_model_bryophytes_V2 <- Bayesian_BDV_model_bryophytes_V2 %>%
  group_by(year) %>%
  select(
    age,
    volume_DW,
    lai_sim,
    ba_broadl,
    trees_10_40,
    broadl_40)

# CREATE THE Y BDV VALUE PREDICTED - y(bryophytes)= beta0 + beta1*age + beta2*dw
Bayesian_BDV_model_bryophytes_V2 <- Bayesian_BDV_model_bryophytes_V2 %>%
  group_by(year) %>%
  mutate( 
    BRYO_PRED_RICH_97.5 = 11.13348 + 0.04492*age + 0.13528*volume_DW,
    BRYO_PRED_RICH_50 = 9.29610 + 0.02951*age + 0.07511*volume_DW,
    BRYO_PRED_RICH_2.5 = 7.49163 + 0.01300 *age + 0.01863*volume_DW
  )

writexl::write_xlsx(Bayesian_BDV_model_bryophytes_V2, "C:/iLand/2023/20230901_Bottoms_Up/plot_init/Jenik/final_table_imp/tables_for_stat/Bayesian_BDV_model_bryophytes.xlsx")


library(ggplot2)
library(GGally)
library(corrplot)



#--------------------------------------------------------------
# Start with plots
#_______________________________________________
library(ggplot2)
library(gridExtra) # To arrange the graphs in a grid

# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/outputs/20231129/"
pdf(paste0(dataroot, "2023121444_BDV_mng_plot_L1_10_300.pdf"), height=8, width=12)

# or
png(paste0(dataroot, "1_20231205_BDV_bayesian_mng_plot_L1_10_300.png"), height = 8 * 300, width = 12 * 300, res = 300)


#-------------------------------------------------------------------------------
# This tells the colors:

species.we.have<-unique(lnd_scen$species)                                            # IT IS SAYING WHICH SPECIES WE HAVE IN THE DATABASE IN THIS CASE LANDSCAPE


# LIST OF ALL POSSIBLE SPECIES

cols.all=c( "rops"="#e0e0e0", "acpl"="#A9A9A9",   "alin"="#696969", "alvi"="#2e2e2e",
            "bepe"="#fadfad", 
            "casa"="#7eeadf", "coav"="#20c6b6",  
            "tipl"="#645394", "ulgl"="#311432" ,
            "saca"="#D8BFD8",  "soar"="#DDA0DD", "soau"="#BA55D3",
            "pice"="#D27D2D", "pini"="#a81c07",
            "algl"="#2ECBE9","tico"="#128FC8",  "potr"="#00468B","poni"="#5BAEB7",
            "frex"="#fe9cb5","cabe"="#fe6181","acps"="#fe223e",
            "lade"="#FFFE71","abal"="#FFD800", "pisy"="#A4DE02",
            "fasy"="#76BA1B", "piab"="#006600",
            "quro"="#FF7F00", "qupe"="#FF9900", "qupu"="#CC9900" 
)


# COLORATION ORDER FOR ALL THE POSSIBLE SPECIES

new_order_gg.all=c("alvi","alin", "acpl", "rops","bepe" ,"coav", "casa", "ulgl", "tipl",  "soau", "soar", "saca",  "pini", "pice",
                   "poni", "algl", "tico", "potr",  "frex","cabe", "acps",  "lade", "abal",  "qupu", "qupe","quro","pisy", "fasy", "piab")


# This will show at the end only the species we really have on the landscape. 

cols<-cols.all[names(cols.all) %in% species.we.have]
new_order_gg<- new_order_gg.all[new_order_gg.all %in% species.we.have]

# STARTING PLOTS

#-------------------------------------------------------------------------------
# COLUMN DIAGRAM PLOT ON THE HARVEST

M1 <- ggplot(removals, aes(year, volume, fill=factor(type, levels=c( "regcut","finalcut","thinning","salvager"))))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="Removed volume m3/ha",fill = "Removal")+
  scale_fill_manual(values=c("#4897D8","#FFDB5C","#FA6E59","#B3C100"))+               #"#B7B8B6","#34675C","#B3C100" grey and greens
  theme_bw()

# Make a plot with ggplot, volume, colored by species for the transitional period for Clear cut management system
#-------------------------------------------------------------------------------
# PLOT LANDSCAPE VOLUME PLOT FOR CASES (GEOM AREA)

g1 <- ggplot(lnd_scen, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Landscape Volume by species")+
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="Volume [m3/ha]",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,1000)+
  theme_bw()

# Plot grid arrange
grid.arrange(M1,g1, ncol=1)

#-------------------------------------------------------------------------------
# (SHOULD BE REALIZED) PLOT 2 "Y" AXIS WITH relationship between realized harvest and volume increasing in the landscape
# Total realized harvest at landscape level in average per ha

g2 <- ggplot(abeUnit_scen, aes(year,realizedHarvest, color='red'))+
  geom_col(size=1, show.legend = F)+
  facet_wrap(~run, ncol=2)+
  ggtitle("Realized Harvest Transitional Period")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest [m3/ha]")+
  theme_bw()


#-------------------------------------------------------------------------------

# SPECIES specificaly BA:

#species.to.keep<-c("piab","pisy", "fasy","qupe")

#lnd2 <- lnd %>% filter(species %in% species.to.keep)

#ggplot(data=lnd2, aes(x=year, y=basal_area_m2, colour=species)) + 
#  geom_line(size=1.2)+
#  scale_colour_manual(values = c("#76BA1B","#006600", "#A4DE02", "orange"))+
#  ggtitle("Clearcut management in brow pressure 0") +
#  theme(plot.title = element_text(hjust = 0.5))+
#  ylab("Basal area [m2/ha]")+
#  theme_bw()

#-------------------------------------------------------------------------------
# PLOT BASAL AREA GEOM_LINE AT LANDSCAPE LEVEL BY SPECIES SELECTED

# SPECIES specificaly BA:

species.to.keep<-c("piab", "fasy","bepe", "pisy", "abal")


lnd_scen2 <- lnd_scen %>% filter(species %in% species.to.keep)

b1 <- ggplot(data=lnd_scen2, aes(x=year, y=basal_area_m2, color=species)) + 
  geom_line(size=1.2)+
  ggtitle("Basal area by species") +
  facet_wrap(~run, ncol=2)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area [m2/ha]")+  
  theme_bw()

#-------------------------------------------------------------------------------
# PLOT TOTAL AVG BASAL AREA AT LANDSCAPE LEVEL BY SPECIES

g3 <- ggplot(lnd_scen, aes(year, basal_area_m2, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Total Basal Area")+
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="Basal Area [m2/ha]",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

#-------------------------------------------------------------------------------
# PLOT SUM BASAL AREA AT LANDSCAPE

b2 <- ggplot(data=dys_scen, aes(x=year, y=basalarea_sum/landscape.area, color = "red")) + 
  geom_line(size=1, show.legend = F)+
  ggtitle("Avarege Basal area") +
  facet_wrap(~run, ncol=2)+
  ylab("Basal area [m2/ha]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

#-------------------------------------------------------------------------------
# PLOT DBH GEOM_LINE AT LANDSCAPE LEVEL BY SPECIES

species.to.keep<-c("piab", "fasy","qupe", "pisy")


lnd_scen2 <- lnd_scen %>% filter(species %in% species.to.keep)

b3 <- ggplot(data=lnd_scen2, aes(x=year, y=dbh_avg_cm, color=species)) + 
  geom_line(size=1.2)+
  ggtitle("Avarage DBH by species") +
  facet_wrap(~run, ncol=2)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("DBH [cm]")+  
  theme_bw()


#-------------------------------------------------------------------------------
# PLOT DBH GEOM_AREA AT LANDSCAPE LEVEL AVARAGE ALL SP TOGETHER

g4 <- ggplot(data=dys_scen, aes(x=year, y=dbh_mean)) + 
  geom_area(size=1.2)+
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Avarage DBH by species") +
  facet_wrap(~run, ncol=2)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("DBH [cm]")+  
  theme_bw()

# SD DBH / try with violin plots

# ggplot(data=dys_scen, aes(x=year, y=dbh_sd)) + 
#  geom_area(size=1.2)+
#  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
#  ggtitle("Avarage DBH by species") +
#  facet_wrap(~run, ncol=1)+
#  theme(plot.title = element_text(hjust = 0.5))+
#  ylab("DBH [cm]")+  
#  theme_bw()

#-------------------------------------------------------------------------------
# PLOT NUMBER OF STEMS GEOM_LINE AT LANDSCAPE LEVEL BY SPECIES

# lnd2 <- lnd %>% filter(species %in% species.to.keep)    --- ADD IF YOU WANT SELECT SPECIES TO KEEP

b4 <- ggplot(data=lnd_scen2, aes(x=year, y=count_ha, color=species)) + 
  geom_line(size=1.2)+
  ggtitle("N. individual stems by species") +
  facet_wrap(~run, ncol=2)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Individual stems")+  
  theme_bw()


#-------------------------------------------------------------------------------
# PLOT NUMBER OF STEMS GEOM_AREA AT LANDSCAPE LEVEL BY SPECIES

g5 <- ggplot(lnd_scen, aes(x=year, y=count_ha, fill=factor(species, levels=new_order_gg)))+ 
  geom_area(size=1.2)+
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("N. individual stems by species") +
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="Individual Stems", fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

#-------------------------------------------------------------------------------
# PLOT HEIGHT GEOM_LINE AT LANDSCAPE LEVEL BY SPECIES

# lnd2 <- lnd %>% filter(species %in% species.to.keep)    ---   ADD IF YOU WANT SELECT SPECIES TO KEEP

b5 <- ggplot(data=lnd_scen2, aes(x=year, y=height_avg_m, color=species)) + 
  geom_line(size=1.2)+
  ggtitle("Avarage Height by species") +
  facet_wrap(~run, ncol=2)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Height [m]")+  
  theme_bw()


#-------------------------------------------------------------------------------
# PLOT TOTAL HEIGHT GEOM_AREA AT LANDSCAPE LEVEL BY SPECIES

g6 <- ggplot(dys_scen, aes(x=year, y=height_mean))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Total Avarage Height")+
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="Height [m]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# AGE 

g7 <- ggplot(dys_scen, aes(x=year, y=age_mean))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Avarage Tree Age")+
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="Age [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()


#-------------------------------------------------------------------------------
# Total Carbon in Kg (total_carbon_kg	double	total carbon in living biomass (aboveground compartments and roots) of all living trees (including regeneration layer) (kg/ha))

g8 <- ggplot(lnd_scen, aes(year, total_carbon_kg, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Total Carbon in Living Biomass")+
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="[kg/ha]",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

#-------------------------------------------------------------------------------
# PLOT LAI AT LANDSCAPE LEVEL BY SPECIES

g9 <- ggplot(lnd_scen, aes(year, LAI, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("LAI index by species")+
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="LAI index",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()


#-------------------------------------------------------------------------------
# PLOT NPP AT LANDSCAPE LEVEL BY SPECIES

g10 <- ggplot(lnd_scen, aes(year, NPP_kg, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Net Primary Productivity")+
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="NPP [kg/ha]",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()


#grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10, ncol=10)

#grid.arrange(b1,b2,b3,b4,b5,ncol=5)

grid.arrange(g1,g5,g6, ncol=3)

grid.arrange(g1,g5,g4, ncol=3)



# Natural mortality 1st scenario browsing 1

landscape_removed_scen_natmor <- landscape_removed_scen %>%
  filter(reason == "N")

nm1 <- ggplot(landscape_removed_scen_natmor, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_col() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  facet_wrap(~run, ncol=2)+
  ggtitle("Natural Mortality volume") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))


# CUMULATIVE NATURAL MORTALITY
P1

# CARBON 

snag_C <- ggplot(carbon_scen, aes(x=year, y=snags_c))+
  geom_line() +
  facet_wrap(~run, ncol=2)+
  ggtitle("Snags_C [iLand snags_C fun]")+
  labs(x = "Year",y="snags_C [kg/ha]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()


# Shannon

H.count <- ggplot(variables.all, aes(x=year, y=H.count))+
  geom_line() +
  facet_wrap(~case, ncol=2)+
  ggtitle("Shannon Index on numbers of individuals per species")+
  labs(x = "Year",y="Shannon Index")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# Shannon

H.VOL <- ggplot(variables.all, aes(x=year, y=H.VOL))+
  geom_line() +
  facet_wrap(~case, ncol=2)+
  ggtitle("Shannon Index on volume proportion per species")+
  labs(x = "Year",y="Shannon Index")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# Shannon

H.BA <- ggplot(variables.all, aes(x=year, y=H.BA))+
  geom_line() +
  facet_wrap(~case, ncol=2)+
  ggtitle("Shannon Index on basal area per species")+
  labs(x = "Year",y="Shannon Index")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

#-------------------------------------------------------------------------------
# Make new plots



# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/outputs/20231129/"
png(paste0(dataroot, "3_20231205_Fungi_RedList_BDV_mng_plot_L1_10_300.png"), height = 8 * 300, width = 12 * 300, res = 300)



# TOTAL CARBON IN THE PLOT (LIVING + DEADWOOD + LITTER + SOIL)
totalC_kgha_iland <- ggplot(plot_variables_all, aes(x=year, y=totalC_kgha_iland))+
  geom_line() +
  facet_wrap(~run, ncol=2)+
  ggtitle("Total Plot Carbon [living trees - deadwood - litter - soil]")+
  labs(x = "Year",y="Total C [kg/ha]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# TOTAL DEADWOOD CARBON (SNAGS + OTHERSNAGS + DOWNED DEADWOOD)
total_DW_C_kgha <- ggplot(plot_variables_all, aes(x=year, y=total_DW_C_kgha))+
  geom_line() +
  facet_wrap(~run, ncol=2)+
  ggtitle("Total Deadwood C in iLand standing and lying [kg/ha]")+
  labs(x = "Year",y="Deadwood C [kg/ha]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# TOTAL DEADWOOD CARBON (SNAGS + DOWNED DEADWOOD)
total_DW_C_kgha_2 <- ggplot(plot_variables_all, aes(x=year, y=total_DW_C_kgha_2))+
  geom_line() +
  facet_wrap(~run, ncol=2)+
  ggtitle("Total Deadwood C in iLand standing and lying [kg/ha]")+
  labs(x = "Year",y="Deadwood C [kg/ha]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# TOTAL STANDING DEADWOOD CARBON (SNAGS ONLY)
standing_DW_C <- ggplot(plot_variables_all, aes(x=year, y=standing_DW_C))+
  geom_line(color = "#2e2e2e") +
  facet_wrap(~run, ncol=2)+
  ggtitle("snag_C [iLand snag_C fun] = standing_DW_C")+
  labs(x = "Year",y="snag_C [kg/ha]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# Volume deadwood
volume_DW <- ggplot(Bayesian_BDV_model_V1, aes(x=year, y=volume_DW))+
  geom_line(color = "#2e2e2e") +
  facet_wrap(~run, ncol=2)+
  ggtitle("Volume Deadwood [m3/ha]")+
  labs(x = "Year",y="Volume Deadwood [m3/ha]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# Age mean

g7 <- ggplot(dys_scen, aes(x=year, y=age_mean))+
  geom_line(color = "red") +
  ggtitle("Avarage Tree Age")+
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="Age mean [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# PLOT LAI AT LANDSCAPE LEVEL BY SPECIES

g9 <- ggplot(lnd_scen, aes(year, LAI, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=FALSE)+
  ggtitle("LAI index by species")+
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="LAI index",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# BA BROADLEAVE SP

ba_broadl <- ggplot(plot_variables_all, aes(x=year, y=ba_broadl))+
  geom_line(color = "#311432") +
  ggtitle("BA BROADLEAVE SP")+
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="BA BROADLEAVE SP [m2]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()


# NUMBER OF TREES WITH DBH BETWEEN 10cm AND 40cm included

trees_10_40 <- ggplot(plot_variables_all, aes(x=year, y=trees_10_40))+
  geom_line(color = "#4897D8") +
  ggtitle("NUMBER OF TREES WITH DBH BETWEEN 10cm AND 40cm included")+
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="Trees with dbh 10cm to 40cm [No]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# NUMBER OF BROADLEAVE TREES WITH DBH > 40cm

broadl_40 <- ggplot(plot_variables_all, aes(x=year, y=broadl_40))+
  geom_line(color ="#006600") +
  ggtitle("NUMBER OF BROADLEAVE TREES WITH DBH > 40cm")+
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="Broadleave > 40cm [No]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# Cumulative Natural Mortality

P1 <- ggplot(landscape_removed_scen_natmor, aes(year, cumm_mortality_total_carbon, color=species )) +
  geom_line(size = 0.6) +
  facet_wrap(~run, ncol=2)+
  ggtitle("Cumulative Natural Mortality in Carbon") +
  ylab("Total Carbon in Nat. Mortality [kg/ha]") +
  xlab("Year") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, lineheight = 3, face = "bold", color = "black", size = 20),
    axis.title.y = element_text(size = rel(1.8), angle = 90),
    axis.title.x = element_text(size = rel(1.8), angle = 0)
  )

# Display the plot
grid.arrange(g7, volume_DW, g9, ncol=1)
dev.off()
# Shannon entropy of single species basal area (heterogeneity of individuals dimensions)

{H_BA_sp <- ggplot(H_BA_heterogenity_scen, aes(x=year, y=shannon_ba_heterog))+
  geom_line() +
  ggtitle("Shannon entropy of single species basal area (heterogeneity of individuals dimensions)")+
  facet_wrap(~run, ncol=2)+
  labs(x = "Year",y="Shannon entropy per sp on individuals BA")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()}


species.to.keep<-c("piab", "fasy","qupe", "pisy")


H_BA_heterogenity_scen2 <- H_BA_heterogenity_scen %>% filter(species %in% species.to.keep)

ggplot(data=H_BA_heterogenity_scen2, aes(x=year, y=shannon_ba_heterog, color=species)) + 
  geom_line(size=0.6)+
  ggtitle("Shannon entropy of single species basal area (heterogeneity of individuals dimensions)") +
  facet_wrap(~run, ncol=2)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Shannon entropy per sp on individuals BA")+  
  theme_bw()


# Plot grid arrange
grid.arrange(avara,trees_10_40,broadl_40, ncol=1)
dev.off()

########################################################## CLOSE EVERY PLOTs



#-------------------------------------------------------------------------------

# PLOT FOR Bayesian model meeting

###############    BRYOPHYTES VOLUME CHANGING ################

P1 <-ggplot(Bayesian_BDV_model_V1, aes(year, ymin = BRYO_PRED_RICH_2.5, ymax = BRYO_PRED_RICH_97.5,fill=run))+
  geom_ribbon(  alpha=0.3)+
  geom_line(aes(year,BRYO_PRED_RICH_50, color=run), lwd=0.9)+
  facet_grid(~run, scales = "free", switch="y")+
  facet_wrap(~run, scales = "free", ncol = 2) +
  ylab("")+
  scale_fill_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  scale_color_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  theme_bw()+
  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        legend.position = "none",
        strip.placement = "outside"
  )+
  ggtitle("Bryophytes BDV Richness Over Time")+
  scale_x_continuous(breaks = seq(min(Bayesian_BDV_model_V1$year), max(Bayesian_BDV_model_V1$year), by = 20),
                     labels = seq(min(Bayesian_BDV_model_V1$year), max(Bayesian_BDV_model_V1$year), by = 20))


print(P1)

#-------------------------------------------------------------------------------
###############    BRYOPHYTES VOLUME STATIC ################

P1 <-ggplot(Bayesian_BDV_model_V2, aes(year, ymin = BRYO_PRED_RICH_2.5, ymax = BRYO_PRED_RICH_97.5,fill=run))+
  geom_ribbon(  alpha=0.3)+
  geom_line(aes(year,BRYO_PRED_RICH_50, color=run), lwd=0.9)+
  facet_grid(~run, scales = "free", switch="y")+
  facet_wrap(~run, scales = "free", ncol = 2) +
  ylab("")+
  scale_fill_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  scale_color_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        legend.position = "none",
        strip.placement = "outside"
  )+
  ggtitle("Bryophytes BDV Richness Over Time")

print(P1)

#-------------------------------------------------------------------------------
###############    BRYOPHYTES VOLUME CHANGING ################

P1 <- ggplot(Bayesian_BDV_model_V1, aes(x = year, ymin = BRYO_PRED_RICH_2.5, ymax = BRYO_PRED_RICH_97.5, fill = run)) +
  
  # Add ribbon layers for uncertainty with alpha = 0.3
  geom_ribbon(alpha = 0.3) +
  
  # Add lines for mean, min, and max values
  geom_line(aes(y = BRYO_PRED_RICH_50, color = run), lwd = 0.9) +
  geom_line(aes(y = BRYO_PRED_RICH_2.5, color = run), lwd = 0.3) +  # Change line type for min
  geom_line(aes(y = BRYO_PRED_RICH_97.5, color = run), lwd = 0.3) +  # Change line type for max
  
  # Facet the plot based on the 'run' variable in a 2x2 matrix
  facet_wrap(~run, scales = "free", ncol = 2) +
  
  # Customize the theme of the plot
  theme_bw() +
  theme(
    # Remove major and minor grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Remove background from the plot area
    panel.background = element_blank(),
    
    # Remove legend
    legend.position = "none",
    
    # Remove strip background
    strip.background = element_blank(),
    
    # Adjust the title position
    plot.title = element_text(hjust = 0.5),
    
    # Set y-axis to start from 0
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10)
  ) +
  
  # Set y-axis label
  ylab("Species Richness Prediction") +
  
  # Set colors manually for fill and color
  scale_fill_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  scale_color_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  
  # Set y-axis limits
  coord_cartesian(ylim = c(0, 40))

# Set plot title
P1 <- P1 + ggtitle("Bryophytes BDV Richness Over Time")

# Print the plot
print(P1)

# Plot grid arrange
grid.arrange(g7, volume_DW, P1, ncol=1)

########################################################## CLOSE EVERY PLOTs

dev.off()

#-------------------------------------------------------------------------------
###############    BRYOPHYTES VOLUME STATIC ################

P1 <- ggplot(Bayesian_BDV_model_V2, aes(x = year, ymin = BRYO_PRED_RICH_2.5, ymax = BRYO_PRED_RICH_97.5, fill = run)) +
  
  # Add ribbon layers for uncertainty with alpha = 0.3
  geom_ribbon(alpha = 0.3) +
  
  # Add lines for mean, min, and max values
  geom_line(aes(y = BRYO_PRED_RICH_50, color = run), lwd = 0.9) +
  geom_line(aes(y = BRYO_PRED_RICH_2.5, color = run), lwd = 0.3) +  # Change line type for min
  geom_line(aes(y = BRYO_PRED_RICH_97.5, color = run), lwd = 0.3) +  # Change line type for max
  
  # Facet the plot based on the 'run' variable in a 2x2 matrix
  facet_wrap(~run, scales = "free", ncol = 2) +
  
  # Customize the theme of the plot
  theme_bw() +
  theme(
    # Remove major and minor grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Remove background from the plot area
    panel.background = element_blank(),
    
    # Remove legend
    legend.position = "none",
    
    # Remove strip background
    strip.background = element_blank(),
    
    # Adjust the title position
    plot.title = element_text(hjust = 0.5),
    
    # Set y-axis to start from 0
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10)
  ) +
  
  # Set y-axis label
  ylab("") +
  
  # Set colors manually for fill and color
  scale_fill_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  scale_color_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  
  # Set y-axis limits
  coord_cartesian(ylim = c(0, 20))

# Set plot title
P1 <- P1 + ggtitle("Bryophytes BDV Richness Over Time")

# Print the plot
print(P1)

#-------------------------------------------------------------------------------
###############    LICHENS VOLUME CHANGING ################


P2 <- ggplot(Bayesian_BDV_model_V1, aes(x = year, ymin = LICHEN_PRED_RICH_2.5, ymax = LICHEN_PRED_RICH_97.5, fill = run)) +
  
  # Add ribbon layers for uncertainty with alpha = 0.3
  geom_ribbon(alpha = 0.3) +
  
  # Add lines for mean, min, and max values
  geom_line(aes(y = LICHEN_PRED_RICH_50, color = run), lwd = 0.9) +
  geom_line(aes(y = LICHEN_PRED_RICH_2.5, color = run), lwd = 0.3) +  # Change line type for min
  geom_line(aes(y = LICHEN_PRED_RICH_97.5, color = run), lwd = 0.3) +  # Change line type for max
  
  # Facet the plot based on the 'run' variable in a 2x2 matrix
  facet_wrap(~run, scales = "free", ncol = 2) +
  
  # Customize the theme of the plot
  theme_bw() +
  theme(
    # Remove major and minor grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Remove background from the plot area
    panel.background = element_blank(),
    
    # Remove legend
    legend.position = "none",
    
    # Remove strip background
    strip.background = element_blank(),
    
    # Adjust the title position
    plot.title = element_text(hjust = 0.5),
    
    # Set y-axis to start from 0
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10)
  ) +
  
  # Set y-axis label
  ylab("") +
  
  # Set colors manually for fill and color
  scale_fill_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  scale_color_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  
  # Set y-axis limits
  coord_cartesian(ylim = c(0, 80))

# Set plot title
P2 <- P2 + ggtitle("Lichens BDV Richness Over Time")

# Print the plot
print(P2)

#-------------------------------------------------------------------------------
###############    LICHENS VOLUME STATIC ################


P1 <- ggplot(Bayesian_BDV_model_V2, aes(x = year, ymin = LICHEN_PRED_RICH_2.5, ymax = LICHEN_PRED_RICH_97.5, fill = run)) +
  
  # Add ribbon layers for uncertainty with alpha = 0.3
  geom_ribbon(alpha = 0.3) +
  
  # Add lines for mean, min, and max values
  geom_line(aes(y = LICHEN_PRED_RICH_50, color = run), lwd = 0.9) +
  geom_line(aes(y = LICHEN_PRED_RICH_2.5, color = run), lwd = 0.3) +  # Change line type for min
  geom_line(aes(y = LICHEN_PRED_RICH_97.5, color = run), lwd = 0.3) +  # Change line type for max
  
  # Facet the plot based on the 'run' variable in a 2x2 matrix
  facet_wrap(~run, scales = "free", ncol = 2) +
  
  # Customize the theme of the plot
  theme_bw() +
  theme(
    # Remove major and minor grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Remove background from the plot area
    panel.background = element_blank(),
    
    # Remove legend
    legend.position = "none",
    
    # Remove strip background
    strip.background = element_blank(),
    
    # Adjust the title position
    plot.title = element_text(hjust = 0.5),
    
    # Set y-axis to start from 0
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10)
  ) +
  
  # Set y-axis label
  ylab("") +
  
  # Set colors manually for fill and color
  scale_fill_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  scale_color_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  
  # Set y-axis limits
  coord_cartesian(ylim = c(0, 80))

# Set plot title
P1 <- P1 + ggtitle("Lichens BDV Richness Over Time")

# Print the plot
print(P1)

#-------------------------------------------------------------------------------
###############    MACROFUNGI VOLUME CHANGING ################


P3 <- ggplot(Bayesian_BDV_model_V1, aes(x = year, ymin = MACROFUNGI_RICH_2.5, ymax = MACROFUNGI_PRED_RICH_97.5, fill = run)) +
  
  # Add ribbon layers for uncertainty with alpha = 0.3
  geom_ribbon(alpha = 0.3) +
  
  # Add lines for mean, min, and max values
  geom_line(aes(y = MACROFUNGI_PRED_RICH_50, color = run), lwd = 0.9) +
  geom_line(aes(y = MACROFUNGI_RICH_2.5, color = run), lwd = 0.3) +  # Change line type for min
  geom_line(aes(y = MACROFUNGI_PRED_RICH_97.5, color = run), lwd = 0.3) +  # Change line type for max
  
  # Facet the plot based on the 'run' variable in a 2x2 matrix
  facet_wrap(~run, scales = "free", ncol = 2) +
  
  # Customize the theme of the plot
  theme_bw() +
  theme(
    # Remove major and minor grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Remove background from the plot area
    panel.background = element_blank(),
    
    # Remove legend
    legend.position = "none",
    
    # Remove strip background
    strip.background = element_blank(),
    
    # Adjust the title position
    plot.title = element_text(hjust = 0.5),
    
    # Set y-axis to start from 0
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10)
  ) +
  
  # Set y-axis label
  ylab("Species Richness Prediction") +
  
  # Set colors manually for fill and color
  scale_fill_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  scale_color_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  
  # Set y-axis limits
  coord_cartesian(ylim = c(0, NA))

# Set plot title
P3 <- P3 + ggtitle("Macrofungi BDV Richness Over Time")

# Print the plot
print(P3)


# Plot grid arrange
grid.arrange(ba_broadl, trees_10_40, P1, ncol=1)

########################################################## CLOSE EVERY PLOTs

dev.off()
#-------------------------------------------------------------------------------
###############    MACROFUNGI VOLUME STATIC ################

P3 <- ggplot(Bayesian_BDV_model_V2, aes(x = year, ymin = MACROFUNGI_RICH_2.5, ymax = MACROFUNGI_PRED_RICH_97.5, fill = run)) +
  
  # Add ribbon layers for uncertainty with alpha = 0.3
  geom_ribbon(alpha = 0.3) +
  
  # Add lines for mean, min, and max values
  geom_line(aes(y = MACROFUNGI_PRED_RICH_50, color = run), lwd = 0.9) +
  geom_line(aes(y = MACROFUNGI_RICH_2.5, color = run), lwd = 0.3) +  # Change line type for min
  geom_line(aes(y = MACROFUNGI_PRED_RICH_97.5, color = run), lwd = 0.3) +  # Change line type for max
  
  # Facet the plot based on the 'run' variable in a 2x2 matrix
  facet_wrap(~run, scales = "free", ncol = 2) +
  
  # Customize the theme of the plot
  theme_bw() +
  theme(
    # Remove major and minor grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Remove background from the plot area
    panel.background = element_blank(),
    
    # Remove legend
    legend.position = "none",
    
    # Remove strip background
    strip.background = element_blank(),
    
    # Adjust the title position
    plot.title = element_text(hjust = 0.5),
    
    # Set y-axis to start from 0
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10)
  ) +
  
  # Set y-axis label
  ylab("") +
  
  # Set colors manually for fill and color
  scale_fill_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  scale_color_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  
  # Set y-axis limits
  coord_cartesian(ylim = c(0, NA))

# Set plot title
P3 <- P3 + ggtitle("Macrofungi BDV Richness Over Time")

# Print the plot
print(P1)


#-------------------------------------------------------------------------------
###############    MACROFUNGI RED LIST VOLUME CHANGING ################


P4 <- ggplot(Bayesian_BDV_model_V1, aes(x = year, ymin = MACROFUNGI_RED_RICH_2.5, ymax = MACROFUNGI_RED_PRED_RICH_97.5, fill = run)) +
  
  # Add ribbon layers for uncertainty with alpha = 0.3
  geom_ribbon(alpha = 0.3) +
  
  # Add lines for mean, min, and max values
  geom_line(aes(y = MACROFUNGI_RED_PRED_RICH_50, color = run), lwd = 0.9) +
  geom_line(aes(y = MACROFUNGI_RED_RICH_2.5, color = run), lwd = 0.3) +  # Change line type for min
  geom_line(aes(y = MACROFUNGI_RED_PRED_RICH_97.5, color = run), lwd = 0.3) +  # Change line type for max
  
  # Facet the plot based on the 'run' variable in a 2x2 matrix
  facet_wrap(~run, scales = "free", ncol = 2) +
  
  # Customize the theme of the plot
  theme_bw() +
  theme(
    # Remove major and minor grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Remove background from the plot area
    panel.background = element_blank(),
    
    # Remove legend
    legend.position = "none",
    
    # Remove strip background
    strip.background = element_blank(),
    
    # Adjust the title position
    plot.title = element_text(hjust = 0.5),
    
    # Set y-axis to start from 0
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10)
  ) +
  
  # Set y-axis label
  ylab("Species Richness Prediction") +
  
  # Set colors manually for fill and color
  scale_fill_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  scale_color_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  
  # Set y-axis limits
  coord_cartesian(ylim = c(0, NA))

# Set plot title
P4 <- P4 + ggtitle("Macrofungi IUCN Red List BDV Richness Overweighted")

# Print the plot
print(P4)

# Plot grid arrange
grid.arrange(volume_DW, ba_broadl,P4, ncol=1)

########################################################## CLOSE EVERY PLOTs

dev.off()
#-------------------------------------------------------------------------------
###############    MACROFUNGI RED VOLUME STATIC ################

P1 <- ggplot(Bayesian_BDV_model_V2, aes(x = year, ymin = MACROFUNGI_RED_RICH_2.5, ymax = MACROFUNGI_RED_PRED_RICH_97.5, fill = run)) +
  
  # Add ribbon layers for uncertainty with alpha = 0.3
  geom_ribbon(alpha = 0.3) +
  
  # Add lines for mean, min, and max values
  geom_line(aes(y = MACROFUNGI_RED_PRED_RICH_50, color = run), lwd = 0.9) +
  geom_line(aes(y = MACROFUNGI_RED_RICH_2.5, color = run), lwd = 0.3) +  # Change line type for min
  geom_line(aes(y = MACROFUNGI_RED_PRED_RICH_97.5, color = run), lwd = 0.3) +  # Change line type for max
  
  # Facet the plot based on the 'run' variable in a 2x2 matrix
  facet_wrap(~run, scales = "free", ncol = 2) +
  
  # Customize the theme of the plot
  theme_bw() +
  theme(
    # Remove major and minor grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Remove background from the plot area
    panel.background = element_blank(),
    
    # Remove legend
    legend.position = "none",
    
    # Remove strip background
    strip.background = element_blank(),
    
    # Adjust the title position
    plot.title = element_text(hjust = 0.5),
    
    # Set y-axis to start from 0
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10)
  ) +
  
  # Set y-axis label
  ylab("") +
  
  # Set colors manually for fill and color
  scale_fill_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  scale_color_manual(values=c("chocolate3","chocolate3", "chocolate3", "chocolate3"))+
  
  # Set y-axis limits
  coord_cartesian(ylim = c(0, NA))

# Set plot title
P1 <- P1 + ggtitle("Macrofungi IUCN Red List BDV Richness Overweighted")

# Print the plot
print(P1)


#-------------------------------

#
dev.off()
#


# CREATE A PDF FOR THE STATITICIAN CHECKING

# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/outputs/20231129/"
pdf(paste0(dataroot, "20231211_Scatter_plot_variables_BDV_mng_plot_L1_10_300.pdf"), height=8, width=12)

#-------------------------------------------------------------------------------
# Create a scatter plot to compare the points position in the space between the real values and the simulated ones
# Archaic way

par(mfrow = c(1,2))

plot(Bayesian_BDV_model_bryophytes_V1$BRYO_PRED_RICH_50, Bayesian_BDV_model_bryophytes_V1$volume_DW, xlim = c(5,35))
plot(BDV_predictors$`Epiphytic / epixilic bryophytes (0.212)`, BDV_predictors$volume_dw, xlim = c(5,35))


par(mfrow = c(1,2))

plot(Bayesian_BDV_model_bryophytes_V1$MACROFUNGI_PRED_RICH_50, Bayesian_BDV_model_bryophytes_V1$volume_DW, xlim = c(5,35))
plot(BDV_predictors$`Epiphytic / epixilic bryophytes (0.212)`, BDV_predictors$volume_dw, xlim = c(5,35))


#------------------------------------------------------------------------------
# Make THE SCATTER PLOT FULL BLACK

g1 <- ggplot(BDV_predictors, aes(volume_dw, `Epiphytic / epixilic bryophytes (0.212)`)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Bryophytes sp - Volume Deadwood") +
  labs(x = "Volume Deadwood [m3/ha]", y = " N. of Species", fill = "management") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 40) +
  xlim(0, 100) +
  theme_bw()


g2 <- ggplot(BDV_predictors, aes(volume_dw, `Macrofungi (2.118)`)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Macrofungi sp - Volume Deadwood") +
  labs(x = "Volume Deadwood [m3/ha]", y = " N. of Species", fill = "management") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 300) +
  xlim(0, 100) +
  theme_bw()

g3 <- ggplot(Bayesian_BDV_model_V1, aes(volume_DW, BRYO_PRED_RICH_50)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Bryophytes sp - Volume Deadwood") +
  labs(x = "Volume Deadwood [m3/ha]", y = " N. of Species") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 40) +
  xlim(0, 100) +
  theme_bw()

g4 <- ggplot(Bayesian_BDV_model_V1, aes(volume_DW, MACROFUNGI_PRED_RICH_50)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Macrofungi sp - Volume Deadwood") +
  labs(x = "Volume Deadwood [m3/ha]", y = " N. of Species") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 300) +
  xlim(0, 100) +
  theme_bw()


# Plot grid arrange
grid.arrange(g1,g2,g3,g4, ncol=2)
#


#-------------------------------------------------------------------------------
# Make the scatter plot by color for mang type 
# MACROFUNGI and BRYOPHYTES AGAINST DEADVOLUME 

g1 <- ggplot(BDV_predictors, aes(volume_dw, `Epiphytic / epixilic bryophytes (0.212)`, color = management)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Bryophytes sp - Volume Deadwood") +
  labs(x = "Volume Deadwood [m3/ha]", y = " N. of Species by plotID") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 40) +
  xlim(0, 100) +
  theme_bw()


g2 <- ggplot(BDV_predictors, aes(volume_dw, `Macrofungi (2.118)`, color = management)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Macrofungi sp - Volume Deadwood") +
  labs(x = "Volume Deadwood [m3/ha]", y = " N. of Species by plotID") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 300) +
  xlim(0, 100) +
  theme_bw()

g3 <- ggplot(Bayesian_BDV_model_V1, aes(volume_DW, BRYO_PRED_RICH_50)) +
  geom_point() +
  ggtitle("Scatter Plot Simulated Values Bryophytes sp - Volume Deadwood") +
  labs(x = "Volume Deadwood [m3/ha]", y = " N. of Species by year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 40) +
  xlim(0, 100) +
  theme_bw()

g4 <- ggplot(Bayesian_BDV_model_V1, aes(volume_DW, MACROFUNGI_PRED_RICH_50)) +
  geom_point() +
  ggtitle("Scatter Plot Simulated Values Macrofungi sp - Volume Deadwood") +
  labs(x = "Volume Deadwood [m3/ha]", y = " N. of Species by year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 300) +
  xlim(0, 100) +
  theme_bw()

# Plot grid arrange
grid.arrange(g1,g2,g3,g4, ncol=2)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Make the scatter plot by color for mang type 
# MACROFUNGI and BRYOPHYTES AGAINST AGE

g1 <- ggplot(BDV_predictors, aes(age, `Epiphytic / epixilic bryophytes (0.212)`, color = management)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Bryophytes SP - Mean Age") +
  labs(x = "Mean Age [year]", y = " N. of Species by plotID") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 40) +
  xlim(0, 300) +
  theme_bw()


g2 <- ggplot(BDV_predictors, aes(age, `Macrofungi (2.118)`, color = management)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Macrofungi SP - Mean Age") +
  labs(x = "Mean Age [year]", y = " N. of Species by plotID") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 300) +
  xlim(0, 300) +
  theme_bw()

g3 <- ggplot(Bayesian_BDV_model_V1, aes(age, BRYO_PRED_RICH_50)) +
  geom_point() +
  ggtitle("Scatter Plot Simulated Values Bryophytes SP - Mean Age over time") +
  labs(x = "Mean Age [year]", y = " N. of Species by year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 40) +
  xlim(0, 300) +
  theme_bw()

g4 <- ggplot(Bayesian_BDV_model_V1, aes(age, MACROFUNGI_PRED_RICH_50)) +
  geom_point() +
  ggtitle("Scatter Plot Simulated Values Macrofungi SP - Mean Age over Time") +
  labs(x = "Mean Age [year]", y = " N. of Species by year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 300) +
  xlim(0, 300) +
  theme_bw()

# Plot grid arrange
grid.arrange(g1,g2,g3,g4, ncol=2)

#-------------------------------------------------------------------------------
# Make the scatter plot by color for mang type 
# MACROFUNGI and BRYOPHYTES AGAINST SIMULATED LAI

g1 <- ggplot(BDV_predictors, aes(lai_sim, `Epiphytic / epixilic bryophytes (0.212)`, color = management)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Bryophytes SP - LAI") +
  labs(x = "Leaf Area Index [m2/m2]", y = " N. of Species by plotID") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 40) +
  xlim(0, 10) +
  theme_bw()


g2 <- ggplot(BDV_predictors, aes(lai_sim, `Macrofungi (2.118)`, color = management)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Macrofungi SP - LAI") +
  labs(x = "Leaf Area Index [m2/m2]", y = " N. of Species by plotID") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 300) +
  xlim(0, 10) +
  theme_bw()

g3 <- ggplot(Bayesian_BDV_model_V1, aes(lai_sim, BRYO_PRED_RICH_50)) +
  geom_point() +
  ggtitle("Scatter Plot Simulated Values Bryophytes SP - LAI over time") +
  labs(x = "Leaf Area Index [m2/m2]", y = " N. of Species by year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 40) +
  xlim(0, 10) +
  theme_bw()

g4 <- ggplot(Bayesian_BDV_model_V1, aes(lai_sim, MACROFUNGI_PRED_RICH_50)) +
  geom_point() +
  ggtitle("Scatter Plot Simulated Values Macrofungi SP - LAI over Time") +
  labs(x = "Leaf Area Index [m2/m2]", y = " N. of Species by year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 300) +
  xlim(0, 10) +
  theme_bw()

# Plot grid arrange
grid.arrange(g1,g2,g3,g4, ncol=2)

#-------------------------------------------------------------------------------
# Make the scatter plot by color for mang type 
# MACROFUNGI and BRYOPHYTES AGAINST Basal Area Broadleave

g1 <- ggplot(BDV_predictors, aes(ba_broadl, `Epiphytic / epixilic bryophytes (0.212)`, color = management)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Bryophytes SP - Basal Area Broadleave SP") +
  labs(x = "Basal Area Broadleave sp [m2]", y = " N. of Species by plotID") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 40) +
  xlim(0, 15) +
  theme_bw()


g2 <- ggplot(BDV_predictors, aes(ba_broadl, `Macrofungi (2.118)`, color = management)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Macrofungi SP - Basal Area Broadleave SP") +
  labs(x = "Basal Area Broadleave sp [m2]", y = " N. of Species by plotID") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 300) +
  xlim(0, 15) +
  theme_bw()

g3 <- ggplot(Bayesian_BDV_model_V1, aes(ba_broadl, BRYO_PRED_RICH_50)) +
  geom_point() +
  ggtitle("Scatter Plot Simulated Values Bryophytes SP - Basal Area Broadleave SP over time") +
  labs(x = "Basal Area Broadleave sp [m2]", y = " N. of Species by year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 40) +
  xlim(0, 15) +
  theme_bw()

g4 <- ggplot(Bayesian_BDV_model_V1, aes(ba_broadl, MACROFUNGI_PRED_RICH_50)) +
  geom_point() +
  ggtitle("Scatter Plot Simulated Values Macrofungi SP - Basal Area Broadleave SP over Time") +
  labs(x = "Basal Area Broadleave sp [m2]", y = " N. of Species by year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 300) +
  xlim(0, 15) +
  theme_bw()

# Plot grid arrange
grid.arrange(g1,g2,g3,g4, ncol=2)


#-------------------------------------------------------------------------------
# Make the scatter plot by color for mang type 
# MACROFUNGI and BRYOPHYTES AGAINST Trees between 10 and 40 cm DBH

g1 <- ggplot(BDV_predictors, aes(trees_10_40, `Epiphytic / epixilic bryophytes (0.212)`, color = management)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Bryophytes SP - Trees between 10 and 40 cm DBH") +
  labs(x = "Trees between 10 and 40 cm DBH [No.]", y = " N. of Species by plotID") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 40) +
  xlim(0, 550) +
  theme_bw()


g2 <- ggplot(BDV_predictors, aes(trees_10_40, `Macrofungi (2.118)`, color = management)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Macrofungi SP - Trees between 10 and 40 cm DBH") +
  labs(x = "Trees between 10 and 40 cm DBH [No.]", y = " N. of Species by plotID") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 300) +
  xlim(0, 550) +
  theme_bw()

g3 <- ggplot(Bayesian_BDV_model_V1, aes(trees_10_40, BRYO_PRED_RICH_50)) +
  geom_point() +
  ggtitle("Scatter Plot Simulated Values Bryophytes SP - Trees between 10 and 40 cm DBH over time") +
  labs(x = "Trees between 10 and 40 cm DBH [No.]", y = " N. of Species by year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 40) +
  xlim(0, 550) +
  theme_bw()

g4 <- ggplot(Bayesian_BDV_model_V1, aes(trees_10_40, MACROFUNGI_PRED_RICH_50)) +
  geom_point() +
  ggtitle("Scatter Plot Simulated Values Macrofungi SP - Trees between 10 and 40 cm DBH over Time") +
  labs(x = "Trees between 10 and 40 cm DBH [No.]", y = " N. of Species by year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 300) +
  xlim(0, 550) +
  theme_bw()

# Plot grid arrange
grid.arrange(g1,g2,g3,g4, ncol=2)

#-------------------------------------------------------------------------------
# Make the scatter plot by color for mang type 
# MACROFUNGI and BRYOPHYTES AGAINST Trees Broadleave > 40 cm DBH

g1 <- ggplot(BDV_predictors, aes(`broadl>40`, `Epiphytic / epixilic bryophytes (0.212)`, color = management)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Bryophytes SP - Trees Broadleave > 40 cm DBH") +
  labs(x = "Trees Broadleave > 40 cm DBH [No.]", y = " N. of Species by plotID") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 40) +
  xlim(0, 50) +
  theme_bw()


g2 <- ggplot(BDV_predictors, aes(`broadl>40`, `Macrofungi (2.118)`, color = management)) +
  geom_point() +
  ggtitle("Scatter Plot Real Values Macrofungi SP - Trees Broadleave > 40 cm DBH") +
  labs(x = "Trees Broadleave > 40 cm DBH [No.]", y = " N. of Species by plotID") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 300) +
  xlim(0, 50) +
  theme_bw()

g3 <- ggplot(Bayesian_BDV_model_V1, aes(broadl_40, BRYO_PRED_RICH_50)) +
  geom_point() +
  ggtitle("Scatter Plot Simulated Values Bryophytes SP - Trees Broadleave > 40 cm DBH over time") +
  labs(x =  "Trees Broadleave > 40 cm DBH [No.]", y = " N. of Species by year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 40) +
  xlim(0, 50) +
  theme_bw()

g4 <- ggplot(Bayesian_BDV_model_V1, aes(broadl_40, MACROFUNGI_PRED_RICH_50)) +
  geom_point() +
  ggtitle("Scatter Plot Simulated Values Macrofungi SP - Trees Broadleave > 40 cm DBH over Time") +
  labs(x =  "Trees Broadleave > 40 cm DBH [No.]", y = " N. of Species by year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 300) +
  xlim(0, 50) +
  theme_bw()

# Plot grid arrange
grid.arrange(g1,g2,g3,g4, ncol=2)

#-------------------------------------------------------------------------------






# Create and plot the Bayesian coefficients by predictor time series against the predicted species richness 

#-------------------------------------------------------------------------------
# Bryophytes

b1 <- ggplot(Bayesian_BDV_model_V1, aes(year, BRYO_PRED_RICH_50_beta1)) +
  geom_line() +
  ggtitle("Isolated [Beta 1 * AGE] BRYOPHYTES Time Series") +
  labs(x =  "Year", y = " Beta 1 * Age") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~run, ncol=2)+
  theme_bw()

b2 <- ggplot(Bayesian_BDV_model_V1, aes(year, BRYO_PRED_RICH_50_beta2)) +
  geom_line() +
  ggtitle("Isolated [Beta 2 * Deaewood Volume] BRYOPHYTES Time Series") +
  labs(x =  "Year", y = " Beta 2 * DW Vol") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~run, ncol=2)+
  theme_bw()


# Plot grid arrange
grid.arrange(b1,b2,P1, ncol=1)

# IN CASE WANNA PLOT THEM TOGETHER
# Plotting beta1 and beta2 over time
B1 <- ggplot(Bayesian_BDV_model_V1, aes(x = year)) +
  geom_line(aes(y = BRYO_PRED_RICH_50_beta1, color = "Beta 1 age"), size = 0.5) +
  geom_line(aes(y = BRYO_PRED_RICH_50_beta2, color = "Beta 2 deadwood"), size = 0.5) +
  geom_line(aes(y = BRYO_PRED_RICH_50, color = "Y"), size = 0.5) +
  labs(title = "Time Series of Bryophytes in function of Beta 1 and Beta 2",
       x = "Year",
       y = "Values") +
  theme_minimal() +
  facet_wrap(~run, ncol=2)+
  scale_color_manual(values = c("Beta 1 age" = "blue", "Beta 2 deadwood" = "black", "Y" = "chocolate3")) +
  scale_x_continuous(breaks = seq(min(Bayesian_BDV_model_V1$year), max(Bayesian_BDV_model_V1$year), by = 20),
                     labels = seq(min(Bayesian_BDV_model_V1$year), max(Bayesian_BDV_model_V1$year), by = 20))+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

# Plot grid arrange
grid.arrange(C1,P1, ncol=1)


#-------------------------------------------------------------------------------
# Lichens

# Plotting beta1, beta2, ... etc over time with Y

L1 <- ggplot(Bayesian_BDV_model_V1, aes(x = year)) +
  geom_line(aes(y = LICHEN_PRED_RICH_50_beta1, color = "Beta 1 age"), size = 0.5) +
  geom_line(aes(y = LICHEN_PRED_RICH_50_beta2, color = "Beta 2 deadwood"), size = 0.5) +
  geom_line(aes(y = LICHEN_PRED_RICH_50_beta3, color = "Beta 3 lai_sim"), size = 0.5) +
  geom_line(aes(y = LICHEN_PRED_RICH_50_beta4, color = "Beta 4 broadl_40_1"), size = 0.5) +
  geom_line(aes(y = LICHEN_PRED_RICH_50_beta5, color = "Beta 5 broadl_40_2"), size = 0.5) +
  geom_line(aes(y = LICHEN_PRED_RICH_50, color = "Y"), size = 0.5) +
  labs(title = "Time Series of Lichens in function of Beta 1 to 5",
       x = "Year",
       y = "Values") +
  theme_minimal() +
  facet_wrap(~run, ncol=2)+
  scale_color_manual(values = c("Beta 1 age" = "blue", "Beta 2 deadwood" = "black", "Beta 3 lai_sim" = "#8FBC8F", "Beta 4 broadl_40_1" = "#FF1493","Beta 5 broadl_40_2" = "mediumpurple4",  "Y" = "chocolate3")) +
  scale_x_continuous(breaks = seq(min(Bayesian_BDV_model_V1$year), max(Bayesian_BDV_model_V1$year), by = 20),
                     labels = seq(min(Bayesian_BDV_model_V1$year), max(Bayesian_BDV_model_V1$year), by = 20))+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

# Plot grid arrange
grid.arrange(L1,P1, ncol=1)


#-------------------------------------------------------------------------------
# Macrofungi

# Plotting beta1, beta2, ... etc over time with Y

F1 <- ggplot(Bayesian_BDV_model_V1, aes(x = year)) +
  geom_line(aes(y = MACROFUNGI_PRED_RICH_50_beta1, color = "Beta 1 age"), size = 0.5) +
  geom_line(aes(y = MACROFUNGI_PRED_RICH_50_beta2, color = "Beta 2 deadwood"), size = 0.5) +
  geom_line(aes(y = MACROFUNGI_PRED_RICH_50_beta3, color = "Beta 3 ba_broadl"), size = 0.5) +
  geom_line(aes(y = MACROFUNGI_PRED_RICH_50_beta4, color = "Beta 4 trees_10_40"), size = 0.5) +
  geom_line(aes(y = MACROFUNGI_PRED_RICH_50, color = "Y"), size = 0.5) +
  labs(title = "Time Series of Macrofungi in function of Beta 1 to 4",
       x = "Year",
       y = "Values") +
  theme_minimal() +
  facet_wrap(~run, ncol=2)+
  scale_color_manual(values = c("Beta 1 age" = "blue", "Beta 2 deadwood" = "black", "Beta 3 ba_broadl" = "#8FBC8F", "Beta 4 trees_10_40" = "#FF1493",  "Y" = "chocolate3")) +
  scale_x_continuous(breaks = seq(min(Bayesian_BDV_model_V1$year), max(Bayesian_BDV_model_V1$year), by = 20),
                     labels = seq(min(Bayesian_BDV_model_V1$year), max(Bayesian_BDV_model_V1$year), by = 20))+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

# Plot grid arrange
grid.arrange(L1,P1, ncol=1)


#-------------------------------------------------------------------------------
# Red Listed - Macrofungi

# Plotting beta1, beta2, ... etc over time with Y

RM1 <- ggplot(Bayesian_BDV_model_V1, aes(x = year)) +
  geom_line(aes(y = MACROFUNGI_RED_PRED_RICH_50_beta1, color = "Beta 1 age"), size = 0.5) +
  geom_line(aes(y = MACROFUNGI_RED_PRED_RICH_50_beta2, color = "Beta 2 deadwood"), size = 0.5) +
  geom_line(aes(y = MACROFUNGI_RED_PRED_RICH_50_beta3, color = "Beta 3 ba_broadl"), size = 0.5) +
  geom_line(aes(y = MACROFUNGI_RED_PRED_RICH_50, color = "Y"), size = 0.5) +
  labs(title = "Time Series of Red List Macrofungi in function of Beta 1 to 3",
       x = "Year",
       y = "Values") +
  theme_minimal() +
  facet_wrap(~run, ncol=2)+
  scale_color_manual(values = c("Beta 1 age" = "blue", "Beta 2 deadwood" = "black", "Beta 3 ba_broadl" = "#8FBC8F",  "Y" = "chocolate3")) +
  scale_x_continuous(breaks = seq(min(Bayesian_BDV_model_V1$year), max(Bayesian_BDV_model_V1$year), by = 20),
                     labels = seq(min(Bayesian_BDV_model_V1$year), max(Bayesian_BDV_model_V1$year), by = 20))+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

# Plot grid arrange
grid.arrange(L1,P1, ncol=1)


#---------------------------------------------------------------------------------
