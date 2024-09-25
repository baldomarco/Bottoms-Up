
# Load required libraries
library(tidyr)
library(dplyr)
library(RSQLite)
library(vegan)
library(fields)

setwd("C:/iLand/2023/20230901_Bottoms_Up/outputs/20240703/Test_unmanaged_wind_all_plots/2020903/output/")

# Path to the directory containing your SQLite databases
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/outputs/20240703/Test_unmanaged_wind_all_plots/2020903/output/"

# Get a list of all SQLite databases in the directory
# database_files <- list.files(path = dataroot, pattern = ".sqlite", full.names = TRUE)

setwd("C:/iLand/2023/20230901_Bottoms_Up/outputs/20240905/20240907/test/")

# Path to the directory containing your SQLite databases
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/outputs/20240905/20240907/test/"

{# Create an empty list to store data frames
  dfs <- list() # not working for several subset, only one
}


# Create the list of dataframes I want to save

abeStand_scen <- c()

bb_scen <- c()

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

#-------------------------------------------------------------------------------
# import the list of files within the folder in dataroot with .sqlite extension

database_files <- list.files(path = dataroot, pattern = ".sqlite", full.names = FALSE)                              # alternative way to select all the databases within a folder select all the file with sqlite format
database_files <- list.files(dataroot, ".sqlite")  

for (i in (1:length(database_files)))  {    # We read in the files in the loop. The "i" is for the x from 1 to i lenght of the dataset of files 
  
  #i<-1
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
  tables.in.the.file <- dbListTables(db)         # explore the tables in the file
  print(tables.in.the.file)
  
  #-----------------------------------------------------------------------------
  # LOAD THE DATABASE # Read tables from the database
  abeStand <- dbReadTable(db, "abeStand")
  barkbeetle <- dbReadTable(db, "barkbeetle")
  carbon <- dbReadTable(db, "carbon")
  carbonflow <- dbReadTable(db, "carbonflow")
  dynamicstand <- dbReadTable(db, "dynamicstand")
  landscape <- dbReadTable(db, "landscape")
  landscape_removed <- dbReadTable(db, "landscape_removed")
  stand <- dbReadTable(db, "stand")
  tree <- dbReadTable(db, "tree")
  
  # Check if the "wind" table is present before reading it
  if ("wind" %in% tables.in.the.file) {
    wind <- dbReadTable(db, "wind")
  }
  
  dbDisconnect(db)    # close the file
  
  # TO UNDERSTAND THE OPERATORS %>% AND %IN% HAVE TO STUDY THEM IN DATACAMP AND IN DPLYR CRAN PACKAGES
  
  #-----------------------------------------------------------------------------
  # CREATE SHANNON VARIABLE
  
  # CREATE A TABLE FOR THE ANNUAL VOL,BA,COUNT DATA
  annual.data<-landscape %>% 
    group_by(year) %>% 
    filter(year>0) %>% 
    summarize(VOL.tot=sum(volume_m3), BA.tot=sum(basal_area_m2), count.tot=sum(count_ha))
  
  # SAME BUT PER SPECIES
  annual.spec.data<-landscape %>% 
    group_by(year, species) %>%  
    filter(year>0) %>% 
    summarize(VOL=(volume_m3), BA=(basal_area_m2), count=sum(count_ha))
  
  # print(head(annual.data))
  # print(head(annual.spec.data))
  
  S<-landscape %>% 
    group_by(year) %>%                        # BECARFUL THE NUMBERS OF VALUES CAN BE LESS THEN THE NUMBER OF YEARS IN YOUNG STANDS.
    filter(volume_m3>0 & year>0) %>% 
    summarise(n=n())   
  
  # number of species in each year  (added the filter to count non-zero volumes, now it is okay)
  
  #print(S$n)
  #length(S$n)
  
  t<-annual.spec.data %>% right_join(annual.data, by="year")  %>% 
    mutate(prop.VOL=VOL/VOL.tot, prop.BA=BA/BA.tot, prop.count=count/count.tot) %>% 
    filter(prop.VOL>0)  # here I also filtering them to have only records with m3>0
  
  #https://www.statology.org/shannon-diversity-index/
  #https://www.statology.org/shannon-diversity-index-calculator/
  
  # Shannon diversity index (SDI): this is already Shannon.... that extra step by dividing the by S$n is making equitability index based on the link above.
  # so maybe we can make shannon based on BA, VOL and number of trees.... (? can discuss or save all 3 and will see...)
  
  # Calculate H.BA
  H.BA <- t %>%
    group_by(year) %>%
    summarize(H = -1 * sum(prop.BA * log(prop.BA)))
  
  # Calculate H.VOL
  H.VOL <- t %>%
    group_by(year) %>%
    summarize(H = -1 * sum(prop.VOL * log(prop.VOL)))
  
  # Calculate H.count
  H.count <- t %>%
    group_by(year) %>%
    summarize(H = -1 * sum(prop.count * log(prop.count)))
  
  # Find the maximum year across all dataframes
  max_year <- max(
    max(H.BA$year, na.rm = TRUE),
    max(H.VOL$year, na.rm = TRUE),
    max(H.count$year, na.rm = TRUE)
  )
  
  # Create a reference dataframe with all years from 1 to the maximum year
  reference_df <- tibble(year = 1:max(H.BA$year, H.VOL$year, H.count$year))
  
  
  # Left join each dataframe with the reference dataframe and replace NA with 0
  H.BA <- reference_df %>%
    left_join(H.BA, by = "year") %>%
    replace(is.na(.), 0)
  
  H.VOL <- reference_df %>%
    left_join(H.VOL, by = "year") %>%
    replace(is.na(.), 0)
  
  H.count <- reference_df %>%
    left_join(H.count, by = "year") %>%
    replace(is.na(.), 0)
  
  # here I just put the proportion of number of trees
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
  
  esp<-data.frame(year=c(1:max(landscape$year))) # FILL THE MISSING YEARS WITH ZERO
  esp<-esp %>% left_join(early.spec.prop, by="year")
  esp$BA[which(is.na(esp$BA)==T)]<-0
  
  #-----------------------------------------------------------------------------
  # Create Shannon on BA heterogeneity with VEGAN
  
  # Calculate the Shannon diversity index
  H_BA_heterogenity <- t %>%
    group_by(year) %>%
    summarize(shannon_ba_heterog = diversity(prop.BA, base = exp(1)))
  
  # Print the resulting dataframe
  print(H_BA_heterogenity)
  
  # PLOT THE EARLY SPECIES PROPORTION AND THE SHANNON EQUITABILITY INDEX
  set.panel(3,1)
  plot(SEI)
  plot(esp$year,esp$BA)
  plot(H_BA_heterogenity$year, H_BA_heterogenity$shannon_ba_heterog)
  
  
  
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
  
  
  # THIS IS NEEDED FOR MAKE THE DATA FRAME dynamicstand OF THE SAME SIZE OF THE carbon AND carbonflow. WE DID THE SAME FOR THE SELECTED VARIABLES IN LANDSCAPE
  dynamicstand_1 <- dynamicstand %>%
    select(year, height_mean, dbh_mean, age_mean)
  
  dynamicstand_1 <- tibble(
    year = setdiff(1:max(dynamicstand_1$year), dynamicstand_1$year),
    height_mean = 0,
    dbh_mean = 0,
    age_mean = 0
  ) %>%
    bind_rows(dynamicstand_1) %>%
    arrange(year) %>%
    mutate(
      height_mean = replace(height_mean, is.na(height_mean), 0),
      dbh_mean = replace(dbh_mean, is.na(dbh_mean), 0),
      age_mean = replace(age_mean, is.na(age_mean), 0)
    ) %>%
    filter(year > 0) %>%
    select(year, height_mean, dbh_mean, age_mean)
  
  # CREATE THE NEW DATA FRAME FOR VARIABLES 
  
  variables <- data.frame(case=case,
                          year=H.VOL$year,
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
  # variables = inner_join(ab.lnd.v, ab.tot.c, by="year")
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
  tree_10_40 <- tree %>%
    filter(dbh >= dbh_min, dbh <= dbh_max) %>%
    group_by(year) %>%
    summarise(tree_10_40 = n()) %>%
    complete(year = full_seq(year, 1)) %>%
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
    summarize(basal_area_m2 = sum(basal_area_m2))%>%
    select(year,basal_area_m2)
  
  #-------------------------------------------------------------------------------
  # Calculate the basal area only of the broadleave with a dbh > 40cm
  
  # Create a data frame with all unique years
  all_years <- data.frame(year = unique(landscape$year))
  
  # Perform left join with the summarization result
  # Create the broadl_40 summary with continuous years and fill missing values with 0
  broadl_40 <- tree %>%
    filter(dbh > 40 & !species %in% species_to_remove) %>%
    group_by(year) %>%
    summarise(broadl_40 = n()) %>%
    complete(year = seq(min(year), max(year), by = 1), fill = list(broadl_40 = 0))
  
  # Join with all_years and replace any NA values with 0
  broadl_40 <- all_years %>%
    left_join(broadl_40, by = "year") %>%
    replace(is.na(.), 0)
  
  #-------------------------------------------------------------------------------
  # Age - comes from the avarage age
  
  # Summarize the mean age for each year and round the ages
  stand_age <- tree %>%
    group_by(year) %>%
    summarize(age = mean(age)) %>%
    mutate(age = round(age))
  
  # Create a complete sequence of years and fill missing ages with 0
  stand_age <- stand_age %>%
    complete(year = seq(min(year), max(year), by = 1), fill = list(age = 0))
  
  #-------------------------------------------------------------------------------
  # DW volume - USE snags_c divided by 4 and converted into volume 
  # Alternative use the Carbon instead that volume
  
  #-----------------------------------------------------------------------------
  # CREATE THE DATA FRAME FOR ADD VARIABLES ABOUT CARBON IN THE FINAL DATA FRAME
  
  # Total Carbon
  totalC_kgha_iland <- data.frame(carbon %>% 
                                    group_by(year) %>% 
                                    summarise(totalC_kgha_iland=sum(stem_c, branch_c, foliage_c, coarseRoot_c, fineRoot_c, regeneration_c, snags_c, snagsOther_c, downedWood_c, litter_c, soil_c)))
  
  # Total living carbon
  total_alive_C_sim <- data.frame(carbon %>% 
                                    group_by(year) %>% 
                                    summarise(total_alive_C_sim=sum(stem_c, branch_c, foliage_c, coarseRoot_c, fineRoot_c, regeneration_c)))
  
  # Total stems carbon
  total_stem_C_sim <- data.frame(carbon %>% 
                                   group_by(year) %>% 
                                   summarise(total_stem_C_sim=sum(stem_c)))
  
  # DW carbon total
  total_DW_C_kgha <- data.frame(carbon %>% 
                                  group_by(year) %>% 
                                  summarise(total_DW_C_kgha=sum(snags_c, snagsOther_c, downedWood_c)))
  
  # Total Aboveground DW Carbon
  total_AG_DW_C_sim <- data.frame(carbon %>% 
                                    group_by(year) %>% 
                                    summarise(total_AG_DW_C_sim=sum(snags_c, snagsOther_c_ag, downedWood_c_ag)))
  
  # Total Snags Carbon
  standing_DW_C <- data.frame(carbon %>% 
                                group_by(year) %>% 
                                summarise(standing_DW_C = sum(snags_c))) 
  

  
  # Create a new row with manually specified values
  new_row_1 <- c(0, standing_DW_C %>% filter(year == 1)%>% pull(standing_DW_C))       # Add your values accordingly
  new_row_2 <- c(0, total_DW_C_kgha %>% filter(year == 1)%>% pull(total_DW_C_kgha))     # Add your values accordingly : original 749+233+225
  new_row_3 <- c(0, totalC_kgha_iland %>% filter(year == 1)%>% pull(totalC_kgha_iland))   # Add your values accordingly
  new_row_4 <- c(0, total_alive_C_sim %>% filter(year == 1)%>% pull(total_alive_C_sim))
  new_row_5 <- c(0, total_stem_C_sim %>% filter(year == 1)%>% pull(total_stem_C_sim))
  new_row_6 <- c(0, total_AG_DW_C_sim %>% filter(year == 1)%>% pull(total_AG_DW_C_sim))
  
  # Convert the new row to a data frame
  new_row_1_df <- as.data.frame(t(new_row_1))
  colnames(new_row_1_df) <- colnames(standing_DW_C)
  
  new_row_2_df <- as.data.frame(t(new_row_2))
  colnames(new_row_2_df) <- colnames(total_DW_C_kgha)
  
  new_row_3_df <- as.data.frame(t(new_row_3))
  colnames(new_row_3_df) <- colnames(totalC_kgha_iland)
  
  new_row_4_df <- as.data.frame(t(new_row_4))
  colnames(new_row_4_df) <- colnames(total_alive_C_sim)
  
  new_row_5_df <- as.data.frame(t(new_row_5))
  colnames(new_row_5_df) <- colnames(total_stem_C_sim)
  
  new_row_6_df <- as.data.frame(t(new_row_6))
  colnames(new_row_6_df) <- colnames(total_AG_DW_C_sim)
  
  # Add the new row at the beginning of the data frame in all Carbon variables
  totalC_kgha_iland <- rbind(new_row_3_df, totalC_kgha_iland)
  total_DW_C_kgha <- rbind(new_row_2_df, total_DW_C_kgha)
  standing_DW_C <- rbind(new_row_1_df, standing_DW_C)
  total_alive_C_sim <- rbind(new_row_4_df, total_alive_C_sim)
  total_stem_C_sim <- rbind(new_row_5_df, total_stem_C_sim)
  total_AG_DW_C_sim <- rbind(new_row_6_df, total_AG_DW_C_sim)
  
  
  #-------------------------------------------------------------------------------
  # Merge the data frames Plot L1_10
  plot_L1_10_df_simul <- bind_cols(stand_age, 
                                   standing_DW_C = standing_DW_C$standing_DW_C,
                                   totalC_kgha_iland = totalC_kgha_iland$totalC_kgha_iland,
                                   total_DW_C_kgha = total_DW_C_kgha$total_DW_C_kgha,
                                   total_alive_C_sim = total_alive_C_sim$total_alive_C_sim,
                                   total_stem_C_sim = total_stem_C_sim$total_stem_C_sim,
                                   total_AG_DW_C_sim = total_AG_DW_C_sim$total_AG_DW_C_sim,
                                   lai_sim = LAI$LAI,
                                   ba_broadl = summed_broadl_ba$basal_area_m2,
                                   trees_10_40 = tree_10_40$tree_10_40,
                                   broadl_40 = broadl_40$broadl_40)
  
  
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
  
  # CREATE THE DATA FRAME FOR DAMAGE OF BARKBEETLE
  damage <- data.frame(year = barkbeetle$year, 
                       barkbeetle = barkbeetle$killedVolume, 
                       case = case)
  
  # Check if the "wind" table is present
  if ("wind" %in% tables.in.the.file) {
    head(wind)
    
    # ADD WIND IMPACT IN THE DAMAGE DATA FRAME
    damage<-left_join(damage,wind[,c(1,8)],by=("year"))                           # LEFT_JOIN IS A FUNCTION TO JOIN A VARIABLE IN THIS CASE COLUMN 1 AND 2 AND MANY ROWS BASE ON YEAR VARIABLE NUMBER OF ROWS
    
    # Set NA values in the wind column to 0
    damage$killedVolume[is.na(damage$wind)] <- 0
    
  } else {
    # If wind table is not present, create the wind column with 0 values
    damage$killedVolume <- rep(0, nrow(damage))
  }
  
  # ADD THE LANDSCAPE VOLUME IN THE DAMAGE DATA FRAME
  damage <- left_join(damage, lnd_volume, by = "year")
  
  # GIVE THE NAME TO EVERY VARIABLE
  colnames(damage) <- c("year", "barkbeetle", "case", "wind", "volume")
  
  head(damage)
  
  
  #------------------------------                                               # CLOSE THE litte FOR CYCLE
  
  # Collect abeUnit data FOR CREATE THE VARIABLE ABEUNIT FOR ALL THE RUNS
  #landscape_removed <-(landscape_removed %>% mutate(run=case))
  #landscape_removed_scen<-rbind(landscape_removed_scen, landscape_removed)
  
  # Collect abeStand data for create the variable abeStand for all the run
  abeStand <-(abeStand %>% mutate(run=case))
  abeStand_scen <-rbind(abeStand_scen, abeStand)
  
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
  plot_L1_33_df_simul <-(plot_L1_10_df_simul %>% mutate(run=case))
  plot_variables_all <- rbind(plot_variables_all, plot_L1_33_df_simul)
  
}                                                                               # CLOSE THE WHOLE FOR CYCLE


library(ggplot2)
library(GGally)
library(corrplot)


# Start with plots

#_______________________________________________
library(ggplot2)
library(gridExtra) # To arrange the graphs in a grid

# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/outputs/20240703/Test_unmanaged_wind_all_plots/2020903/output/"
pdf(paste0(dataroot, "20240917_tests_deadwood_managed_unmanaged_spruce.pdf"), height=8, width=12)


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
  ylim(0,1100)+
  theme_bw()


# Create a bar chart of volume by species
ggplot(lnd_scen, aes(x = year, y = volume_m3, fill = factor(species, levels = new_order_gg))) +
  geom_bar(stat = "identity") +  # Change from geom_area to geom_bar
  scale_fill_manual(values = cols[new_order_gg], guide = guide_legend(reverse = TRUE)) +
  ggtitle("Landscape Volume by Species") +
  facet_wrap(~run, ncol = 2) +
  labs(x = "Year", y = "Volume [m3/ha]", fill = "Species") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1100) +
  theme_bw()

#-------------------------------------------------------------------------------
# PLOT TOTAL AVG BASAL AREA AT LANDSCAPE LEVEL BY SPECIES

g3 <- ggplot(lnd_scen, aes(year, basal_area_m2, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Total Basal Area")+
  facet_wrap(~run, ncol=6)+
  labs(x = "Year",y="Basal Area [m2/ha]",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
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
  facet_wrap(~run, ncol=6)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Individual stems")+  
  theme_bw()


#-------------------------------------------------------------------------------
# PLOT NUMBER OF STEMS GEOM_AREA AT LANDSCAPE LEVEL BY SPECIES

g5 <- ggplot(lnd_scen, aes(x=year, y=count_ha, fill=factor(species, levels=new_order_gg)))+ 
  geom_area(size=1.2)+
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("N. individual stems by species") +
  facet_wrap(~run, ncol=6)+
  labs(x = "Year",y="Individual Stems", fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# Create a bar chart of n of trees by species
ggplot(lnd_scen, aes(x = year, y = count_ha, fill = factor(species, levels = new_order_gg))) +
  geom_bar(stat = "identity") +  # Change from geom_area to geom_bar
  scale_fill_manual(values = cols[new_order_gg], guide = guide_legend(reverse = TRUE)) +
  ggtitle("N. individual stems by species") +
  facet_wrap(~run, ncol = 2) +
  labs(x = "Year", y = "Individual Stems", fill = "Species") +
  theme(plot.title = element_text(hjust = 0.5)) +
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
  facet_wrap(~run, ncol=6)+
  labs(x = "Year",y="[kg/ha]",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()


#grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10, ncol=10)

#grid.arrange(b1,b2,b3,b4,b5,ncol=5)



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
  facet_wrap(~run, ncol=6)+
  ggtitle("Snags_C [iLand snags_C fun]")+
  labs(x = "Year",y="snags_C [kg/ha]")+
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
  facet_wrap(~case, ncol=6)+
  ggtitle("Shannon Index on basal area per species")+
  labs(x = "Year",y="Shannon Index")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

#-------------------------------------------------------------------------------
# Make new plots

# TOTAL CARBON IN THE PLOT (LIVING + DEADWOOD + LITTER + SOIL)
totalC_kgha_iland <- ggplot(plot_variables_all, aes(x=year, y=totalC_kgha_iland))+
  geom_line() +
  facet_wrap(~run, ncol=2)+
  ggtitle("Total Plot Carbon [living trees - deadwood - litter - soil]")+
  labs(x = "Year",y="snag_C [kg/ha]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# TOTAL DEADWOOD CARBON (SNAGS + OTHERSNAGS + DOWNED DEADWOOD)
total_DW_C_kgha <- ggplot(plot_variables_all, aes(x=year, y=total_DW_C_kgha))+
  geom_line() + geom_smooth(method = "loess")+
  facet_wrap(~run, ncol=6)+
  ggtitle("Total Deadwood C in iLand standing and lying [kg/ha]")+
  labs(x = "Year",y="Deadwood C [kg/ha]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# TOTAL STANDING DEADWOOD CARBON (SNAGS ONLY)
standing_DW_C <- ggplot(plot_variables_all, aes(x=year, y=standing_DW_C))+
  geom_line() + geom_smooth(method = "loess")+
  facet_wrap(~run, ncol=6)+
  ggtitle("snag_C [iLand snag_C fun] = standing_DW_C")+
  labs(x = "Year",y="snag_C [kg/ha]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# TOTAL LIVING CARBON
total_alive_C_sim <- ggplot(plot_variables_all, aes(x=year, y=total_alive_C_sim))+
  geom_line() + geom_smooth(method = "loess")+
  facet_wrap(~run, ncol=6)+
  ggtitle("Total alive Carbon simulation")+
  labs(x = "Year", y="total_alive [kg/ha]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# TOTAL STEMS CARBON
total_stem_C_sim <- ggplot(plot_variables_all, aes(x=year, y=total_stem_C_sim))+
  geom_line() + geom_smooth(method = "loess")+
  facet_wrap(~run, ncol=6)+
  ggtitle("Total Stem Carbon simulation")+
  labs(x = "Year",y="total_stem_C_sim [kg/ha]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# TOTAL ABOVEGROUND DEADWOOD CARBON
total_AG_DW_C_sim <- ggplot(plot_variables_all, aes(x=year, y=total_AG_DW_C_sim))+
  geom_line() + geom_smooth(method = "loess")+
  facet_wrap(~run, ncol=6)+
  ggtitle("Total Aboveground Deadwood Carbon simulation")+
  labs(x = "Year",y="total_AG_DW_C_sim [kg/ha]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# BOX PLOT 
total_AG_DW_C_sim <- ggplot(plot_variables_all, aes(y=total_AG_DW_C_sim))+
  geom_boxplot() +
  facet_wrap(~run, ncol=2)+
  ggtitle("Total Aboveground Deadwood Carbon simulation")+
  labs(y="total_AG_DW_C_sim [kg/ha]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()


med <- plot_variables_all %>% group_by(run) %>% summarise(median = median(total_AG_DW_C_sim, na.rm = TRUE))
avg <- plot_variables_all %>% group_by(run) %>% summarise(mean = mean(total_AG_DW_C_sim, na.rm = TRUE))

#-----------------------------------------
# PLOT ALL THE VARIABLES IN THE SAME GRAPH

# Combine all relevant variables into a long-format data frame
plot_variables_long <- plot_variables_all %>%
  pivot_longer(cols = c(standing_DW_C, total_alive_C_sim, total_stem_C_sim, total_AG_DW_C_sim),
               names_to = "variable",
               values_to = "value")

# Plot all variables in a single plot
all_variables_plot <- ggplot(plot_variables_long, aes(x = year, y = value, color = variable, linetype = variable)) +
  geom_line() +
  geom_line(size = 0.5) +
  facet_wrap(~run, ncol=6) +
  ggtitle("Comparison of Different Carbon Metrics Over Time") +
  labs(x = "Year", y = "Value [kg/ha]", color = "Variable", linetype = "Variable") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1")  # Optional: Change color palette

# Display the plot
print(all_variables_plot)

#---------------
# ADDING THE AGE

# Combine all relevant variables into a long-format data frame
plot_variables_long <- plot_variables_all %>%
  pivot_longer(cols = c(standing_DW_C, total_alive_C_sim, total_stem_C_sim, total_AG_DW_C_sim),
               names_to = "variable",
               values_to = "value")

# Calculate scaling factor to match the secondary y-axis
age_scaling_factor <- max(plot_variables_long$value) / max(abeStand_scen$age)

# Plot all variables and the scaled age in a single plot
all_variables_plot <- ggplot(plot_variables_long, aes(x = year, y = value, color = variable)) +
  geom_line(size = 0.5) +
  #geom_line(data = abeStand_scen, aes(x = year, y = age * age_scaling_factor), color = "black", size = 1, linetype = "dashed") +
  facet_wrap(~run, ncol = 6) +
  ggtitle("Comparison of Different Carbon Metrics Over Time") +
  labs(x = "Year", y = "Value [kg/ha]", color = "Variable", linetype = "Variable") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") #+
  #scale_y_continuous(sec.axis = sec_axis(~ . / age_scaling_factor, name = "Age [years]"))

# Display the plot
print(all_variables_plot)

#-----------------------------------------
# LEVEL PRO!!!!
# Combine all relevant variables into a long-format data frame
plot_variables_long <- plot_variables_all %>%
  pivot_longer(cols = c(standing_DW_C, total_alive_C_sim, total_stem_C_sim, total_AG_DW_C_sim),
               names_to = "variable",
               values_to = "value")

# Calculate scaling factor to match the secondary y-axis
age_scaling_factor <- max(plot_variables_long$value) / max(abeStand_scen$age)

# Plot for L1 runs - the read control line is for avg living carbon (upper) and standing deadwood (lower)
all_variables_plot_L1 <- ggplot(subset(plot_variables_long, grepl("L1", run)), aes(x = year, y = value, color = variable)) +
  geom_line(size = 0.7) +
  geom_line(data = subset(abeStand_scen, grepl("L1", run)), aes(x = year, y = age * age_scaling_factor), color = "black", size = 0.7, linetype = "dashed") +
  geom_hline(yintercept = mean(subset(plot_variables_long, grepl("L1", run) & variable == "total_AG_DW_C_sim")$value), color = "red", linetype = "dashed") +
  geom_hline(yintercept = mean(subset(plot_variables_long, grepl("L1", run) & variable == "standing_DW_C")$value), color = "red", linetype = "dashed") +
  ggtitle("Comparison of Different Carbon Metrics Over Time for L1 Runs (L1_10_hist_clim_calib_deadwood pools") +
  labs(x = "Year", y = "Value [kg/ha]", color = "Variable") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(sec.axis = sec_axis(~ . / age_scaling_factor, name = "Age [years]"))

# Plot for L4 runs
all_variables_plot_L4 <- ggplot(subset(plot_variables_long, grepl("L2", run)), aes(x = year, y = value, color = variable)) +
  geom_line(size = 1) +
  geom_line(data = subset(abeStand_scen, grepl("L2", run)), aes(x = year, y = age * age_scaling_factor), color = "black", size = 1, linetype = "dashed") +
  geom_hline(yintercept = mean(subset(plot_variables_long, grepl("L2", run) & variable == "total_AG_DW_C_sim")$value), color = "red", linetype = "dashed")+
  geom_hline(yintercept = mean(subset(plot_variables_long, grepl("L2", run) & variable == "standing_DW_C")$value), color = "red", linetype = "dashed") +
  ggtitle("Comparison of Different Carbon Metrics Over Time for L2_34 Runs") +
  labs(x = "Year", y = "Value [kg/ha]", color = "Variable") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(sec.axis = sec_axis(~ . / age_scaling_factor, name = "Age [years]"))

# Display the plots
print(all_variables_plot_L1)
print(all_variables_plot_L4)

#------------------------------------------------------------------------------
# TOGETHER PLOT
# Combine all relevant variables into a long-format data frame
plot_variables_long <- plot_variables_all %>%
  pivot_longer(cols = c(standing_DW_C, total_alive_C_sim, total_stem_C_sim, total_AG_DW_C_sim),
               names_to = "variable",
               values_to = "value")

# Calculate scaling factor to match the secondary y-axis
age_scaling_factor <- max(plot_variables_long$value) / max(abeStand_scen$age)

# Combine L1 and L2 runs into one plot with facets
combined_plot <- ggplot(plot_variables_long, aes(x = year, y = value, color = variable)) +
  geom_line(size = 0.7) +
  geom_line(data = abeStand_scen, aes(x = year, y = age * age_scaling_factor), color = "black", size = 0.7, linetype = "dashed") +
  #geom_hline(yintercept = mean(subset(plot_variables_long, variable == "total_AG_DW_C_sim")$value), color = "red", linetype = "dashed") +
  #geom_hline(yintercept = mean(subset(plot_variables_long, variable == "standing_DW_C")$value), color = "red", linetype = "dashed") +
  ggtitle("Comparison of Different Carbon Metrics Over Time for L1_10 and L2_34 Runs") +
  labs(x = "Year", y = "Value [kg/ha]", color = "Variable") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(sec.axis = sec_axis(~ . / age_scaling_factor, name = "Age [years]")) +
  facet_wrap(~ run, scales = "free_y")  # Facet by run (L1, L2, etc.)

# Display the combined plot
print(combined_plot)



#------------------------------------------------------------------------------
# Age mean

g7 <- ggplot(dys_scen, aes(x=year, y=age_mean))+
  geom_line() +
  ggtitle("Avarage Tree Age")+
  facet_wrap(~run, ncol=6)+
  labs(x = "Year",y="Age mean [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

ggplot(plot_variables_all, aes(x=year, y=age))+
  geom_line() +
  ggtitle("Mean Trees Age")+
  facet_wrap(~run, ncol=6)+
  labs(x = "Year",y="Age mean [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

ggplot(abeStand_scen, aes(x=year, y=age))+
  geom_line() +
  ggtitle("Stand Age")+
  facet_wrap(~run, ncol=6)+
  labs(x = "Year",y="Age [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# BA BROADLEAVE SP

ba_broadl <- ggplot(plot_variables_all, aes(x=year, y=ba_broadl))+
  geom_line() +
  ggtitle("BA BROADLEAVE SP")+
  facet_wrap(~run, ncol=6)+
  labs(x = "Year",y="BA BROADLEAVE SP [m2]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()


# NUMBER OF TREES WITH DBH BETWEEN 10cm AND 40cm included

trees_10_40 <- ggplot(plot_variables_all, aes(x=year, y=trees_10_40))+
  geom_line() +
  ggtitle("NUMBER OF TREES WITH DBH BETWEEN 10cm AND 40cm included")+
  facet_wrap(~run, ncol=6)+
  labs(x = "Year",y="No Trees with dbh from 10cm to 40cm [No]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# NUMBER OF BROADLEAVE TREES WITH DBH > 40cm

broadl_40 <- ggplot(plot_variables_all, aes(x=year, y=broadl_40))+
  geom_line() +
  ggtitle("NUMBER OF BROADLEAVE TREES WITH DBH > 40cm")+
  facet_wrap(~run, ncol=6)+
  labs(x = "Year",y="No broadleave trees with dbh > 40cm [No]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# Cumulative Natural Mortality

P1 <- ggplot(landscape_removed_scen_natmor, aes(year, cumm_mortality_total_carbon, color=species )) +
  geom_line(size = 0.6) +
  facet_wrap(~run, ncol=6)+
  ggtitle("Cumulative Natural Mortality in Carbon") +
  ylab("Total Carbon in Nat. Mortality [kg/ha]") +
  xlab("Year") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, lineheight = 3, face = "bold", color = "black", size = 20),
    axis.title.y = element_text(size = rel(1.8), angle = 90),
    axis.title.x = element_text(size = rel(1.8), angle = 0)
  )

########################################################## CLOSE EVERY PLOTs

dev.off()
