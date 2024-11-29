# Load required libraries
library(tidyr)
library(dplyr)
library(RSQLite)
library(vegan)
library(fields)
library(ggplot2)
library(GGally)
library(corrplot)
library(gridExtra) # To arrange the graphs in a grid
library(writexl)

# This was the directory to plot one by one the plots with seed background based on plot and based on sites
#setwd("C:/iLand/2023/20230901_Bottoms_Up/outputs/20240523/L6_21_test/")

# Path to the directory containing your SQLite databases
#dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/outputs/20240523/L6_21_test/"

setwd("C:/iLand/2023/20230901_Bottoms_Up/outputs/20240523_official_plot_models_setting/DB_plot/")


dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/outputs/20240523_official_plot_models_setting/DB_plot/"
# Get a list of all SQLite databases in the directory
# database_files <- list.files(path = dataroot, pattern = ".sqlite", full.names = TRUE)

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
  tables.in.the.file<-dbListTables(db)           # explore the tables in the file
  print(tables.in.the.file)
  
  
  #-----------------------------------------------------------------------------
  # LOAD THE DATABASE # Read tables from the database
  abeStand <- dbReadTable(db,"abeStand")
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
    dplyr::select(year, height_mean, dbh_mean, age_mean)
  
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
    summarise(tree_10_40 = n(), .groups = 'drop') %>%
    complete(year = 0:max(year), fill = list(tree_10_40 = 0))
  
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
    summarize(basal_area_m2 = sum(basal_area_m2, na.rm = TRUE)) %>%
    complete(year = seq(0, max(year), by = 1), fill = list(basal_area_m2 = 0))
  
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
  # Age - comes from the avarage tree age
  
  # Summarize the mean age for each year and round the ages
  tree_age <- tree %>%
    group_by(year) %>%
    summarize(age = mean(age)) %>%
    mutate(age = round(age))
  
  # Create a complete sequence of years and fill missing ages with 0
  tree_age <- tree_age %>%
    complete(year = seq(min(year), max(year), by = 1), fill = list(age = 0))
  
  # stand age - rotation
  stand_age <- abeStand %>%
    select(year,age)
    
  # Create a new row with manually specified values to avoid the missing starting year 0
  new_row_1 <- c(0, 79)
  
  # Add the new row at the beginning of the data frame
  stand_age <- rbind(new_row_1, stand_age)
  
  #-------------------------------------------------------------------------------
  # DW volume - USE snags_c divided by 4 and converted into volume 
  # Alternative use the Carbon instead that volume
  
  #-----------------------------------------------------------------------------
  # CREATE THE DATA FRAME FOR ADD VARIABLES ABOUT CARBON IN THE FINAL DATA FRAME
  
  # Total Carbon
  total_C_sim <- data.frame(carbon %>% 
                                    group_by(year) %>% 
                                    summarise(total_C_sim=sum(stem_c, branch_c, foliage_c, coarseRoot_c, fineRoot_c, regeneration_c, snags_c, snagsOther_c, downedWood_c, litter_c, soil_c)))
  
  # Total living carbon
  total_alive_C_sim <- data.frame(carbon %>% 
                                 group_by(year) %>% 
                                 summarise(total_alive_C_sim=sum(stem_c, branch_c, foliage_c, coarseRoot_c, fineRoot_c, regeneration_c)))
  
  # Total stems carbon
  total_stem_C_sim <- data.frame(carbon %>% 
                                    group_by(year) %>% 
                                    summarise(total_stem_C_sim=sum(stem_c)))
  
  # Total DW Carbon
  total_DW_C_sim <- data.frame(carbon %>% 
                                  group_by(year) %>% 
                                  summarise(total_DW_C_sim =sum(snags_c, snagsOther_c, downedWood_c)))
  # Total Aboveground DW Carbon
  total_AG_DW_C_sim <- data.frame(carbon %>% 
                                  group_by(year) %>% 
                                  summarise(total_AG_DW_C_sim=sum(snags_c, snagsOther_c_ag, downedWood_c_ag)))
  
  # Snag Carbon
  standing_DW_C_sim <- data.frame(carbon %>% 
                                group_by(year) %>% 
                                summarise(standing_DW_C_sim = sum(snags_c))) 
  
  # Create a new row with manually specified values
  new_row_1 <- c(0, 1690)  # Add your values accordingly
  new_row_2 <- c(0, 3966.662)  # Add your values accordingly : original 749+233+225
  new_row_3 <- c(0, 283716.5)  # Add your values accordingly
  new_row_4 <- c(0,3465.85)    # AG deadwood C
  new_row_5 <- c(0,150000)     # C in alive trees compartments
  new_row_6 <- c(0, total_stem_C_sim %>% filter(year == 1) %>% pull(total_stem_C_sim))  # Living C in stems
  
  # Add the new row at the beginning of the data frame in all Carbon variables
  total_stem_C_sim <- rbind(new_row_6, total_stem_C_sim)
  total_alive_C_sim <- rbind(new_row_5, total_alive_C_sim)
  total_AG_DW_C_sim <- rbind(new_row_4, total_AG_DW_C_sim)
  total_C_sim <- rbind(new_row_3, total_C_sim)
  total_DW_C_sim <- rbind(new_row_2, total_DW_C_sim)
  standing_DW_C_sim <- rbind(new_row_1, standing_DW_C_sim)
  
  #-------------------------------------------------------------------------------
  # Merge the data frames Plot L1_10 Substitute with "tree_age" is want the mean tree age
  plot_L1_10_df_simul <- bind_cols(stand_age,
                                   snag_c = standing_DW_C_sim$standing_DW_C_sim,
                                   total_AG_DW_C_sim = total_AG_DW_C_sim$total_AG_DW_C_sim,
                                   total_DW_C_sim = total_DW_C_sim$total_DW_C_sim,
                                   total_stem_C_sim = total_stem_C_sim$total_stem_C_sim,
                                   total_alive_C_sim = total_alive_C_sim$total_alive_C_sim,
                                   total_C_sim = total_C_sim$total_C_sim,
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
  #------------------------------                                               # CLOSE THE litte FOR CYCLE
  
  # Collect abeUnit data FOR CREATE THE VARIABLE ABEUNIT FOR ALL THE RUNS
  abeStand <-(abeStand %>% mutate(run=case))
  abeStand_scen<-rbind(abeStand_scen, abeStand)
  
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
  
}

#-------------------------------------------------------------------------------
# Start with plots
#-------------------------------------------------------------------------------



# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/outputs/20240523/L6_21_test/"
pdf(paste0(dataroot, "Plot_L6_21.pdf"), height=8, width=12)


#-------------------------------------------------------------------------------
# This tells the colors:

species.we.have<-unique(lnd_scen$species)                                            # IT IS SAYING WHICH SPECIES WE HAVE IN THE DATABASE IN THIS CASE LANDSCAPE


# LIST OF ALL POSSIBLE SPECIES

cols.all=c( "rops"="#e0e0e0", "acpl"="#A9A9A9",   "alin"="#696969", "alvi"="#2e2e2e",
            "bepe"="#fadfad", "acca"="#FF4600",
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

new_order_gg.all=c("alvi","alin","acca", "acpl", "rops","bepe" ,"coav", "casa", "ulgl", "tipl",  "soau", "soar", "saca",  "pini", "pice",
                   "poni", "algl", "tico", "potr",  "frex","cabe", "acps",  "lade", "abal",  "qupu", "qupe","quro","pisy", "fasy", "piab")


# This will show at the end only the species we really have on the landscape. 

cols<-cols.all[names(cols.all) %in% species.we.have]
new_order_gg<- new_order_gg.all[new_order_gg.all %in% species.we.have]

# STARTING PLOts

# Make a plot with ggplot, volume, colored by species for the transitional period for Clear cut management system
#-------------------------------------------------------------------------------
# PLOT LANDSCAPE VOLUME PLOT FOR CASES (GEOM AREA)

ggplot(lnd_scen, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Landscape Volume by species")+
  facet_wrap(~run, ncol=1)+
  labs(x = "Year",y="Volume [m3/ha]",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,1000)+
  theme_bw()

# CARBON 

ggplot(carbon_scen, aes(x=year, y=snags_c))+
  geom_line() +
  facet_wrap(~run, ncol=1)+
  ggtitle("Snags_C [iLand snags_C fun]")+
  labs(x = "Year",y="snags_C [kg/ha]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# Age mean

ggplot(abeStand_scen, aes(x=year, y=age))+
  geom_line() +
  ggtitle("Stand Age")+
  facet_wrap(~run, ncol=1)+
  labs(x = "Year",y="Age [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()



# BA BROADLEAVE SP

ggplot(plot_variables_all, aes(x=year, y=ba_broadl))+
  geom_line() +
  ggtitle("BA BROADLEAVE SP")+
  facet_wrap(~run, ncol=1)+
  labs(x = "Year",y="BA BROADLEAVE SP [m2]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()


# NUMBER OF TREES WITH DBH BETWEEN 10cm AND 40cm included

ggplot(plot_variables_all, aes(x=year, y=trees_10_40))+
  geom_line() +
  ggtitle("NUMBER OF TREES WITH DBH BETWEEN 10cm AND 40cm included")+
  facet_wrap(~run, ncol=1)+
  labs(x = "Year",y="No Trees with dbh from 10cm to 40cm [No]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# NUMBER OF BROADLEAVE TREES WITH DBH > 40cm

ggplot(plot_variables_all, aes(x=year, y=broadl_40))+
  geom_line() +
  ggtitle("NUMBER OF BROADLEAVE TREES WITH DBH > 40cm")+
  facet_wrap(~run, ncol=1)+
  labs(x = "Year",y="No broadleave trees with dbh > 40cm [No]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

# close the plots
dev.off()

#-------------------------------------------------------------------------------
# SNAG CARBON FUNCTION SIMULATION vs FIELDWORK SAMPLES
#-------------------------------------------------------------------------------
# Merge the carbon_scen with the abeStand_scen
snag_age <- data.frame(carbon_scen=carbon_scen,
                       volume=abeStand_scen$volume,
                       age=abeStand_scen$age)


#-------------------------------------------------------------------------------
# Create the table of the snag values

snag_c <- carbon_scen %>%
  # Filter the dataframe based on the specific value in the 'run' column
  filter(run == "DB_CZ_JH1_L6XL6_21_plot.sqlite") %>%
  group_by(year)%>%
  select(year,snags_c)

# Select the values for the needed years
selected_years <- snag_c %>% filter(year %in% c(160, 320, 480))

# Ensure the selected years are sorted properly
selected_years <- selected_years %>% arrange(year)

# Create a new data frame with the required structure
snags_fun <- data.frame(
  plotID = "L6_21",  # Replace with actual plotID value or a sequence of plotIDs if available
  first_rot = selected_years$snags_c[selected_years$year == 160],
  second_rot = selected_years$snags_c[selected_years$year == 320],
  third_rot = selected_years$snags_c[selected_years$year == 480]
)

# Add a new column for the average of the three rotations
snags_fun <- snags_fun %>%
  mutate(average = (first_rot + second_rot + third_rot) / 3)

# Print the new data frame
print(snags_fun)

# Write the table
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/deadwood_fun/"

# sp prop per site based on n. of trees
write_xlsx(snags_fun, file.path(dataroot, "snags_fun_table_plot_L6_21.xlsx"))

#-------------------------------------------------------------------------------
# Create the table of the snag values

snag_c <- carbon_scen %>%
  # Filter the dataframe based on the specific value in the 'run' column
  filter(run == "DB_CZ_JH1_L6XL6_14_plot.sqlite") %>%
  group_by(year)%>%
  select(year,snags_c)

# Select the values for the needed years
selected_years <- snag_c %>% filter(year %in% c(120, 240, 360))

# Ensure the selected years are sorted properly
selected_years <- selected_years %>% arrange(year)

# Create a new data frame with the required structure
snags_fun <- data.frame(
  plotID = "L6_14",  # Replace with actual plotID value or a sequence of plotIDs if available
  first_rot = selected_years$snags_c[selected_years$year == 120],
  second_rot = selected_years$snags_c[selected_years$year == 240],
  third_rot = selected_years$snags_c[selected_years$year == 360]
)

# Add a new column for the average of the three rotations
snags_fun <- snags_fun %>%
  mutate(average = (first_rot + second_rot + third_rot) / 3)

# Print the new data frame
print(snags_fun)

# Write the table
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/deadwood_fun/"

# sp prop per site based on n. of trees
write_xlsx(snags_fun, file.path(dataroot, "snags_fun_table_plot_L6_14.xlsx"))


#-------------------------------------------------------------------------------
library(readxl)
library(dplyr)

# Define the directory containing the Excel files
directory <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/snags_fun/"

# List all Excel files in the directory
excel_files <- list.files(path = directory, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty list to store the data frames
list_of_dfs <- list()

# Loop through each file and read it into a data frame, then store it in the list
for (file in excel_files) {
  df <- read_xlsx(file)
  list_of_dfs <- append(list_of_dfs, list(df))
}

# Combine all data frames in the list into a single data frame
snag_mng_df <- bind_rows(list_of_dfs)

# Print the combined data frame to verify the result
print(snag_mng_df)

# snag vales for the bdv snag function creation
write_xlsx(snag_mng_df, "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/snags_fun/Snags_function_all.xlsx")
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# TOTAL DEADWOOD CARBON FUNCTION SIMULATION vs FIELDWORK SAMPLES
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Merge the carbon_scen with the abeStand_scen
deadwood_age <- data.frame(carbon_scen=carbon_scen,
                       volume=abeStand_scen$volume,
                       age=abeStand_scen$age)
#-------------------------------------------------------------------------------


# Create the table of the C pools values for Broadleaf dominated stands
#-------------------------------------------------------------------------------
data_C_pools_BL <- plot_variables_all %>%
  # Filter the dataframe based on the specific value in the 'run' column
  filter(run == "DB_CZ_JH1_L1XL1_03_plot.sqlite") %>%
  group_by(year)%>%
  select(year, snag_c , total_AG_DW_C_sim , total_DW_C_sim ,total_alive_C_sim, total_C_sim, total_stem_C_sim)

# Select the values for the needed years
selected_years <- data_C_pools_BL %>% filter(year %in% c(160, 320, 480))

# Ensure the selected years are sorted properly
selected_years <- selected_years %>% arrange(year)

# Create a new data frame using the equivalent rotation time of the starting stand age at simulation year 0.
deadwood_fun <- data.frame(
  plotID = "L1_03",  # Replace with actual plotID value or a sequence of plotIDs if available
  first_rot = selected_years$total_DW_C_sim[selected_years$year == 160],
  second_rot = selected_years$total_DW_C_sim[selected_years$year == 320],
  third_rot = selected_years$total_DW_C_sim[selected_years$year == 480]
)

# Add a new column for the average of the three rotations
deadwood_fun <- deadwood_fun %>%
  mutate(average = (first_rot + second_rot + third_rot) / 3)

# Print the new data frame
print(deadwood_fun)

# Write the table
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/deadwood_fun/"

# sp prop per site based on n. of trees
write_xlsx(deadwood_fun, file.path(dataroot, "deadwood_fun_table_plot_L1_03.xlsx"))

#-------------------------------------------------------------------------------
# Aboveground (AG) deadwood (DW) Carbon
AG_DW_C <- data.frame(
  plotID = "L1_03",  # Replace with actual plotID value or a sequence of plotIDs if available
  first_rot = selected_years$total_AG_DW_C_sim[selected_years$year == 160],
  second_rot = selected_years$total_AG_DW_C_sim[selected_years$year == 320],
  third_rot = selected_years$total_AG_DW_C_sim[selected_years$year == 480]
)

# Add a new column for the average of the three rotations
AG_DW_C <- AG_DW_C %>%
  mutate(average = (first_rot + second_rot + third_rot) / 3)

# Print the new data frame
print(AG_DW_C)

# Write the table
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/AG_DW_C_sim_fun/"

# sp prop per site based on n. of trees
write_xlsx(AG_DW_C, file.path(dataroot, "AG_DW_C_sim_fun_table_plot_L1_03.xlsx"))

#-------------------------------------------------------------------------------
# TOTAL CARBON
total_carbon_fun <- data.frame(
  plotID = "L1_03",  # Replace with actual plotID value or a sequence of plotIDs if available
  first_rot = selected_years$total_C_sim[selected_years$year == 160],
  second_rot = selected_years$total_C_sim[selected_years$year == 320],
  third_rot = selected_years$total_C_sim[selected_years$year == 480]
)

# Add a new column for the average of the three rotations
total_carbon_fun <- total_carbon_fun %>%
  mutate(average = (first_rot + second_rot + third_rot) / 3)

# Print the new data frame
print(total_carbon_fun)

# Write the table
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/total_carbon_fun/"

# sp prop per site based on n. of trees
write_xlsx(total_carbon_fun, file.path(dataroot, "total_carbon_fun_table_plot_L1_03.xlsx"))
#-------------------------------------------------------------------------------

# ALIVE TREE COMPARTMENT CARBON (remember to add the year 0 of the simulation for the same thing you need to use or year 1 here or year 0 from landscape converting in the volume)
total_alive_C_sim <- data.frame(
  plotID = "L1_03",  # Replace with actual plotID value or a sequence of plotIDs if available
  first_rot = selected_years$total_alive_C_sim[selected_years$year == 160],
  second_rot = selected_years$total_alive_C_sim[selected_years$year == 320],
  third_rot = selected_years$total_alive_C_sim[selected_years$year == 480]
)

# Add a new column for the average of the three rotations
total_alive_C_sim <- total_alive_C_sim %>%
  mutate(average = (first_rot + second_rot + third_rot) / 3)

# Print the new data frame
print(total_alive_C_sim)

# Write the table
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/total_alive_C_sim_fun/"

# sp prop per site based on n. of trees
write_xlsx(total_alive_C_sim, file.path(dataroot, "total_alive_C_sim_fun_table_plot_L1_03.xlsx"))

#-------------------------------------------------------------------------------
# Total C in stems alive  (remember to add the year 0 of the simulation for the same thing you need to use or year 1 here or year 0 from landscape converting in the volume)

total_stem_C_sim <- data.frame(
  plotID = "L1_03",  # Replace with actual plotID value or a sequence of plotIDs if available
  first_rot = selected_years$total_stem_C_sim[selected_years$year == 160],
  second_rot = selected_years$total_stem_C_sim[selected_years$year == 320],
  third_rot = selected_years$total_stem_C_sim[selected_years$year == 480]
)

# Add a new column for the average of the three rotations
total_stem_C_sim <- total_stem_C_sim %>%
  mutate(average = (first_rot + second_rot + third_rot) / 3)

# Print the new data frame
print(total_stem_C_sim)

# Write the table
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/total_stem_C_sim_fun/"

# sp prop per site based on n. of trees
write_xlsx(total_stem_C_sim, file.path(dataroot, "total_stem_C_sim_fun_table_plot_L1_03.xlsx"))
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------




# Create the table of the deadwood values X coniferous stands dominated
#-------------------------------------------------------------------------------
data_C_pools_CF <- plot_variables_all %>%
  # Filter the dataframe based on the specific value in the 'run' column
  filter(run == "DB_CZ_JH1_L1XL1_07_plot.sqlite") %>%
  group_by(year)%>%
  select(year,snag_c , total_AG_DW_C_sim , total_DW_C_sim ,total_alive_C_sim, total_C_sim, total_stem_C_sim)

# Select the values for the needed years
selected_years <- data_C_pools_CF %>% filter(year %in% c(120, 240, 360))

# Ensure the selected years are sorted properly
selected_years <- selected_years %>% arrange(year)

# Create a new data frame with the required structure
deadwood_fun <- data.frame(
  plotID = "L1_07",  # Replace with actual plotID value or a sequence of plotIDs if available
  first_rot = selected_years$total_DW_C_sim[selected_years$year == 120],
  second_rot = selected_years$total_DW_C_sim[selected_years$year == 240],
  third_rot = selected_years$total_DW_C_sim[selected_years$year == 360]
)

# Add a new column for the average of the three rotations
deadwood_fun <- deadwood_fun %>%
  mutate(average = (first_rot + second_rot + third_rot) / 3)

# Print the new data frame
print(deadwood_fun)

# Write the table
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/deadwood_fun/"

# sp prop per site based on n. of trees
write_xlsx(deadwood_fun, file.path(dataroot, "deadwood_fun_table_plot_L1_03.xlsx"))
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Aboveground (AG) deadwood (DW) Carbon

AG_DW_C <- data.frame(
  plotID = "L1_07",  # Replace with actual plotID value or a sequence of plotIDs if available
  first_rot = selected_years$total_AG_DW_C_sim[selected_years$year == 120],
  second_rot = selected_years$total_AG_DW_C_sim[selected_years$year == 240],
  third_rot = selected_years$total_AG_DW_C_sim[selected_years$year == 360]
)

# Add a new column for the average of the three rotations
AG_DW_C <- AG_DW_C %>%
  mutate(average = (first_rot + second_rot + third_rot) / 3)

# Print the new data frame
print(AG_DW_C)

# Write the table
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/AG_DW_C_sim_fun/"

# sp prop per site based on n. of trees
write_xlsx(AG_DW_C, file.path(dataroot, "AG_DW_C_sim_fun_table_plot_L1_03.xlsx"))

#-------------------------------------------------------------------------------
# TOTAL CARBON FUNCTION 
total_carbon_fun <- data.frame(
  plotID = "L1_03",  # Replace with actual plotID value or a sequence of plotIDs if available
  first_rot = selected_years$total_C_sim[selected_years$year == 120],
  second_rot = selected_years$total_C_sim[selected_years$year == 240],
  third_rot = selected_years$total_C_sim[selected_years$year == 360]
)

# Add a new column for the average of the three rotations
total_carbon_fun <- total_carbon_fun %>%
  mutate(average = (first_rot + second_rot + third_rot) / 3)

# Print the new data frame
print(total_carbon_fun)

# Write the table
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/total_carbon_fun/"

# sp prop per site based on n. of trees
write_xlsx(total_carbon_fun, file.path(dataroot, "total_carbon_fun_table_plot_L1_03.xlsx"))
#-------------------------------------------------------------------------------

# ALIVE TREE COMPARTMENT CARBON (remember to add the year 0 of the simulation for the same thing you need to use or year 1 here or year 0 from landscape converting in the volume)
total_alive_C_sim <- data.frame(
  plotID = "L1_07",  # Replace with actual plotID value or a sequence of plotIDs if available
  first_rot = selected_years$total_alive_C_sim[selected_years$year == 120],
  second_rot = selected_years$total_alive_C_sim[selected_years$year == 240],
  third_rot = selected_years$total_alive_C_sim[selected_years$year == 360]
)

# Add a new column for the average of the three rotations
total_alive_C_sim <- total_alive_C_sim %>%
  mutate(average = (first_rot + second_rot + third_rot) / 3)

# Print the new data frame
print(total_alive_C_sim)

# Write the table
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/total_alive_C_sim/"

# sp prop per site based on n. of trees
write_xlsx(total_alive_C_sim, file.path(dataroot, "total_alive_C_sim_fun_table_plot_L1_03.xlsx"))

#-------------------------------------------------------------------------------
# Total C in stems alive  (remember to add the year 0 of the simulation for the same thing you need to use or year 1 here or year 0 from landscape converting in the volume)

total_stem_C_sim <- data.frame(
  plotID = "L1_03",  # Replace with actual plotID value or a sequence of plotIDs if available
  first_rot = selected_years$total_stem_C_sim[selected_years$year == 120],
  second_rot = selected_years$total_stem_C_sim[selected_years$year == 240],
  third_rot = selected_years$total_stem_C_sim[selected_years$year == 360]
)

# Add a new column for the average of the three rotations
total_stem_C_sim <- total_stem_C_sim %>%
  mutate(average = (first_rot + second_rot + third_rot) / 3)

# Print the new data frame
print(total_stem_C_sim)

# Write the table
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/total_stem_C_sim_fun/"

# sp prop per site based on n. of trees
write_xlsx(total_stem_C_sim, file.path(dataroot, "total_stem_C_sim_fun_table_plot_L1_03.xlsx"))
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# LOOP TRHOUGH ALL THE EXCEL FILES FOR DEADWOOD CARBON POOL TO CREATE UNIQUE TABLES
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# SNAGS
#-------------------------------------------------------------------------------

library(readxl)
library(dplyr)

# Define the directory containing the Excel files
directory <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/snags_fun/"

# List all Excel files in the directory
excel_files <- list.files(path = directory, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty list to store the data frames
list_of_dfs <- list()

# Loop through each file and read it into a data frame, then store it in the list
for (file in excel_files) {
  df <- read_xlsx(file)
  list_of_dfs <- append(list_of_dfs, list(df))
}

# Combine all data frames in the list into a single data frame
snags_fun <- bind_rows(list_of_dfs)

# Print the combined data frame to verify the result
print(snags_fun)

# Total carbon vales for the bdv total carbon function creation
write_xlsx(snags_fun, "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/snags_fun/snags_function_all.xlsx")

#-------------------------------------------------------------------------------
# TOTAL DEADWOOD
#-------------------------------------------------------------------------------

library(readxl)
library(dplyr)

# Define the directory containing the Excel files
directory <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/deadwood_fun/"

# List all Excel files in the directory
excel_files <- list.files(path = directory, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty list to store the data frames
list_of_dfs <- list()

# Loop through each file and read it into a data frame, then store it in the list
for (file in excel_files) {
  df <- read_xlsx(file)
  list_of_dfs <- append(list_of_dfs, list(df))
}

# Combine all data frames in the list into a single data frame
deadwood_fun <- bind_rows(list_of_dfs)

# Print the combined data frame to verify the result
print(deadwood_fun)

# Total deadwood carbon vales for the bdv total deadwood carbon function creation
write_xlsx(deadwood_fun, "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/deadwood_fun/Deadwood_function_all.xlsx")

#-------------------------------------------------------------------------------
# TOTAL CARBON
#-------------------------------------------------------------------------------
library(readxl)
library(dplyr)

# Define the directory containing the Excel files
directory <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/total_carbon_fun/"

# List all Excel files in the directory
excel_files <- list.files(path = directory, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty list to store the data frames
list_of_dfs <- list()

# Loop through each file and read it into a data frame, then store it in the list
for (file in excel_files) {
  df <- read_xlsx(file)
  list_of_dfs <- append(list_of_dfs, list(df))
}

# Combine all data frames in the list into a single data frame
total_carbon_mng_df <- bind_rows(list_of_dfs)

# Print the combined data frame to verify the result
print(total_carbon_mng_df)

# Total carbon vales for the bdv total carbon function creation
write_xlsx(total_carbon_mng_df, "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/total_carbon_fun/total_carbon_function_all.xlsx")

#-------------------------------------------------------------------------------
# ABOVEGROUND DW C
#-------------------------------------------------------------------------------
library(readxl)
library(dplyr)

# Define the directory containing the Excel files
directory <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/AG_DW_C_sim_fun/"

# List all Excel files in the directory
excel_files <- list.files(path = directory, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty list to store the data frames
list_of_dfs <- list()

# Loop through each file and read it into a data frame, then store it in the list
for (file in excel_files) {
  df <- read_xlsx(file)
  list_of_dfs <- append(list_of_dfs, list(df))
}

# Combine all data frames in the list into a single data frame
AG_DW_C_sim_fun <- bind_rows(list_of_dfs)

# Print the combined data frame to verify the result
print(AG_DW_C_sim_fun)

# Total carbon vales for the bdv total carbon function creation
write_xlsx(AG_DW_C_sim_fun, "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/AG_DW_C_sim_fun/AG_DW_C_sim_function_all.xlsx")


#-------------------------------------------------------------------------------
# TOTAL ALIVE CARBON
#-------------------------------------------------------------------------------
library(readxl)
library(dplyr)

# Define the directory containing the Excel files
directory <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/total_alive_C_sim_fun/"

# List all Excel files in the directory
excel_files <- list.files(path = directory, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty list to store the data frames
list_of_dfs <- list()

# Loop through each file and read it into a data frame, then store it in the list
for (file in excel_files) {
  df <- read_xlsx(file)
  list_of_dfs <- append(list_of_dfs, list(df))
}

# Combine all data frames in the list into a single data frame
total_alive_C_sim_fun <- bind_rows(list_of_dfs)

# Print the combined data frame to verify the result
print(total_alive_C_sim_fun)

# Total carbon vales for the bdv total carbon function creation
write_xlsx(total_alive_C_sim_fun, "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/total_alive_C_sim_fun/total_alive_C_sim_function_all.xlsx")

#-------------------------------------------------------------------------------
# STEM CARBON
#-------------------------------------------------------------------------------
library(readxl)
library(dplyr)

# Define the directory containing the Excel files
directory <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/total_stem_C_sim_fun/"

# List all Excel files in the directory
excel_files <- list.files(path = directory, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty list to store the data frames
list_of_dfs <- list()

# Loop through each file and read it into a data frame, then store it in the list
for (file in excel_files) {
  df <- read_xlsx(file)
  list_of_dfs <- append(list_of_dfs, list(df))
}

# Combine all data frames in the list into a single data frame
total_stem_C_sim_fun <- bind_rows(list_of_dfs)

# Print the combined data frame to verify the result
print(total_stem_C_sim_fun)

# Total carbon vales for the bdv total carbon function creation
write_xlsx(total_stem_C_sim_fun, "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/total_stem_C_sim_fun/total_stem_C_sim_function_all.xlsx")


#----------------------------
# END 








































































#-----------------------------------------------------------------------------------------------------------
# In case you want to substitute in the way to make all the data together regarding the carbon pools tables:

# Load necessary libraries
library(dplyr)
library(readxl)
library(writexl)

# Define a function to process each plot
process_plot <- function(plot_id, run_id, plot_data, dataroot) {
  # Filter the data for the specific plot
  plot_filtered <- plot_data %>% filter(run == run_id)
  
  # Determine the year intervals based on the maximum year
  max_year <- max(plot_filtered$year)
  if (max_year >= 480) {
    selected_years <- c(160, 320, 480)
  } else {
    selected_years <- c(120, 240, 360)
  }
  
  # Filter and sort the selected years
  selected_data <- plot_filtered %>% filter(year %in% selected_years) %>% arrange(year)
  
  # Create data frames for each type of carbon simulation
  create_carbon_df <- function(column_name) {
    data.frame(
      plotID = plot_id,
      first_rot = selected_data[[column_name]][selected_data$year == selected_years[1]],
      second_rot = selected_data[[column_name]][selected_data$year == selected_years[2]],
      third_rot = selected_data[[column_name]][selected_data$year == selected_years[3]]
    ) %>% mutate(average = (first_rot + second_rot + third_rot) / 3)
  }
  
  # List of columns to process
  columns_to_process <- c("snags_c", "total_AG_DW_C_sim", "total_DW_C_sim", "total_alive_C_sim", "total_C_sim", "total_stem_C_sim")
  
  # Directory names based on columns
  dir_names <- c("snags_fun", "AG_DW_C_sim_fun", "deadwood_fun", "total_alive_C_sim", "total_carbon_fun", "total_stem_C_sim_fun")
  
  # Process each column and write to corresponding directory
  for (i in seq_along(columns_to_process)) {
    df <- create_carbon_df(columns_to_process[i])
    write_xlsx(df, file.path(dataroot, dir_names[i], paste0(columns_to_process[i], "_table_plot_", plot_id, ".xlsx")))
  }
}

# Define the main function to process all plots
process_all_plots <- function(plot_variables_all, plot_ids, run_ids, dataroot) {
  for (i in seq_along(plot_ids)) {
    process_plot(plot_ids[i], run_ids[i], plot_variables_all, dataroot)
  }
}

# Define the root directory
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/"

# Example plot IDs and run IDs (replace with actual values)
plot_ids <- c("L1_03", "L6_21", "L6_14")
run_ids <- c("DB_CZ_JH1_L1XL1_03_plot.sqlite", "DB_CZ_JH1_L6XL6_21_plot.sqlite", "DB_CZ_JH1_L6XL6_14_plot.sqlite")

# Load the plot_variables_all data (replace with actual loading code)
plot_variables_all <- read.csv("path_to_your_data.csv")  # Replace with actual data loading

# Process all plots
process_all_plots(plot_variables_all, plot_ids, run_ids, dataroot)

# Function to combine all Excel files in a directory into a single data frame
combine_excel_files <- function(directory) {
  excel_files <- list.files(path = directory, pattern = "\\.xlsx$", full.names = TRUE)
  list_of_dfs <- lapply(excel_files, read_xlsx)
  bind_rows(list_of_dfs)
}

# Combine data frames for each type and write the combined files
combined_deadwood_df <- combine_excel_files(file.path(dataroot, "deadwood_fun"))
write_xlsx(combined_deadwood_df, file.path(dataroot, "deadwood_fun", "Deadwood_function_all.xlsx"))

combined_total_carbon_df <- combine_excel_files(file.path(dataroot, "total_carbon_fun"))
write_xlsx(combined_total_carbon_df, file.path(dataroot, "total_carbon_fun", "total_carbon_function_all.xlsx"))

