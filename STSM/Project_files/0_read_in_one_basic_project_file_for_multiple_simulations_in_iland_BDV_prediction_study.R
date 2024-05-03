# this scripts aims to produce different variants of project files.


rm(list=ls()) 
#Marco Baldo, 2024.05.02
#install.packages("xml2")
#install.packages("methods")

library(dplyr)
library(xml2)
library(methods)

# Import the deadwood pools
DWP<- read_excel("C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/CZ_JH1_C_pools_99_plots.xlsx")

DWP <- DWP %>% rename(swdC = swdC_kgha,
                      swdCount = count,
                      youngRefractoryC = yrC_kgha)

DWP <- DWP %>% mutate(swdCount = swdCount *4,
                      otherC = (swdC*0.3)*2 )

#-------------------------------------------------------------------------------
# Get list of env tables in the database
#----------------------------
plot <- c("CZ_JH1_L1XL1_03", "CZ_JH1_L1XL1_07" ,
            "CZ_JH1_L1XL1_10", "CZ_JH1_L1XL1_13", "CZ_JH1_L1XL1_17", "CZ_JH1_L1XL1_18",
            "CZ_JH1_L1XL1_22", "CZ_JH1_L1XL1_24", "CZ_JH1_L1XL1_26", "CZ_JH1_L1XL1_27",
            "CZ_JH1_L1XL1_31" ,"CZ_JH1_L1XL1_33" ,"CZ_JH1_L1XL1_34" ,"CZ_JH1_L1XL1_36",
            "CZ_JH1_L1XL1_38", "CZ_JH1_L1XL1_43", "CZ_JH1_L1XL1_44", "CZ_JH1_L1XL1_48",
            "CZ_JH1_L2XL2_01", "CZ_JH1_L2XL2_05", "CZ_JH1_L2XL2_06",
            "CZ_JH1_L2XL2_09", "CZ_JH1_L2XL2_13", "CZ_JH1_L2XL2_14" ,"CZ_JH1_L2XL2_16",
            "CZ_JH1_L2XL2_17", "CZ_JH1_L2XL2_18", "CZ_JH1_L2XL2_20",
            "CZ_JH1_L2XL2_26", "CZ_JH1_L2XL2_27", "CZ_JH1_L2XL2_30", "CZ_JH1_L2XL2_32",
            "CZ_JH1_L2XL2_33", "CZ_JH1_L2XL2_34", "CZ_JH1_L2XL2_38" ,"CZ_JH1_L2XL2_44",
            "CZ_JH1_L3XL3_02" ,"CZ_JH1_L3XL3_06", "CZ_JH1_L3XL3_09", "CZ_JH1_L3XL3_10",
            "CZ_JH1_L3XL3_12", "CZ_JH1_L3XL3_16", "CZ_JH1_L3XL3_18", "CZ_JH1_L3XL3_23",
            "CZ_JH1_L3XL3_24", "CZ_JH1_L3XL3_26", "CZ_JH1_L3XL3_27", "CZ_JH1_L3XL3_31",
            "CZ_JH1_L3XL3_32",  "CZ_JH1_L3XL3_34", 
            "CZ_JH1_L3XL3_37","CZ_JH1_L3XL3_38", "CZ_JH1_L3XL3_41", "CZ_JH1_L3XL3_42",
            "CZ_JH1_L4XL4_02" ,"CZ_JH1_L4XL4_04" ,"CZ_JH1_L4XL4_06", "CZ_JH1_L4XL4_09",
            "CZ_JH1_L4XL4_10", "CZ_JH1_L4XL4_11", "CZ_JH1_L4XL4_16", "CZ_JH1_L4XL4_17",
            "CZ_JH1_L4XL4_18" ,"CZ_JH1_L4XL4_20", "CZ_JH1_L4XL4_23", "CZ_JH1_L4XL4_25",
            "CZ_JH1_L4XL4_26" ,"CZ_JH1_L4XL4_27", "CZ_JH1_L4XL4_30" ,"CZ_JH1_L4XL4_31",
            "CZ_JH1_L4XL4_32" ,"CZ_JH1_L4XL4_33" ,"CZ_JH1_L4XL4_39" ,"CZ_JH1_L4XL4_42",
            "CZ_JH1_L5XL5_01"  ,"CZ_JH1_L5XL5_05" ,"CZ_JH1_L5XL5_12",
            "CZ_JH1_L5XL5_18" ,"CZ_JH1_L5XL5_19" ,"CZ_JH1_L5XL5_21", "CZ_JH1_L5XL5_25",
            "CZ_JH1_L5XL5_28" ,"CZ_JH1_L5XL5_32" ,"CZ_JH1_L5XL5_33", "CZ_JH1_L5XL5_37",
            "CZ_JH1_L5XL5_38" ,"CZ_JH1_L6XL6_01" ,"CZ_JH1_L6XL6_02" ,"CZ_JH1_L6XL6_03",
            "CZ_JH1_L6XL6_04" ,"CZ_JH1_L6XL6_05" ,"CZ_JH1_L6XL6_08" ,"CZ_JH1_L6XL6_10",
            "CZ_JH1_L6XL6_11" ,"CZ_JH1_L6XL6_14" ,"CZ_JH1_L6XL6_15", "CZ_JH1_L6XL6_17",
            "CZ_JH1_L6XL6_19" ,"CZ_JH1_L6XL6_21")


#--------------------------------------
# this work like I read in one project file which I prepared for this "modification".
# And I made modification based on the table what I create in the first part of the script.
# Second part goes on the records of the table and made the project files based on the values inside the table.


# READ IN A BASIC PROJECT FILE
basic<-"C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/2022_Artifical_bottom_L1_10.xml"
data<- read_xml(basic)
d<-as_list(data)

print(d$project$system$database$climate[[1]])


#------------------------------------- *****

project.files.to.put<-"C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/project_files/"

#------------------------------------------------------
#  CREATE THE VARIANTS:

# We have climate, browsing and winds

#climates<-c("refclim","HadGEM2_CCLM_rcp45","HadGEM2_CCLM_rcp85",
 #           "CNRM_ALADIN53_rcp45","CNRM_ALADIN53_rcp85",
  #          "EC-EARTH_RACMO22E_r1_rcp45","EC-EARTH_RACMO22E_r1_rcp85",
   #         "MPI_CCLM_rcp45","MPI_CCLM_rcp85",
    #        "NCC_HIRHAM5_rcp45","NCC_HIRHAM5_rcp85")

# we have 50 wind variants
# winds<-c("w1", "w2", "w3")
# browsings<-c(0,0.5,1,2)

homeroot<-"C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/"     # this is the ssd disk, homeroot where the model is run and the outputs are produced

ROOT<-"C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/"         # this supposed to be the mounted disk

env.root<-paste0(ROOT,"gis/env_table/")
abe.root<-paste0(ROOT,"abe/")
climate.root<-paste0(ROOT,"database/")
init<-paste0(ROOT,"init/init_with_ages/")
gis.root<-paste0(ROOT,"gis/")
spec.root<-paste0(ROOT,"database/")

      env.file<-paste0(env.root,"Environment_",plot,".txt")  # Set the climate
      env.file
      # Generate init_name by removing first 10 characters from plot variable
      init_name <- paste0(substr(plot, 11, nchar(plot)), "_init_age_CORR.txt")
      init_name
      stand.grid<-paste0("gis/plot_work.asc")
      env.grid<-paste0("gis/environment_grid_plot.asc")
      #env.file<-paste0("gis/Environment_110.txt") 
      
      outputfoldername<-paste0(homeroot,"output")
      
      # Get list of simulation folders
      # simulation_folders <- list.files("gis/", pattern = "^[0-9]+$", full.names = TRUE)
      
      spec.file<-paste0('species_param_europe_allometry_20220603_CZ.sqlite')
      
      #init.file<-paste0(root,"init/")
      
      variants.table<-data.frame(#time.event.file=clim_name,
                                 plot=plot,
                                 mng.script=paste0(abe.root,"01_abe_bottoms_up_L1_10.js"),
                                 csv.file=paste0(abe.root,"CZ_stand_types.csv"),
                                 salvaging=0.7,
                                 stand.grid=stand.grid,
                                 env.grid=env.grid,
                                 env.file=env.file, 
                                 spec.file=spec.file,
                                 home.root=homeroot,
                                 output.foldername=outputfoldername,
                                 init_file=init_name,
                                 swdC = DWP$swdC,
                                 youngRefractoryC = DWP$youngRefractoryC,
                                 swdCount = DWP$swdCount,
                                 otherC = DWP$otherC)
      
      variants.table
      
      variants.table<-cbind(variants.table, project_file_name=paste0("Project_", plot, ".xml"))
      variants.table

      variants.table<-cbind(variants.table,output_file_name=paste0("DB_", plot, ".sqlite"))
      variants.table



write.csv(variants.table,paste0(ROOT,"project.files.to.putBottomsup.csv"), quote=F, row.names = F)
#write.csv(variants.table.all,"C:/Users/xzims001/Documents/PROJECT_FILES_WHAT_IF/spinup/Whatif_20200325.csv", quote=F, row.names = F)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------

# now we have the plan, so lets create the project files:
n<-length(variants.table[,1])


#----------------------- REFLCIM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# For the reference climate I want to have 300 years, but not repeating the same year after each other!
library(markovchain)
first30<-as.character(c(0:29))
# transition matrix: each state can go to another state with probability 1/30
tmatrix<-matrix(1/29, 30,30)
for (dd in 1:30) {
  tmatrix[dd,dd]<-0
}

# Markov chain
chain <- new("markovchain", states = first30, transitionMatrix = tmatrix)
# sample the Markov chain
ll<-as.numeric(rmarkovchain(n = 400, chain))

# this is the final order (ll):
refrandomCLIM<-c(11,1,15,27,29,9,7,5,25,11,3,15,12,28,19,24,14,24,29,21,5,29,26,1,19,2,13,6,23,12,21,25,29,16,22,10,19,11,2,9,22,2,7,1,7,2,22,10,15,6,7,10,14,2,24,11,16,27,14,24,29,28,4,9,16,12,0,28,29,16,25,18,9,29,23,14,1,16,27,29,17,4,25,5,21,10,18,13,18,26,9,15,1,5,26,18,6,18,1,15,7,27,7,13,26,14,24,9,4,22,24,6,16,14,4,27,23,19,2,10,9,12,16,17,6,13,22,27,6,28,3,25,22,28,5,15,23,19,24,22,4,7,16,28,26,27,11,17,9,24,17,25,27,16,15,25,21,18,9,22,16,13,15,22,29,18,26,18,23,13,4,22,5,28,17,15,10,18,3,22,7,6,0,12,4,27,20,15,29,15,17,21,6,17,15,2,17,16,27,13,18,3,5,2,4,23,24,19,3,26,24,9,17,13,29,4,1,13,10,21,6,28,16,24,20,22,17,28,5,12,29,13,9,12,21,12,24,26,20,8,7,17,25,29,3,9,14,11,15,7,14,8,22,6,16,29,6,4,18,26,20,8,4,25,10,1,9,7,15,16,3,26,3,2,17,16,25,22,18,0,10,1,22,10,16,17,12,21,13,1,22,21,22,11,26,3,6,5,7,1, 9 ,22 ,14 ,27 ,22 ,19 , 7  ,1  ,2 ,26 , 1 ,16 ,13 , 2 ,28 ,23 ,20 , 5 ,17 ,22 ,20 ,19 , 1 , 0 ,21, 12 ,29 , 5 ,10 , 5 ,17 ,21 ,27 ,22 ,15 ,22 ,19  ,9 , 4 ,22 ,21 ,17 , 4 , 1 ,22 ,25 ,16  ,4 ,10,27 ,15 ,25 ,20 ,27 ,23  ,0 ,29 ,18, 21  ,1 ,20  ,4  ,7  ,6 ,18  ,1  ,2 ,23 ,11 ,13 ,29 ,24 ,11 ,17, 15  ,1  ,7, 11,  3, 23, 24, 14, 22,  2, 12,  5,  9, 23, 22,  0, 20, 11, 13, 20, 14, 16,  6, 24, 12)                                                                        

#----------------------- SCENARIO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# for climate change we need 2021-2100: 80year, than replicate the last 25 year randomly. to have 300y: 

last25<-as.character(c(55:79))
# transition matrix: each state can go to another state with probability 1/30
tmatrix<-matrix(1/24, 25,25)
for (dd in 1:25) {
  tmatrix[dd,dd]<-0
}

chain <- new("markovchain", states = last25, transitionMatrix = tmatrix)
# sample the Markov chain
ll2<-as.numeric(rmarkovchain(n = 220, chain))


# this is the final order:
last220<-c(77,73,64,73,71,69,67,71,59,67,65,71,62,76,68,59,67,74,55,60,74,62,57,60,55,68,59,74,75,68,62,60,56,62,79,75,79,70,76,77,71,79,69,58,77,56,64,58,76,77,68,57,70,74,63,62,65,56,79,63,57,62,68,71,63,62,61,77,55,57,55,61,78,60,71,56,63,64,63,69,74,65,55,74,59,76,78,58,78,64,63,66,70,65,72,63,59,79,68,69,63,76,62,60,71,67,76,79,74,61,60,79,77,70,74,59,57,61,78,67,74,70,56,60,66,67,59,61,65,68,69,58,72,58,69,76,71,67,71,75,65,79,74,75,71,72,59,69,73,62,69,77,76,77,76,55,79,70,61,79,66,60,78,72,68,65,75,66,57,78,77,64,63,69,67,70,72,57,71,70,72,58,60,63,79,66,78,66,58,65,73,71,73,55,62,61,57,64,76,75,62,55,79,68,71,69,57,76,71,61,65,74,72,76,58,59,58,77,59,62)

scenrandomCLIM<-c(0:79,last220)

n<-nrow(variants.table)
for (i in 1:n) {
  
  case<-variants.table[i,]
  print(case)  
  
  #d$project$system$path$home[[1]]<-case$home.root
  d$project$system$path$output[[1]]<-case$output.foldername
  d$project$system$database$out[[1]]<-case$output_file_name
  #d$project$system$database$climate[[1]]<-case$climfile
  
  d$project$system$logging$logFile[[1]]<-paste0("log/",plot,"log.txt")
  
  # TIME EVENTS
  d$project$model$world$timeEventsEnabled[[1]]<-"false"
  
  # Activate in case you have wind event and put the name of the txt wind event file at the place of time.event.file
  #d$project$model$world$timeEventsFile[[1]]<-case$time.event.file 
  
  # CLIMATE (in case you will have climate change scenarios)
  # year numbering start at 0
  
  # Add in case of climate change
  # d$project$model$climate$randomSamplingEnabled[[1]]<-"true"
  # d$project$model$climate$randomSamplingList[[1]]<-paste0(as.character(scenrandomCLIM), collapse=' ')
  # d$project$model$climate$batchYears[[1]]<-"80"                     #2021-2100
  
  #  if (case$climfile=="E:/2023/20230801_browsing_revision/browsing_esperiment/database/CZ_region_1961-2018_20211208f.sqlite"){
  #  d$project$model$climate$randomSamplingList[[1]]<-paste0(as.character(refrandomCLIM), collapse=' ')
  #  d$project$model$climate$batchYears[[1]]<-"30" 
  #  }
  
    case$climfile=="C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/database/db_clim_cz_plot.sqlite"
    d$project$model$climate$randomSamplingList[[1]]<-paste0(as.character(refrandomCLIM), collapse=' ')
    d$project$model$climate$batchYears[[1]]<-"30" 
  
  # CO2
    d$project$model$species$CO2Response$compensationPoint[[1]] <- 80
    d$project$model$species$CO2Response$beta0[[1]] <- 0.3
    
  # species parameter file
  d$project$system$database$`in`[[1]]<-"species_param_kostelec_allometry_20220603_CZ.sqlite"
  
  # gis
  # enviroment file and grid
  d$project$model$world$environmentGrid[[1]]<-case$env.grid
  d$project$model$world$environmentFile[[1]]<-case$env.file
  
  # stand grid
  d$project$model$world$standGrid$fileName[[1]]<-case$stand.grid
  
  # init input tables
  d$project$model$site$youngRefractoryC[[1]]<-case$youngRefractoryC
  
  # Deadwood pools
  d$project$model$initialization$snags$swdC[[1]]<- case$swdC
  
  d$project$model$initialization$snags$swdC[[1]]<- case$swdC
  d$project$model$initialization$snags$swdCount[[1]]<-case$swdCount
  d$project$model$initialization$snags$otherC[[1]]<-case$otherC
  d$project$model$initialization$snags$otherAbovegroundFraction[[1]]<- 0.3
  
  # MANAGEMENT
  d$project$model$management$enabled[[1]]<-"true"
  d$project$model$management$file[[1]]<-""
  d$project$model$management$abeEnabled[[1]]<-"true"
  d$project$model$management$abe$file[[1]]<-case$mng.script 
  d$project$model$management$abe$agentDataFile[[1]]<-case$csv.file
  
  
  # Browsing
  d$project$model$settings$browsing$enabled[[1]]<-"true"
  d$project$model$settings$browsing$browsingPressure[[1]]<- 0.8
  
  
  # Barkbeetle module
   d$project$modules$barkbeetle$enabled[[1]]<-"true"
   d$project$modules$barkbeetle$backgroundInfestationProbability[[1]]<-case$background
   d$project$modules$barkbeetle$stormInfestationProbability[[1]]<-"0.05"
   d$project$modules$barkbeetle$baseWinterMortality[[1]]<-"0.4"
   d$project$modules$barkbeetle$cohortsPerGeneration[[1]]<-"20"
   d$project$modules$barkbeetle$cohortsPerSisterbrood[[1]]<-"30"
   d$project$modules$barkbeetle$deadTreeSelectivity[[1]]<-"0.85*x+0.15"
   d$project$modules$barkbeetle$initialInfestationProbability[[1]]<-case$background   
   d$project$modules$barkbeetle$referenceClimate$tableName[[1]]<- plot
  #   
  
  # SALVAGE
  d$project$user$salvage$remove[[1]]<-case$salvaging   #---------------- FIXED NOW
  
  # Create XML document
  p <- as_xml_document(d)
  
  # Generate a unique filename for this XML document
  out <- paste0(project.files.to.put, case$project_file_name)
  
  # Write XML document to file
  write_xml(p, out, option = "as_xml")
  
}     # close the loop of creating project files

shell<-paste0(project.files.to.put,"Bottom_UP_run_all.bat")
write.table(paste0("ilandc.exe ", variants.table$project_file_name," 400"), shell, row.names = F,col.names = F,quote = F)


