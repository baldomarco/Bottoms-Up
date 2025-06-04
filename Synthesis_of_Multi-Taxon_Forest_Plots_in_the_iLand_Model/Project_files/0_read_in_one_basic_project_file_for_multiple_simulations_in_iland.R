# this scripts aims to produce different variants of project files.


rm(list=ls()) 
#Marco Baldo, 2024.05.02
#install.packages("xml2")
#install.packages("methods")

library(xml2)
library(methods)



# this work like I read in one project file which I prepared for this "modification".
# And I made modification based on the table what I create in the first part of the script.
# Second part goes on the records of the table and made the project files based on the values inside the table.


# READ IN A BASIC PROJECT FILE
basic<-"C:/_R/R_tables/Project_files/Basic_to_use_to_produce_prfiles.xml"
data<- read_xml(basic)
d<-as_list(data)

print(d$project$system$database$climate[[1]])


#------------------------------------- *****

project.files.to.put<-"C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Project_files/"

#------------------------------------------------------
#  CREATE THE VARIANTS:

# We have climate, browsing and winds

climates<-c("refclim","HadGEM2_CCLM_rcp45","HadGEM2_CCLM_rcp85",
            "CNRM_ALADIN53_rcp45","CNRM_ALADIN53_rcp85",
            "EC-EARTH_RACMO22E_r1_rcp45","EC-EARTH_RACMO22E_r1_rcp85",
            "MPI_CCLM_rcp45","MPI_CCLM_rcp85",
            "NCC_HIRHAM5_rcp45","NCC_HIRHAM5_rcp85")

# we have 50 wind variants
winds<-c("w1", "w2", "w3")
browsings<-c(0,0.5,1,2)


homeroot<-"E:/2023/20230801_browsing_revision/browsing_esperiment/"     # this is the ssd disk, homeroot where the model is run and the outputs are produced

ROOT<-"E:/2023/20230801_browsing_revision/browsing_esperiment/"   # this supposed to be the mounted disk

wind.root<-paste0(ROOT,"scripts/")
abe.root<-paste0(ROOT,"abe/")
climate.root<-paste0(ROOT,"database/")
snap.root<-paste0(ROOT,"init_snapshot/")
gis.root<-paste0(ROOT,"gis/")
spec.root<-paste0(ROOT,"database/")

variants.table.all<-c()
for (p in 1:length(browsings))  {
  for (r in 1:length(winds))  {
    for (m in 1:length(climates)) {
      
      
      wind<-winds[r]
      climate <-climates[m]
      
      
      # CLIMATE DATABASE FILE  
      # I need rcp45 and rcp85 files for the 3 winds.
      #w1_refco2
      #w1_rcp45
      #w2..
      #w3..
      
      # This is giving at the reference run the name refco2 and the reference climate database otherwise go on
      if (m==1){
        clim<-paste0(climate.root,"CZ_region_1961-2018_20211208f.sqlite")
        scen<-"refco2"
      }
      
      # Here are the remaining climate database
      if (m>1){
        scen<-strsplit(climate,'_')[[1]][length(strsplit(climate,'_')[[1]])] # Set the scenario
        clim<-paste0(climate.root,"CZ_region_FORESEEv4_",climate,".sqlite")  # Set the climate
      }
      
      longwindfile<-paste0(wind.root,wind,"_",scen,".txt")
      outputfoldername<-paste0(homeroot,"output")
      
      stand.grid<-paste0("gis/Environment_20220404.txt")
      env.grid<-paste0("gis/ru3_hran_kost.asc")
      env.file<-paste0("gis/Environment_20220404.txt")
      
      spec.file<-paste0('species_param_europe_allometry_20220603_CZ.sqlite')
      
      init.file<-paste0(snap.root,"after1500y_spinup_snapshot_6190.sqlite")
      
      variants.table<-data.frame(wind=wind, 
                                 time.event.file=longwindfile,
                                 scen=scen,
                                 mng.script=paste0(abe.root,"Cz_region_20220513_BAU_SW_management_browsing_final.js"),
                                 csv.file=paste0(abe.root,"CZ_stand_types.csv"),
                                 clim=climate,
                                 climfile=clim,
                                 browsing=browsings[p],
                                 salvaging=0.7,
                                 stand.grid=stand.grid,
                                 env.grid=env.grid,
                                 env.file=env.file,
                                 spec.file=spec.file,
                                 home.root=homeroot,
                                 output.foldername=outputfoldername,
                                 init.snapshot=init.file
      )
      
      
      variants.table<-cbind(variants.table, projectfile=paste0("B",browsings[p],"_",climate, "_",wind,  ".xml" ) )
      
      variants.table<-cbind(variants.table,outputfilename=paste0("B",browsings[p],"_",climate, "_",wind, ".sqlite"))
      
      
      variants.table.all<-rbind(variants.table.all,variants.table)
      
      
    }#r
  } #m
} #p




write.csv(variants.table.all,paste0("project.files.to.putBrowsing_20230513.csv"), quote=F, row.names = F)
#write.csv(variants.table.all,"C:/Users/xzims001/Documents/PROJECT_FILES_WHAT_IF/spinup/Whatif_20200325.csv", quote=F, row.names = F)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------

# now we have the plan, so lets create the project files:
n<-length(variants.table.all[,1])


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
ll<-as.numeric(rmarkovchain(n = 300, chain))

# this is the final order (ll):
refrandomCLIM<-c(11,1,15,27,29,9,7,5,25,11,3,15,12,28,19,24,14,24,29,21,5,29,26,1,19,2,13,6,23,12,21,25,29,16,22,10,19,11,2,9,22,2,7,1,7,2,22,10,15,6,7,10,14,2,24,11,16,27,14,24,29,28,4,9,16,12,0,28,29,16,25,18,9,29,23,14,1,16,27,29,17,4,25,5,21,10,18,13,18,26,9,15,1,5,26,18,6,18,1,15,7,27,7,13,26,14,24,9,4,22,24,6,16,14,4,27,23,19,2,10,9,12,16,17,6,13,22,27,6,28,3,25,22,28,5,15,23,19,24,22,4,7,16,28,26,27,11,17,9,24,17,25,27,16,15,25,21,18,9,22,16,13,15,22,29,18,26,18,23,13,4,22,5,28,17,15,10,18,3,22,7,6,0,12,4,27,20,15,29,15,17,21,6,17,15,2,17,16,27,13,18,3,5,2,4,23,24,19,3,26,24,9,17,13,29,4,1,13,10,21,6,28,16,24,20,22,17,28,5,12,29,13,9,12,21,12,24,26,20,8,7,17,25,29,3,9,14,11,15,7,14,8,22,6,16,29,6,4,18,26,20,8,4,25,10,1,9,7,15,16,3,26,3,2,17,16,25,22,18,0,10,1,22,10,16,17,12,21,13,1,22,21,22,11,26,3,6,5,7,1)

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

n<-nrow(variants.table.all)
for (i in 1:n) {
  
  
  case<-variants.table.all[i,]
  print(case)  
  
  d$project$system$path$home[[1]]<-case$home.root
  d$project$system$path$output[[1]]<-case$output.foldername
  d$project$system$database$out[[1]]<-case$outputfilename
  d$project$system$database$climate[[1]]<-case$climfile
  
  d$project$system$logging$logFile[[1]]<-paste0("log/log.txt")
  
  # TIME EVENTS
  d$project$model$world$timeEventsEnabled[[1]]<-"true"
  d$project$model$world$timeEventsFile[[1]]<-case$time.event.file
  
  # CLIMATE 
  # year numbering start at 0
  
  d$project$model$climate$randomSamplingEnabled[[1]]<-"true"
  d$project$model$climate$randomSamplingList[[1]]<-paste0(as.character(scenrandomCLIM), collapse=' ')
  d$project$model$climate$batchYears[[1]]<-"80"                     #2021-2100
  
  if (case$climfile=="E:/2023/20230801_browsing_revision/browsing_esperiment/database/CZ_region_1961-2018_20211208f.sqlite"){
    d$project$model$climate$randomSamplingList[[1]]<-paste0(as.character(refrandomCLIM), collapse=' ')
    d$project$model$climate$batchYears[[1]]<-"30" 
  }
  
  
  # species parameter file
  d$project$system$database$`in`[[1]]<-"species_param_europe_allometry_20220603_CZ.sqlite"
  
  # gis
  # enviroment file and grid
  #d$project$model$world$environmentGrid[[1]]<-case$env.grid
  #d$project$model$world$environmentFile[[1]]<-case$env.file
  
  # stand grid
  #d$project$model$world$standGrid$fileName[[1]]<-case$stand.grid
  
  # snapshot init
  d$project$model$initialization$file[[1]]<-case$init.snapshot
  
  # MANAGEMENT
  d$project$model$management$enabled[[1]]<-"true"
  d$project$model$management$file[[1]]<-""
  d$project$model$management$abeEnabled[[1]]<-"true"
  d$project$model$management$abe$file[[1]]<-case$mng.script 
  d$project$model$management$abe$agentDataFile[[1]]<-case$csv.file
  
  
  # Browsing
  d$project$model$settings$browsing$enabled[[1]]<-"true"
  d$project$model$settings$browsing$browsingPressure[[1]]<-case$browsing
  
  
  # 
  # d$project$modules$barkbeetle$enabled[[1]]<-"true"
  # d$project$modules$barkbeetle$backgroundInfestationProbability[[1]]<-case$background    #****
  # d$project$modules$barkbeetle$stormInfestationProbability[[1]]<-"0.8"
  # d$project$modules$barkbeetle$baseWinterMortality[[1]]<-"0.4"
  # d$project$modules$barkbeetle$cohortsPerGeneration[[1]]<-"20"
  # d$project$modules$barkbeetle$cohortsPerSisterbrood[[1]]<-"30"
  # d$project$modules$barkbeetle$deadTreeSelectivity[[1]]<-"0.9"
  # d$project$modules$barkbeetle$initialInfestationProbability[[1]]<-case$background   #****** 
  #   
  #   
  
  #SALVAGE
  d$project$user$salvage$remove[[1]]<-case$salvaging   #---------------- FIXED NOW
  
  
  
  r<-as_xml_document(d)
  
  
  
  out<-paste0(project.files.to.put,case$projectfile)
  write_xml(r,out,option = "as_xml")
  
  
  
}     # close the loop of creating project files

shell<-paste0(project.files.to.put,"run_all.sh")
write.table(paste0("ilandc ", variants.table.all$projectfile," 300"),shell, row.names = F,col.names = F,quote = F)


