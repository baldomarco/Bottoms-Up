# biomod2

##   https://biomodhub.github.io/biomod2/articles/examples_1_mainFunctions.html


devtools::install_github("biomodhub/biomod2", dependencies = TRUE, force = TRUE)
install.packages("igraph")
install.packages("LaplacesDemon")
install.packages("GGally")
install.packages("glasso")
install.packages("ade4")
install.packages("PLNmodels")
install.packages("ggplot2")
install.packages("remotes")


library(biomodhub)
library(biomod2)
library(igraph)
library(LaplacesDemon)
library(GGally)
library(glasso)
library(ade4)
library(PLNmodels)
library(ggplot2)
library(remotes)

library(biomod2)
library(raster)

# Load species occurrences (6 species available)
myFile <- system.file('external/species/mammals_table.csv', package = 'biomod2')
DataSpecies <- read.csv(myFile, row.names = 1)
head(DataSpecies)

# Select the name of the studied species
myRespName <- 'GuloGulo'

# Get corresponding presence/absence data
myResp <- as.numeric(DataSpecies[, myRespName])

# Get corresponding XY coordinates
myRespXY <- DataSpecies[, c('X_WGS84', 'Y_WGS84')]

# Load environmental variables extracted from BIOCLIM (bio_3, bio_4, bio_7, bio_11 & bio_12)
myFiles <- paste0('external/bioclim/current/bio', c(3, 4, 7, 11, 12), '.grd')
myExpl <- raster::stack(system.file(myFiles, package = 'biomod2'))


# Format Data with true absences
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName)
myBiomodData
plot(myBiomodData)
plot(myExpl)
plot(myRespXY)

myBiomodData
myBiomodOptions


# Transform true absences into potential pseudo-absences
 myResp.PA <- ifelse(myResp == 1, 1, NA)
 
 # Format Data with pseudo-absences : random method
 myBiomodData.r <- BIOMOD_FormatingData(resp.var = myResp.PA,
                                        expl.var = myExpl,
                                        resp.xy = myRespXY,
                                        resp.name = myRespName,
                                        PA.nb.rep = 4,
                                        PA.nb.absences = 1000,
                                        PA.strategy = 'random')
 
 # Format Data with pseudo-absences : disk method
 myBiomodData.d <- BIOMOD_FormatingData(resp.var = myResp.PA,
                                        expl.var = myExpl,
                                        resp.xy = myRespXY,
                                        resp.name = myRespName,
                                        PA.nb.rep = 4,
                                        PA.nb.absences = 500,
                                        PA.strategy = 'disk',
                                        PA.dist.min = 5,
                                        PA.dist.max = 35)
 
 # Format Data with pseudo-absences : SRE method
 myBiomodData.s <- BIOMOD_FormatingData(resp.var = myResp.PA,
                                        expl.var = myExpl,
                                        resp.xy = myRespXY,
                                        resp.name = myRespName,
                                        PA.nb.rep = 4,
                                        PA.nb.absences = 1000,
                                        PA.strategy = 'sre',
                                        PA.sre.quant = 0.025)
 
 # Format Data with pseudo-absences : user.defined method
 myPAtable <- data.frame(PA1 = ifelse(myResp == 1, TRUE, FALSE),
                         PA2 = ifelse(myResp == 1, TRUE, FALSE))
 for (i in 1:ncol(myPAtable)) myPAtable[sample(which(myPAtable[, i] == FALSE), 500), i] = TRUE
 myBiomodData.u <- BIOMOD_FormatingData(resp.var = myResp.PA,
                                        expl.var = myExpl,
                                        resp.xy = myRespXY,
                                        resp.name = myRespName,
                                        PA.strategy = 'user.defined',
                                        PA.user.table = myPAtable)
 
 myBiomodData.r
 myBiomodData.d
 myBiomodData.s
 myBiomodData.u
 plot(myBiomodData.r)
 plot(myBiomodData.d)
 plot(myBiomodData.s)
 plot(myBiomodData.u)

# Print default modeling options
bm_DefaultModelingOptions()

# Create default modeling options
myBiomodOptions <- BIOMOD_ModelingOptions(GLM = list(interaction.level=1))
myBiomodOptions

# Create the different validation datasets
myBiomodCV <- BIOMOD_CrossValidation(bm.format = myBiomodData)
head(myBiomodCV)

# # Several validation strategies can be combined
# DataSplitTable.b <- BIOMOD_CrossValidation(bm.format = myBiomodData,
#                                            k = 5,
#                                            nb.rep = 2,
#                                            do.full.models = FALSE)
# DataSplitTable.y <- BIOMOD_CrossValidation(bm.format = myBiomodData,
#                                            k = 2,
#                                            do.stratification = TRUE,
#                                            method = "y")
# colnames(DataSplitTable.y)[1:2] <- c("RUN11", "RUN12")
# myBiomodCV <- cbind(DataSplitTable.b, DataSplitTable.y)
# head(myBiomodCV)

 # Part (or totality) of the print can be copied and customized
 # Below is an example to compute quadratic GLM and select best model with 'BIC' criterium
 myBiomodOptions <- BIOMOD_ModelingOptions(
   GLM = list(type = 'quadratic',
              interaction.level = 0,
              myFormula = NULL,
              test = 'BIC',
              family = 'binomial',
              control = glm.control(epsilon = 1e-08,
                                    maxit = 1000,
                                    trace = FALSE)))
 myBiomodOptions
 
 # It is also possible to give a specific GLM formula
 myForm <- 'Sp277 ~ bio3 + log(bio10) + poly(bio16, 2) + bio19 + bio3:bio19'
 myBiomodOptions <- BIOMOD_ModelingOptions(GLM = list(myFormula = formula(myForm)))
 myBiomodOptions

# Model single models
myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData,
                                    bm.options = myBiomodOptions,
                                    model = c("RF","GAM","GLM", "GBM"),
                                    modeling.id = 'AllModels',
                                    nb.rep = 2,
                                    data.split.perc = 80,
                                    # data.split.table = myBiomodCV,
                                    var.import = 3,
                                    metric.eval = c('TSS','ROC'),
                                    do.full.models = FALSE)
                                    # seed.val = 123)
                                    # nb.cpu = 8)
myBiomodModelOut

# Get evaluation scores & variables importance
get_evaluations(myBiomodModelOut, as.data.frame= TRUE)
get_variables_importance(myBiomodModelOut, as.data.frame = TRUE)

# Represent evaluation scores & variables importance
bm_PlotEvalMean(bm.out = myBiomodModelOut)
bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'algo'))
bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'run'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'algo'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'dataset'))
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'expl.var', 'dataset'))

# Represent response curves
bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                      models.chosen = get_built_models(myBiomodModelOut)[c(1:3, 12:14)],
                      fixed.var = 'median')
bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                      models.chosen = get_built_models(myBiomodModelOut)[c(1:3, 12:14)],
                      fixed.var = 'min')
bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                      models.chosen = get_built_models(myBiomodModelOut)[3],
                      fixed.var = 'median',
                      do.bivariate = TRUE)



# Model ensemble models
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut,
                                      models.chosen = 'all',
                                      em.by = 'all',
                                      metric.select = c('TSS'),
                                      metric.select.thresh = c(0.7),
                                      var.import = 3,
                                      metric.eval = c('TSS', 'ROC'),
                                      prob.mean = FALSE,
                                      prob.median = FALSE,
                                      prob.cv = TRUE,
                                      prob.ci = TRUE,
                                      prob.ci.alpha = 0.05,
                                      committee.averaging = TRUE,
                                      prob.mean.weight = TRUE,
                                      prob.mean.weight.decay = 'proportional')
myBiomodEM

# Get evaluation scores & variables importance
get_evaluations(myBiomodEM, as.data.frame = TRUE)
get_variables_importance(myBiomodEM, as.data.frame = TRUE)

# Represent evaluation scores & variables importance
bm_PlotEvalMean(bm.out = myBiomodEM, group.by = 'model')
bm_PlotEvalBoxplot(bm.out = myBiomodEM, group.by = c('model', 'model'))
bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'model', 'model'))
bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('expl.var', 'model', 'dataset'))
bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('model', 'expl.var', 'dataset'))

# Represent response curves
bm_PlotResponseCurves(bm.out = myBiomodEM, 
                      models.chosen = get_built_models(myBiomodEM)[c(1, 6, 7)],
                      fixed.var = 'median')
bm_PlotResponseCurves(bm.out = myBiomodEM, 
                      models.chosen = get_built_models(myBiomodEM)[c(1, 6, 7)],
                      fixed.var = 'min')
bm_PlotResponseCurves(bm.out = myBiomodEM, 
                      models.chosen = get_built_models(myBiomodEM)[7],
                      fixed.var = 'median',
                      do.bivariate = TRUE)

#-------------------------------------------------------------------------------
# Evaluate models with Boyce index and MPA
myBiomodPO <- BIOMOD_PresenceOnly(bm.mod = myBiomodModelOut,
                                  bm.em = myBiomodEM)
myBiomodPO

# Evaluate models with Boyce index and MPA (using background data)
myBiomodPO <- BIOMOD_PresenceOnly(bm.mod = myBiomodModelOut,
                                  bm.em = myBiomodEM, 
                                  bg.env = getValues(myExpl))
plot(myBiomodPO)
myBiomodPO

#------------------------------------------------------------------------------
# Project single models
myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                  proj.name = 'Current',
                                  new.env = myExpl,
                                  models.chosen = 'all',
                                  metric.binary = 'all',
                                  metric.filter = 'all',
                                  build.clamping.mask = TRUE)
myBiomodProj
plot(myBiomodProj)

#------------------------------------------------------------------------------

# Project ensemble models (from single projections)
myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM, 
                                             bm.proj = myBiomodProj,
                                             models.chosen = 'all',
                                             metric.binary = 'all',
                                             metric.filter = 'all')

# Project ensemble models (building single projections)
myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                             proj.name = 'CurrentEM',
                                             new.env = myExpl,
                                             models.chosen = 'all',
                                             metric.binary = 'all',
                                             metric.filter = 'all')
myBiomodEMProj
plot(myBiomodEMProj)

#------------------------------------------------------------------------------

# Load environmental variables extracted from BIOCLIM (bio_3, bio_4, bio_7, bio_11 & bio_12)
myFiles = paste0('external/bioclim/future/bio', c(3, 4, 7, 11, 12), '.grd')
myExplFuture = raster::stack(system.file(myFiles, package = 'biomod2'))

# Project onto future conditions
myBiomodProjectionFuture <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                              proj.name = 'Future',
                                              new.env = myExplFuture,
                                              models.chosen = 'all',
                                              metric.binary = 'TSS',
                                              build.clamping.mask = TRUE)

# Load current and future binary projections
CurrentProj <- stack("GuloGulo/proj_Current/proj_Current_GuloGulo_TSSbin.grd")
FutureProj <- stack("GuloGulo/proj_Future/proj_Future_GuloGulo_TSSbin.grd")

# Compute differences
myBiomodRangeSize <- BIOMOD_RangeSize(proj.current = CurrentProj, proj.future = FutureProj)

myBiomodRangeSize$Compt.By.Models
plot(myBiomodRangeSize$Diff.By.Pixel)


# Represent main results 
gg = bm_PlotRangeSize(bm.range = myBiomodRangeSize, 
                      do.count = TRUE,
                      do.perc = TRUE,
                      do.maps = TRUE,
                      do.mean = TRUE,
                      do.plot = TRUE,
                      row.names = c("Species", "Dataset", "Run", "Algo"))



x11() # it is a function to open a plot or several plots

