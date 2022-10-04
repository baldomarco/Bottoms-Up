# R test for LaTeX 

library(gdalUtilities)
setwd("C:/sentinel/")



library(devtools)
devtools::install_github("biomodhub/biomod2", dependencies = TRUE)
install.packages("igraph")
install.packages("LaplacesDemon")
install.packages("GGally")
install.packages("glasso")
install.packages("ade4")
install.packages("PLNmodels")
install.packages("ggplot2")
install.packages("remotes")
remotes::install_github("annescharf/animove")


install.packages("RangeShiftR")  # if you have a very large and robust amount of data, this package is one of the best for create a workflow for SDM analysis


install.packages("gbif")



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

library(rgbif)

library(annescharf)
library(animove)


# setwd("D/")

# You can query GBIF using a species name or a taxonomic key
# One way to find the key for your study species is:
key <- name_suggest(q="Canis lupus", rank="species")$data$key[1]

# Now download occurrence data for your species
# Note: by default, this function will only be limited to 500 observations
# Note: this can takes some time if there are many observations
gbif_data <- occ_search(taxonKey=key, # query by taxonomic key
                        limit=10000, # increase the number of records downloaded
                        hasCoordinate=TRUE) # only get records with geographic coordinates



occ_search(scientificName="Canis lupus")

# There are many options to filter the records returned, e.g.,
gbif_data <- occ_search(scientificName="Canis lupus",
                        continent="europe",  # only get records from Europe
                        limit=10000,
                        hasCoordinate=TRUE,
                        hasGeospatialIssue=FALSE)

gbif_data$data


library(maps)
map(fill=TRUE, col='grey')
points(gbif_data$data$decimalLongitude, 
       gbif_data$data$decimalLatitude, 
       bg=rgb(1,0,0,0.5), pch=21, lwd=0.5)



# Filter only for France (to filter you need to use 2 letter code country)

# There are many options to filter the records returned, e.g.,
gbif_data <- occ_search(scientificName="Canis lupus",
                        country ="FR",  # only get records from Europe
                        limit=10000,
                        hasCoordinate=TRUE,
                        hasGeospatialIssue=FALSE)

gbif_data$data


library(maps)
map(fill=TRUE, col='grey')
points(gbif_data$data$decimalLongitude, 
       gbif_data$data$decimalLatitude, 
       bg=rgb(1,0,0,0.5), pch=21, lwd=0.5)


map(xlim = c(-20, 59),
    ylim = c(35, 71), fill=T, col='grey')
points(gbif_data$data$decimalLongitude, 
       gbif_data$data$decimalLatitude, 
       bg=rgb(1,0,0,0.5), pch=21, lwd=0.5)


install.packages("CoordinateCleaner")
library("CoordinateCleaner")



# go into lions
install.packages("countrycode")
library(countrycode)
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(rgbif)
library(sp)

#obtain data from GBIF via rgbif
dat <- occ_search(scientificName = "Panthera leo", limit = 5000, hasCoordinate = T)

dat <- dat$data

# names(dat) #a lot of columns

#select columns of interest
dat <- dat %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, individualCount,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, datasetName)

# remove records without coordinates
dat <- dat%>%
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude))

# plot the data

#plot data to get an overview
wm <- borders("world", colour="gray50", fill="gray50")
ggplot()+ coord_fixed()+ wm +
  geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  theme_bw()

#--------------------------------------------------------------------------------
# we want to clean the coordinates and data

install.packages("rnaturalearthdata")
library(rnaturalearthdata)


#convert country code from ISO2c to ISO3c
dat$countryCode <-  countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c')


#flag problems
dat <- data.frame(dat)
flags <- clean_coordinates(x = dat,
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                     "zeros", "countries")) # most test are on by default this is delating occurence in order in capitals, in equal lat and log, in gbif location, in the zoo or institutes, in coor. 0. 
## Testing coordinate validity
## Flagged 0 records.
## Testing equal lat/lon
## Flagged 6 records.
## Testing zero coordinates
## Flagged 4 records.
## Testing country capitals
## Flagged 22 records.
## Testing country centroids
## Flagged 8 records.
## Testing country identity
## Flagged 241 records.
## Testing GBIF headquarters, flagging records around Copenhagen
## Flagged 0 records.
## Testing biodiversity institutions
## Flagged 0 records.
## Flagged 267 of 4200 records, EQ = 0.06.

summary(flags)
plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")


install.packages("virtualspecies")
library("virtualspecies")



