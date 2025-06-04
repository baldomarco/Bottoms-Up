https://phytoclast.github.io/vegbook/vegetation-plant-profile-diagrams.html


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
