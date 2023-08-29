library(ggplot2)
library(GGally)
library(cowplot)
library(corrplot)
library(dplyr)
library(fields)

install.packages("readxl")
library(readxl)

# tab2 <- read_xlsx("C:/iLand/2023/plot_bottoms_up/Jenik/Bdv_predictors_clean_correlation.xlsx") 
# tab1 <-read.csv("C:/iLand/2022/20220604_final_test/DB_final/variables.all_20220708.csv")

tab2 <- read_xlsx("C:/iLand/2023/plot_bottoms_up/Jenik/Bdv_predictors_table_final.xlsx")

head(tab1)
head(tab2)

plot(tab2$age, tab2$basal_area)

col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", "cyan", "#007FFF", "blue", "#00007F"))


#---------------------------------- just do the correlation plot all together
a.num<-tab2[,3:15]

# Look them all:
par(mfrow = c(1, 1), pty="m", mar=c(3,3,3,3), oma=c(0,0,0,0))
corrplot.mixed(cor(a.num),upper.col = col4(10),lower.col = "black", mar=c(0,0,0,0), tl.pos = "d")#, diag = "l")


#---------------------------------- 
# just do the correlation plot with the selected variables

scenarios <- tab2 %>% 
  filter(case == "B0_Tby1_PRby0")

a.num_selected <-scenarios[,-1]
# Look them all:
par(mfrow = c(1, 1), pty="m", mar=c(3,3,3,3), oma=c(0,0,0,0))
corrplot.mixed(cor(a.num_selected),upper.col = col4(10),lower.col = "black", mar=c(0,0,0,0), tl.pos = "d")#, diag = "l")


#---------------------------------- just do the correlation plot with the selected variables

ggpairs(a.num)


# Second part - Calculate Shannon of every plot based on the species basal area

library(vegan)

# Calculate the basal area per every tree

raw_data <- "C:/iLand/2023/plot_bottoms_up/Jenik/Raw_data_structure_CZ_JH1_final.xlsx"

excel_sheets(raw_data)

raw_data_sp <- "live_dead_trees"

data <- read_excel(raw_data,raw_data_sp, col_names=T)

new_data <- data %>% 
  mutate(basal_area = pi * (treedb / 200)^2)

print(new_data)

write.csv(new_data, "C:/iLand/2023/plot_bottoms_up/Jenik/Raw_data_with_basal_area.csv")

# Sum BA for every species in every plot

# Group by plotID and species, then calculate the sum of basal areas

summed_tree_areas <- new_data %>%
  group_by(plotID, treesp) %>%
  summarize(total_basal_area = sum(basal_area))

# Print the resulting dataframe
print(summed_tree_areas)

unique_plots <- unique(summed_tree_areas$plotID)  # alternative unique_plots <- unique(CZ_JH1[,"plotID"])
print(unique_plots) # 99 plots

write.csv(summed_tree_areas, "C:/iLand/2023/plot_bottoms_up/Jenik/summed_tree_areas.csv")

# Calculate the Shannon diversity index
shannon_index <- summed_tree_areas %>%
  group_by(plotID) %>%
  summarize(shannon = diversity(total_basal_area, base = exp(1)))

# Print the resulting dataframe
print(shannon_index)

write.csv(shannon_index, "C:/iLand/2023/plot_bottoms_up/Jenik/shannon_index_ba.csv")


#-------------------------------------------------------------------------------
# To create the basal area of any plots per species from their dbh

# Create a vector of DBH measurements for each tree in the plot
dbh_measurements <- c(30, 40, 35, 25, 42, 38)  # Replace with your actual DBH measurements

# Calculate the basal area for each tree
basal_area_each_tree <- pi * (dbh_measurements / 200)^2  # Dividing by 2 and squaring gives area in square meters

# Calculate the total basal area for the plot by summing the individual tree basal areas
total_basal_area <- sum(basal_area_each_tree)

# Print the results
cat("Basal Area for Each Tree (in square meters):\n")
print(basal_area_each_tree)

cat("\nTotal Basal Area for the Plot (in square meters):\n")
print(total_basal_area)




#-------------------------------------------------------------------------------------------------

# we select here the variables that we want based on the correlation plot:
# early species proportion
a <-data.frame(year=tab1$year,case=tab1$case,dbh=tab1$dbh,esp_BA_prop=tab1$esp_BA_prop,tot_carbon=tab1$tot_carbon, age=tab1$age)

head(a)

# we select here the variables that we want based on the correlation plot:
k_csp<-data.frame(year=tab2$year,case=tab2$case,dbh=tab2$dbh,csp_BA_prop=tab2$csp_BA_prop,tot_carbon=tab2$tot_carbon, age=tab2$age)

head(k_csp)

############################################################################# not smoothed
#--------------------------- dbh, earlyspecies, total carbon


library(plotly)
data<-data.frame(x=k_csp$dbh,y=k_csp$csp_BA_prop,z=k_csp$tot_carbon, case=k_csp$case)
plot_ly(data, x = ~x, y = ~y, z = ~z, split=~case, type = 'scatter3d', mode = 'lines')

#--------------------------- age, earlyspecies, total carbon

data<-data.frame(x=a$age,y=a$esp_BA_prop,z=a$tot_carbon,years=c(1:nyears), case=a$case)
plot_ly(data, x = ~x, y = ~y, z = ~z, split=~case, type = 'scatter3d', mode = 'lines')


#--------------------------- other trial:
data<-data.frame(x=a$dbh,y=a$H.BA,z=a$tot_carbon,years=c(1:nyears), case=a$case)
plot_ly(data, x = ~x, y = ~y, z = ~z, split=~case, type = 'scatter3d', mode = 'lines')

#---------------------------------------------------------------------------------------------------------
# Do the smoothing

library("zoo")

k_esp<-a_esp %>% group_by(case) %>% mutate(s.dbh=rollmean(dbh,11,align = "left",na.pad = T), 
                                           s.esp_BA_prob=rollmean(esp_BA_prop,11,align = "left",na.pad = T),
                                           s.tot_carbon=rollmean(tot_carbon,11,align = "left",na.pad = T))

# IT WAS THE PREVIOUS 
# data<-data.frame(x=k_esp$s.dbh,y=k_esp$s.esp_BA_prob,z=k_esp$s.tot_carbon,years=c(1:nyears), case=k_esp$case)

data<-data.frame(x=k_esp$s.dbh,y=k_esp$s.esp_BA_prob,z=k_esp$s.tot_carbon, case=k_esp$case)
plot_ly(data, x = ~x, y = ~y, z = ~z, split=~case, type = 'scatter3d', mode = 'lines')

#+ stat_smooth()

# 3D plot second table with Conifer proportion


k_csp<-k_csp %>% group_by(case) %>% mutate(s.dbh=rollmean(dbh,20,align = "left",na.pad = T), 
                                           s.csp_BA_prob=rollmean(csp_BA_prop,20,align = "left",na.pad = T),
                                           s.age=rollmean(age,20,align = "left",na.pad = T),
                                           s.tot_carbon=rollmean(tot_carbon,20,align = "left",na.pad = T))


#csp<-data.frame(x=k_csp$s.dbh,y=k_csp$s.csp_BA_prob,z=k_csp$s.tot_carbon,years=c(1:nyears), case=a_csp$case)
#plot_ly(data, x = ~x, y = ~y, z = ~z, split=~case, type = 'scatter3d', mode = 'lines')



csp<-data.frame(x=k_csp$s.age,y=k_csp$s.csp_BA_prob,z=k_csp$s.tot_carbon, case=k_csp$case)


a <- plot_ly(csp, x = ~x, y = ~y, z = ~z, split=~case, type = 'scatter3d', mode = 'lines', 
             colors = c("grey50", "blue", "red"))
a

pdf(a)

