# Source data

data <- subset(treedata, file == "Raw_data_structure20200515_CZ_JH1.xlsx")

#------------------------------------------------------------------------------- site
# Crete the species proportion per site

library(dplyr)

tree_data <- data %>%
    mutate(Site = sub("_[0-9]+", "", plotID),
         Plot = sub("([A-Z]+)[0-9]+_[0-9]+", "\\1\\2", plotID))

head(tree_data)


# Count the number of trees for each species within each site
site_species_counts <- tree_data %>%
  group_by(Site, treesp) %>%
  summarise(TreeCount = n())

# Calculate species proportions within each site
site_species_proportions_n <- site_species_counts %>%
  group_by(Site) %>%
  mutate(SpeciesProportion = TreeCount / sum(TreeCount))

print(site_species_proportions_n)
show(site_species_proportions_n)

# Filter the data for the specific site 'L1'
site_L1_proportions_n <- subset(site_species_proportions_n, Site == 'L1')

# Extract the species proportions for 'L1'
species_proportions_L1_n <- site_L1_proportions_n$SpeciesProportion

# Print the array or string of species proportions
cat("Species Proportions in Site L1:", species_proportions_L1_n, "\n")


#---------------------------------------------------------------------------------- plot
# Do the same but for plot instead that for sites

library(dplyr)

# Define tree_data with Site and Plot
tree_data <- data %>%
  mutate(
    Site = sub("_[0-9]+", "", plotID),  # Extract the site part (e.g., L1, L2)
    Plot = plotID  # Retain the full plotID as Plot
  )

head(tree_data)

# Count the number of trees for each species within each plot
plot_species_counts <- tree_data %>%
  group_by(Site, Plot, treesp) %>%
  summarise(TreeCount = n(), .groups = 'drop')

# Calculate species proportions within each plot
plot_species_proportions_n <- plot_species_counts %>%
  group_by(Site, Plot) %>%
  mutate(SpeciesProportion = TreeCount / sum(TreeCount))

print(plot_species_proportions_n)
show(plot_species_proportions_n)

# Filter the data for the specific site 'L1'
plot_L1_proportions_n <- subset(plot_species_proportions_n, Plot == 'L1_03')

# Extract the species proportions for 'L1'
species_proportions_L1_n <- plot_L1_proportions_n$SpeciesProportion

# Print the array or string of species proportions
cat("Species Proportions in Site L1:", species_proportions_L1_n, "\n")


#------------------------------------------------------------------------------- site
# Proportion per site in terms of volume
library(dplyr)

# Data transformation to create Site and Plot columns
tree_data <- data %>%
  mutate(Site = sub("_[0-9]+", "", plotID),
         Plot = plotID)  # Retain the full plotID as Plot

head(tree_data)

# Ensure treevol is numeric and replace NA with 0
tree_data <- tree_data %>%
  mutate(treevol = as.numeric(treevol),
         treevol = ifelse(is.na(treevol), 0, treevol))

# Summarize the total tree volume for each species within each site
site_species_volumes <- tree_data %>%
  group_by(Site, treesp) %>%
  summarise(TotalVolume = sum(treevol, na.rm = TRUE))

# Calculate species proportions based on total volume within each site
site_species_proportions_vol <- site_species_volumes %>%
  group_by(Site) %>%
  mutate(SpeciesProportion = TotalVolume / sum(TotalVolume))

# Create a % value column
site_species_proportions_vol <- site_species_proportions_vol %>%
  mutate(sp_per=SpeciesProportion*100)

print(site_species_proportions_vol)
show(site_species_proportions_vol)

# Filter the data for the specific site 'L1'
site_L1_proportions_vol <- subset(site_species_proportions_vol, Site == 'L1')

# Extract the species proportions for 'L1'
species_proportions_L1_vol <- site_L1_proportions_vol$SpeciesProportion

# Print the array or string of species proportions
cat("Species Proportions in Site L1:", species_proportions_L1_vol, "\n")
 
#------------------------------------------------------------------------------- plot
# Do the same but for plot instead that for plot relative at the wood volume
library(dplyr)

# Define tree_data with Site and Plot
tree_data <- data %>%
  mutate(
    Site = sub("_[0-9]+", "", plotID),  # Extract the site part (e.g., L1, L2)
    Plot = plotID  # Retain the full plotID as Plot
  )

head(tree_data)

# Ensure treevol is numeric and replace NA with 0
tree_data <- tree_data %>%
  mutate(treevol = as.numeric(treevol),
         treevol = ifelse(is.na(treevol), 0, treevol))

# Summarize the total tree volume for each species within each plot
plot_species_volumes <- tree_data %>%
  group_by(Plot, treesp) %>%
  summarise(TotalVolume = sum(treevol, na.rm = TRUE))

# Calculate species proportions based on total volume within each plot
plot_species_proportions_vol <- plot_species_volumes %>%
  group_by(Plot) %>%
  mutate(SpeciesProportion = TotalVolume / sum(TotalVolume))

print(plot_species_proportions_vol)
show(plot_species_proportions_vol)

# Filter the data for the specific plot 'L1_33'
plot_L1_33_proportions_vol <- subset(plot_species_proportions_vol, Plot == 'L1_27')
plot_L1_33_proportions_vol

# Extract the species proportions for 'L1_33'
species_proportions_L1_33_vol <- plot_L1_33_proportions_vol$SpeciesProportion

# Print the array or string of species proportions
cat("Species Proportions in Plot L1_33:", species_proportions_L1_33_vol, "\n")


#-------------------------------------------------------------------------------
# Create site and plot species proportion in a excel table

library(writexl)

dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/Sources_bottoms_up/Jenik/final_table_imp/species_proportion/"

# sp prop per site based on n. of trees
write_xlsx(site_species_proportions_n, file.path(dataroot, "sp_prop_site_n.xlsx"))

# sp prop per plot based on n. of trees
write_xlsx(plot_species_proportions_n, file.path(dataroot, "sp_prop_plot_n.xlsx"))

# sp prop per site based on volume per species
write_xlsx(site_species_proportions_vol, file.path(dataroot, "sp_prop_site_vol.xlsx"))

# sp prop per plot based on rel vol per species
write_xlsx(plot_species_proportions_vol, file.path(dataroot, "sp_prop_plot_vol.xlsx"))


#-------------------------------------------------------------------------------
# Species proportion visualization

species.we.have <- unique(plot_species_proportions_vol$treesp)


# Color palette for spiecies
cols.all=c( "Robinia pseudoacacia"="#e0e0e0", "Acer platanoides"="#A9A9A9",   "Alnus incana"="#696969", "Alnus vivirdis"="#2e2e2e",
            "Betula pendula"="#fadfad", "Acer campestre"="#FF4600",
            "Pinus strobus"="#7eeadf", "Corilus avellana"="#20c6b6",
            "Sambucus nigra"="#645394", "Ulmus glabra"="#311432" ,
            "Salix caprea"="#D8BFD8",  "Sorbus torminalis"="#DDA0DD", "Sorbus aucuparia"="#BA55D3",
            "Sambucus racemosa"="#D27D2D", "Pinus nigra"="#a81c07",
            "Ulmus minor"="#2ECBE9","Tilia cordata"="#128FC8",  "Populus tremula"="#00468B","Populus alba"="#5BAEB7",
            "Fraxinus excelsior"="#fe9cb5","Carpinus betulus"="#fe6181","Acer pseudoplatanus"="#fe223e",
            "Larix decidua"="#FFFE71","Abies alba"="#FFD800", "Pinus sylvestris"="#A4DE02",
            "Fagus sylvatica"="#76BA1B", "Picea abies"="#006600",
            "Quercus robur"="#FF7F00", "Quercus petraea"="#FF9900", "Quercus rubra"="#CC9900" 
)


new_order_gg.all=c("Robinia pseudoacacia", "Corilus avellana", "Alnus incana", "Alnus vivirdis",
                   "Acer platanoides", "Acer campestre", "Acer pseudoplatanus","Fraxinus excelsior",
                   "Betula pendula", "Carpinus betulus","Ulmus glabra", "Ulmus minor",
                   "Tilia cordata","Sambucus nigra","Sorbus torminalis", "Sorbus aucuparia", "Sambucus racemosa",
                   "Salix caprea","Populus tremula","Populus alba",
                   "Quercus robur", "Quercus petraea", "Quercus rubra",
                   "Pinus strobus","Pinus nigra","Pinus sylvestris",
                   "Larix decidua","Abies alba",
                   "Fagus sylvatica", "Picea abies")


# This will show at the end only the species we really have on the landscape. 

cols<-cols.all[names(cols.all) %in% species.we.have]
new_order_gg<- new_order_gg.all[new_order_gg.all %in% species.we.have]

# Plot
x7wb <- ggplot(site_species_proportions_vol, aes(x="", y=SpeciesProportion, fill=factor(treesp, levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~Site, ncol=3)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(sp_per, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on site volume [m3/ha]")+
  theme_bw()
x7wb + theme(plot.title = element_text(hjust = 0.5))
