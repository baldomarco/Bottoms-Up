library(RSQLite)

# Scenarios browsing probability = 1 with tree age from iLand

file<-"C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/Test_statitician_L1_22.sqlite"   # file to read


sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = file)  # connect to the file
tables.in.the.file<-dbListTables(db)           # explore the tables in the file
print(tables.in.the.file)


#-----------------------------------------------
# READ IN different tables:    

abeStand <- dbReadTable(db, "abeStand")
abeStandDetail <- dbReadTable(db, "abeStandDetail")
abeStandRemoval <- dbReadTable(db, "abeStandRemoval")
abeUnit <- dbReadTable(db, "abeUnit")
barkbeetle <- dbReadTable(db,"barkbeetle")
carbon <- dbReadTable(db,"carbon")
carbonflow <- dbReadTable(db, "carbonflow")
dynamicstand <- dbReadTable(db, "dynamicstand")
landscape <- dbReadTable(db,"landscape")
landscape_removed_1scen <- dbReadTable(db,"landscape_removed")
stand <- dbReadTable(db, "stand")
tree <- dbReadTable(db, "tree")

dbDisconnect(db)    # close the file


# 1st scenarios browsing probability = 1
{
file<-"C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/Test_L1_10_management1_brow1.sqlite"   # file to read


sqlite.driver <- dbDriver("SQLite")
db1 <- dbConnect(sqlite.driver, dbname = file)  # connect to the file
tables.in.the.file<-dbListTables(db1)           # explore the tables in the file
print(tables.in.the.file)


#-----------------------------------------------
# READ IN different tables:    

abeStand <- dbReadTable(db1, "abeStand")
abeStandDetail <- dbReadTable(db1, "abeStandDetail")
abeStandRemoval <- dbReadTable(db1, "abeStandRemoval")
abeUnit <- dbReadTable(db1, "abeUnit")
barkbeetle <- dbReadTable(db1,"barkbeetle")
carbon <- dbReadTable(db1,"carbon")
carbonflow <- dbReadTable(db1, "carbonflow")
dynamicstand_1scen <- dbReadTable(db1, "dynamicstand")
landscape_1scen <- dbReadTable(db1,"landscape")
landscape_removed_1scen <- dbReadTable(db1,"landscape_removed")
stand <- dbReadTable(db1, "stand")
tree <- dbReadTable(db1, "tree")

dbDisconnect(db1)    # close the file

#-------------------------------------------------------------------------------
# 2nd scenarios browsing probability = 0

file<-"C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/Test_L1_10_management1_brow0.sqlite"   # file to read


sqlite.driver <- dbDriver("SQLite")
db2 <- dbConnect(sqlite.driver, dbname = file)  # connect to the file
tables.in.the.file<-dbListTables(db2)           # explore the tables in the file
print(tables.in.the.file)


#-----------------------------------------------
# READ IN different tables:    

abeStand <- dbReadTable(db2, "abeStand")
abeStandDetail <- dbReadTable(db2, "abeStandDetail")
abeStandRemoval <- dbReadTable(db2, "abeStandRemoval")
abeUnit <- dbReadTable(db2, "abeUnit")
barkbeetle <- dbReadTable(db2,"barkbeetle")
carbon <- dbReadTable(db2,"carbon")
carbonflow <- dbReadTable(db2, "carbonflow")
dynamicstand_2scen <- dbReadTable(db2, "dynamicstand")
landscape_2scen <- dbReadTable(db2,"landscape")
landscape_removed_2scen <- dbReadTable(db2,"landscape_removed")
stand <- dbReadTable(db2, "stand")
tree <- dbReadTable(db2, "tree")

dbDisconnect(db2)    # close the file

#-------------------------------------------------------------------------------
# 3rd scenarios browsing probability = 0.3


file<-"C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/Test_L1_10_management1_brow0.3.sqlite"   # file to read


sqlite.driver <- dbDriver("SQLite")
db3 <- dbConnect(sqlite.driver, dbname = file)  # connect to the file
tables.in.the.file<-dbListTables(db3)           # explore the tables in the file
print(tables.in.the.file)


#-----------------------------------------------
# READ IN different tables:    

abeStand <- dbReadTable(db3, "abeStand")
abeStandDetail <- dbReadTable(db3, "abeStandDetail")
abeStandRemoval <- dbReadTable(db3, "abeStandRemoval")
abeUnit <- dbReadTable(db3, "abeUnit")
barkbeetle <- dbReadTable(db3,"barkbeetle")
carbon <- dbReadTable(db3,"carbon")
carbonflow <- dbReadTable(db3, "carbonflow")
dynamicstand_3scen <- dbReadTable(db3, "dynamicstand")
landscape_3scen <- dbReadTable(db3,"landscape")
landscape_removed_3scen <- dbReadTable(db3,"landscape_removed")
stand <- dbReadTable(db3, "stand")
tree <- dbReadTable(db3, "tree")

dbDisconnect(db3)    # close the file

#-------------------------------------------------------------------------------
# 4th scenarios browsing probability = 0.6

file<-"C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/Test_L1_10_management1_brow0.6.sqlite"   # file to read


sqlite.driver <- dbDriver("SQLite")
db4 <- dbConnect(sqlite.driver, dbname = file)  # connect to the file
tables.in.the.file<-dbListTables(db4)           # explore the tables in the file
print(tables.in.the.file)


#-----------------------------------------------
# READ IN different tables:    

abeStand <- dbReadTable(db4, "abeStand")
abeStandDetail <- dbReadTable(db4, "abeStandDetail")
abeStandRemoval <- dbReadTable(db4, "abeStandRemoval")
abeUnit <- dbReadTable(db4, "abeUnit")
barkbeetle <- dbReadTable(db4,"barkbeetle")
carbon <- dbReadTable(db4,"carbon")
carbonflow <- dbReadTable(db4, "carbonflow")
dynamicstand_4scen <- dbReadTable(db4, "dynamicstand")
landscape_4scen <- dbReadTable(db4,"landscape")
landscape_removed_4scen <- dbReadTable(db4,"landscape_removed")
stand <- dbReadTable(db4, "stand")
tree <- dbReadTable(db4, "tree")

dbDisconnect(db4)    # close the file
}

{
landscape_1scen_300 <- landscape_1scen %>% filter(year <= 300) # Other way df_subset <- df[df$Year <= 300, ]
landscape_2scen_300 <- landscape_2scen %>% filter(year <= 300)
landscape_3scen_300 <- landscape_3scen %>% filter(year <= 300)
landscape_4scen_300 <- landscape_4scen %>% filter(year <= 300)
}


# Start with plots

#_______________________________________________
library(ggplot2)
library(gridExtra) # To arrange the graphs in a grid

# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/outputs/20231106/"
pdf(paste0(dataroot, "20231107_mng_plot_L1_10_300.pdf"), height=8, width=12)

# SET THE COLORS
# this tells the colors:
species.we.have<-unique(landscape$species)

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

new_order_gg.all=c("alvi","alin", "acpl", "rops","bepe" ,"coav", "casa", "ulgl", "tipl",  "soau", "soar", "saca",  "pini", "pice",
                   "poni", "algl", "tico", "potr",  "frex","cabe", "acps",  "lade", "abal",  "qupu", "qupe","quro","pisy", "fasy", "piab")


# This will show at the end only the species we really have on the landscape. 

cols<-cols.all[names(cols.all) %in% species.we.have]
new_order_gg<- new_order_gg.all[new_order_gg.all %in% species.we.have]

#_____________________________________________________________________________________
# Plot the wood volume (m3/h) trajectory in plot L1_22 un managed color by species

# Volume 1st scenario browsing 1
A1 <- ggplot(landscape_1scen_300, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Wood standing volume - Brow 1") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,1150)


# Volume 2nd scenario browsing 0
A2 <- ggplot(landscape_2scen_300, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Wood standing volume - Brow 0") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,1150)

# Volume 3rd scenario browsing 0.3
A3 <- ggplot(landscape_3scen_300, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Wood standing volume - Brow 0.3") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,1150)


# Volume 4th scenario browsing 0.6
A4 <- ggplot(landscape_4scen_300, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Wood standing volume - Brow 0.6") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,1150)

# Plot together
grid.arrange(A1,A2,A3,A4,ncol=2)


# Natural mortality 1st scenario browsing 1
nm1 <- ggplot(landscape_removed_1scen, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Natural Mortality volume - Brow 1") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,50)

# Natural mortality 2nd scenario browsing 0
nm2 <- ggplot(landscape_removed_2scen, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Natural Mortality volume - Brow 0") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,50)

# Natural mortality 3rd scenario browsing 0.3
nm3 <- ggplot(landscape_removed_3scen, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Natural Mortality volume - Brow 0.3") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,50)

# Natural mortality 4th scenario browsing 0.6
nm4 <- ggplot(landscape_removed_4scen, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Natural Mortality volume - Brow 0.6") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,50)

# Plot together
grid.arrange(nm1,nm2,nm3,nm4,ncol=2)


# Average tree age in the plot per 1000 years time serie

{
# Age 1st scenario browsing 1 age from iLand function

age <- ggplot(dynamicstand, aes(x=year, y=age_mean))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Avarage Tree Age [iLand age fun]")+
  labs(x = "Year",y="Age [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  ylim(0,500)

# Age 1st scenario browsing 1 age from plot age 191

age1 <- ggplot(dynamicstand_1scen, aes(x=year, y=age_mean))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Avarage Tree Age [Brow 1]")+
  labs(x = "Year",y="Age [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  ylim(0,200)

# Age 2nd scenario browsing 0 age from plot age 191

age2 <- ggplot(dynamicstand_2scen, aes(x=year, y=age_mean))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Avarage Tree Age [Brow 0]")+
  labs(x = "Year",y="Age [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  ylim(0,200)

# Age 3rd scenario browsing 0.3 age from plot age 191

age3 <- ggplot(dynamicstand_2scen, aes(x=year, y=age_mean))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Avarage Tree Age [Brow 0.3]")+
  labs(x = "Year",y="Age [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  ylim(0,200)

# Age 4th scenario browsing 0.6 age from plot age 191

age4 <- ggplot(dynamicstand_2scen, aes(x=year, y=age_mean))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Avarage Tree Age [Brow 0.6]")+
  labs(x = "Year",y="Age [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  ylim(0,200)

grid.arrange(age1, age2,age3, age4, ncol=2)
}

# Max tree age in the plot 1000 year time serie

{# Age 1st scenario browsing 1 age from iLand function

age <- ggplot(dynamicstand, aes(x=year, y=age_max))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Max Tree Age [iLand age fun]")+
  labs(x = "Year",y="Age [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  ylim(0,1150)

# Age 1st scenario browsing 1 age from plot age 191

age1 <- ggplot(dynamicstand_1scen, aes(x=year, y=age_max))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Max Tree Age [Brow 1]")+
  labs(x = "Year",y="Age [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  ylim(0,700)

# Age 2nd scenario browsing 0 age from plot age 191

age2 <- ggplot(dynamicstand_2scen, aes(x=year, y=age_max))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Max Tree Age [Brow 0]")+
  labs(x = "Year",y="Age [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  ylim(0,700)

# Age 3rd scenario browsing 0.3 age from plot age 191

age3 <- ggplot(dynamicstand_2scen, aes(x=year, y=age_max))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Max Tree Age [Brow 0.3]")+
  labs(x = "Year",y="Age [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  ylim(0,700)

# Age 4th scenario browsing 0.6 age from plot age 191

age4 <- ggplot(dynamicstand_2scen, aes(x=year, y=age_max))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Max Tree Age [Brow 0.6]")+
  labs(x = "Year",y="Age [years]")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  ylim(0,700)

grid.arrange(age1, age2, age3, age4, ncol=2)

}



# Make a plot with ggplot Basal Area by species (in landscape output), for the transitional period

ggplot(landscape, aes(year,basal_area_m2, fill=species))+
  geom_area() +
  ggtitle("Clear Cut Basal Area Transitional Period by species")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=landscape, aes(x=year, y=basal_area_m2, color=species)) + 
  geom_line() +
  ggtitle("Basal Area per species time series in Clear cut")+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data=landscape, aes(x=year, y=total_carbon_kg, color=species)) + 
  geom_line()+
  ggtitle("Total Carbon per species time series in Shalterwood") +
  theme(plot.title = element_text(hjust = 0.5))




#_____________________________________________________________
# PLOT THE TANSITION PERIOD (TP) VOLUME BY SPECIE IN COMPARABLE WAY (MULTI-WINDOW)
# TP IN WIND AND BARK BEETLE REGIME 
#_____________________________________________
g1 <- ggplot(landscape_cc_brow0, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Clearcut management in browsing pressure 0") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,400)

g2 <- ggplot(landscape_sw_brow0, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Shelterwood management in browsing pressure 0") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,400)

#grid.arrange(g1,g2,ncol=2)
grid.arrange(g1,g2,ncol=1)

#______________________________________________
# TP in brow0.3

g3 <- ggplot(landscape_cc_brow0.3, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Clearcut management in browsing pressure 0.3") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,400)

g4 <- ggplot(landscape_sw_brow0.3, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Shelterwood management in browsing pressure 0.3") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,400)

#grid.arrange(g1,g2,ncol=2)
grid.arrange(g3,g4,ncol=1)

#______________________________________________
# All the 4 graphics together

grid.arrange(g1,g2,g3,g4,ncol=1)

# PLOT THE TANSITION PERIOD (TP) VOLUME BY SPECIE IN COMPARABLE WAY (MULTI-WINDOW)
# TP IN brow0 and brow0.8 
#_____________________________________________
g5 <- ggplot(landscape_cc_brow0.8, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Clearcut management in browsing pressure 0.8") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,400)

g6 <- ggplot(landscape_sw_brow0.8, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Shelterwood management in browsing pressure 0.8") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,400)

#grid.arrange(g1,g2,ncol=2)
grid.arrange(g5,g6,ncol=1)

# PLOT THE TANSITION PERIOD (TP) VOLUME BY SPECIE IN COMPARABLE WAY (MULTI-WINDOW)
# TP IN brow0 and brow1.2 
#_____________________________________________
g7 <- ggplot(landscape_cc_brow1.2, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Clearcut management in browsing pressure 1.2") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,400)

g8 <- ggplot(landscape_sw_brow1.2, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(title = "Species", reverse=TRUE))+
  ggtitle("Shelterwood management in browsing pressure 1.2") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22)) +
  ylab("Volume [m3/ha]") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) +
  xlab("Year") + theme(axis.title.x = element_text(size = rel(1.8), angle = 00)) +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,400)

#grid.arrange(g1,g2,ncol=2)
grid.arrange(g7,g8,ncol=1)


# ALL 8 TOGETHER
grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,ncol=2)

#______________________________________________
# HARVEST TIME SERIES IN browsing 0 DISTURBANCE REGIME
#----------------------------------------------

a1<-data.frame(year=abeUnit_cc_brow0$year, harvest=abeUnit_cc_brow0$realizedHarvest, case="Clearcut brow 0")
a2<-data.frame(year=abeUnit_sw_brow0$year, harvest=abeUnit_sw_brow0$realizedHarvest, case="Shelterwood brow 0")

head(a1)
head(a2)

harvests<- rbind(a1,a2)
summary(a1)
summary(a2)
summary(harvests)
dim(harvests)

x1 <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=1.2)+
  ggtitle("Realized transitional period total harvest in browsing 0")+
  #theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,15)+
  theme_bw()
x1 + theme(plot.title = element_text(hjust = 0.5))

x2 <-ggplot(harvests, aes(year,harvest))+
  geom_line(size=1.2)+
  facet_wrap(~case, ncol=1)+
  ggtitle("Realized transitional period total harvest in browsing 0")+
  #theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,25)+
  theme_bw()
x2 + theme(plot.title = element_text(hjust = 0.5))

#_______________________________________________________________________________
# Realized harvest transition period in browsing 0.3

a3<-data.frame(year=abeUnit_cc_brow0.3$year, harvest=abeUnit_cc_brow0.3$realizedHarvest, case="Clearcut brow 0.3")
a4<-data.frame(year=abeUnit_sw_brow0.3$year, harvest=abeUnit_sw_brow0.3$realizedHarvest, case="Shelterwood brow 0.3")

head(a3)
head(a4)

harvests<- rbind(a3,a4)
summary(a3)
summary(a4)
summary(harvests)
dim(harvests)

x3 <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=1.2)+
  ggtitle("Realized transitional period total harvest in browsing 0.3")+
  #theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,25)+
  theme_bw()
x3 + theme(plot.title = element_text(hjust = 0.5))

x4 <-ggplot(harvests, aes(year,harvest))+
  geom_line(size=1.2)+
  facet_wrap(~case, ncol=1)+
  ggtitle("Realized transitional period total harvest in browsing 0.3")+
  #theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,25)+
  theme_bw()
x4 + theme(plot.title = element_text(hjust = 0.5))

#_______________________________________________________________________________
# IN BROWSING 0.8

# HARVEST TIME SERIES IN DISTURBANCE REGIME
#----------------------------------------------

a5<-data.frame(year=abeUnit_cc_brow0.8$year, harvest=abeUnit_cc_brow0.8$realizedHarvest, case="Clearcut brow 0.8")
a6<-data.frame(year=abeUnit_sw_brow0.8$year, harvest=abeUnit_sw_brow0.8$realizedHarvest, case="Shelterwood brow 0.8")

head(a5)
head(a6)

harvests<- rbind(a5,a6)
summary(a5)
summary(a6)
summary(harvests)
dim(harvests)

x5 <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=1.2)+
  ggtitle("Realized transitional period total harvest in browsing 0.8")+
  #theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,25)+
  theme_bw()
x5 + theme(plot.title = element_text(hjust = 0.5))

x6 <-ggplot(harvests, aes(year,harvest))+
  geom_line(size=1.2)+
  facet_wrap(~case, ncol=1)+
  ggtitle("Realized transitional period total harvest in browsing 0.8")+
  #theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,25)+
  theme_bw()
x6 + theme(plot.title = element_text(hjust = 0.5))

#_______________________________________________________________________________
# IN BROWSING 1.2

# HARVEST TIME SERIES IN DISTURBANCE REGIME
#----------------------------------------------

a7<-data.frame(year=abeUnit_cc_brow1.2$year, harvest=abeUnit_cc_brow1.2$realizedHarvest, case="Clearcut brow 1.2")
a8<-data.frame(year=abeUnit_sw_brow1.2$year, harvest=abeUnit_sw_brow1.2$realizedHarvest, case="Shelterwood brow 1.2")

head(a7)
head(a8)

harvests<- rbind(a7,a8)
summary(a7)
summary(a8)
summary(harvests)
dim(harvests)

x7 <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=1.2)+
  ggtitle("Realized transitional period total harvest in browsing 1.2")+
  #theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,25)+
  theme_bw()
x7 + theme(plot.title = element_text(hjust = 0.5))

x8 <-ggplot(harvests, aes(year,harvest))+
  geom_line(size=1.2)+
  facet_wrap(~case, ncol=1)+
  ggtitle("Realized transitional period total harvest in browsing 1.2")+
  #theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,25)+
  theme_bw()
x8 + theme(plot.title = element_text(hjust = 0.5))

#________________________________________________________________________
# ALL 8 TOGETHER

harvests<- rbind(a1,a2,a3,a4,a5,a6,a7,a8)

x7 <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=0.8)+
  ggtitle("Realized harvest transitional period")+
  #theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  ylim(0,20)+
  theme_bw()
x7 + theme(plot.title = element_text(hjust = 0.5))


x8 <-ggplot(harvests, aes(year,harvest))+
  geom_line(size=1.2)+
  facet_wrap(~case, ncol=1)+
  ggtitle("Realized harvest transitional period")+
  #theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  ylim(0,20)+
  theme_bw()
x8 + theme(plot.title = element_text(hjust = 0.5))

#_______________________________________________________________________________
# CUMULATIVE HARVEST IN BROWSING 0

cs1 <-data.frame(year=abeUnit_cc_brow0$year, harvest=cumsum(abeUnit_cc_brow0$realizedHarvest), case="clearcut with brow0")
cs2 <-data.frame(year=abeUnit_sw_brow0$year, harvest=cumsum(abeUnit_sw_brow0$realizedHarvest), case="Shelterwood with brow0")

harvests <- rbind(cs1,cs2)
#head(h1)

summary(cs1)  # statistics
summary(cs2)
#dim(h1)      # dimension of the dataframe

#_________________________________________________________________________________
# LINE

cumHarv <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=1.2)+
  ggtitle("Realized Cumulative Harvest brow0")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest [m3/ha]")+
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
cumHarv

# Plot the realized harvest
# If you want it in logarithmic form -> log(h1) or log10(h1)

# LOGARITHMIC
cumHarv <- ggplot(harvests, aes(year,log(harvest), color=case))+
  geom_line(size=1.2)+
  ggtitle("Log Realized Cumulative Harvest brow0")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
cumHarv


#-----------------------------------------
# with browsing 0.3

cs3<-data.frame(year=abeUnit_cc_brow0.3$year, harvest=cumsum(abeUnit_cc_brow0.3$realizedHarvest), case="clearcut with brow0.3")
cs4<-data.frame(year=abeUnit_sw_brow0.3$year, harvest=cumsum(abeUnit_sw_brow0.3$realizedHarvest), case="Shelterwood with brow0.3")

harvests <- rbind(cs3,cs4)
#head(h1)

summary(cs3)  # statistics
summary(cs4)
#dim(h1)      # dimension of the dataframe

#_________________________________________________________________________________
# LINE

cumHarv <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=1.2)+
  ggtitle("Realized Cumulative Harvest brow0.3")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
cumHarv

# Plot the realized harvest
# If you want it in logarithmic form -> log(h1) or log10(h1)

# LOGARITHMIC
cumHarv <- ggplot(harvests, aes(year,log(harvest), color=case))+
  geom_line(size=1.2)+
  ggtitle("Log Realized Cumulative Harvest brow0.3")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
cumHarv

#___________________________________________________________________________
#-----------------------------------------
# With browsing 0. 8

cs5<-data.frame(year=abeUnit_cc_brow0.8$year, harvest=cumsum(abeUnit_cc_brow0.8$realizedHarvest), case="clearcut with brow0.8")
cs6<-data.frame(year=abeUnit_sw_brow0.8$year, harvest=cumsum(abeUnit_sw_brow0.8$realizedHarvest), case="Shelterwood with brow0.8")

harvests <- rbind(cs5,cs6)
#head(h1)

summary(cs5)  # statistics
summary(cs6)
#dim(h1)      # dimension of the dataframe

#_________________________________________________________________________________
# LINE

cumHarv <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=1.2)+
  ggtitle("Realized Cumulative Harvest brow0.8")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
cumHarv

# Plot the realized harvest
# If you want it in logarithmic form -> log(h1) or log10(h1)

# LOGARITHMIC
cumHarv <- ggplot(harvests, aes(year,log(harvest), color=case))+
  geom_line(size=1.2)+
  ggtitle("Log Realized Cumulative Harvest brow0.8")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
cumHarv

#______________________________________________________
# With browsing 1.2

cs7<-data.frame(year=abeUnit_cc_brow1.2$year, harvest=cumsum(abeUnit_cc_brow1.2$realizedHarvest), case="clearcut with brow1.2")
cs8<-data.frame(year=abeUnit_sw_brow1.2$year, harvest=cumsum(abeUnit_sw_brow1.2$realizedHarvest), case="Shelterwood with brow1.2")

harvests <- rbind(cs7,cs8)
#head(h1)

summary(cs7)  # statistics
summary(cs8)
#dim(h1)      # dimension of the dataframe

#_________________________________________________________________________________
# LINE

cumHarv <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=1.2)+
  ggtitle("Realized Cumulative Harvest brow1.2")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
cumHarv

# Plot the realized harvest
# If you want it in logarithmic form -> log(h1) or log10(h1)

# LOGARITHMIC
cumHarv <- ggplot(harvests, aes(year,log(harvest), color=case))+
  geom_line(size=1.2)+
  ggtitle("Log Realized Cumulative Harvest brow1.2")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()

cumHarv <- cumHarv + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=22))
cumHarv <- cumHarv + theme(plot.title = element_text(hjust = 0.5))
cumHarv <- cumHarv + theme(axis.title.y = element_text(size = rel(1.8), angle = 90))
cumHarv <- cumHarv + theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
cumHarv
#___________________________________________________________________________



-
  -
  -
  -
  -
  -
  -
  -
  #__________________________________________________________________________
  # Yearly increment on the landscape 
  
  
  inc1<-data.frame(year=landscape_cc_brow0$year, volume_inc=abeUnit_cc_brow0$realizedHarvest, case="Clearcut_brow0")
inc2<-data.frame(year=landscape_sw_brow0$year, volume_inc=abeUnit_sw_brow0$realizedHarvest, case="Shelterwood_brow0")

harvests<- rbind(a1,a2)

x1 <- ggplot(harvests, aes(year,harvest, color=case))+
  geom_line(size=1.2)+
  ggtitle("Realized transitional period total harvest in wind and bark beetle regime")+
  #theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,15)+
  theme_bw()
x1 + theme(plot.title = element_text(hjust = 0.5))

x2 <-ggplot(harvests, aes(year,harvest))+
  geom_line(size=1.2)+
  facet_wrap(~case, ncol=1)+
  ggtitle("Realized transitional period total harvest in wind and bark beetle regime")+
  #theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest [m3/ha]")+
  ylim(0,25)+
  theme_bw()
x2 + theme(plot.title = element_text(hjust = 0.5))

  
  
  #_______________________________________________________________________________
  # PIE CHARTS IN browsing 0 pressure VOLUME BY SPECIES PROPORTION
  
  #-----------------------------------------

landscape_0 <- landscape %>% filter(year==0)
landscape_100 <- landscape %>% filter(year==100)
landscape_500 <- landscape %>% filter(year==500)
landscape_200 <- landscape %>% filter(year==200)
landscape_700 <- landscape %>% filter(year==700)
landscape_300 <- landscape %>% filter(year==300)
landscape_800 <- landscape %>% filter(year==800)
landscape_400 <- landscape %>% filter(year==400)
landscape_1000 <- landscape %>% filter(year==1000)

b0_brow0<-landscape_0 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Initial stage")
b1_brow0<-landscape_100 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Plot L1_22 year 100")
b2_brow0<-landscape_200 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Plot L1_22 year 200")
b3_brow0<-landscape_300 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Plot L1_22 year 300")
b4_brow0<-landscape_400 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Plot L1_22 year 400")
b5_brow0<-landscape_500 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Plot L1_22 year 500")
b6_brow0<-landscape_700 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Plot L1_22 year 700")
b7_brow0<-landscape_800 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Plot L1_22 year 800")
b8_brow0<-landscape_1000 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Plot L1_22 year Final Stage")

#summary(b1)
#sum(b1$perc.vol)

# SELETION OF THE INITIAL AND FINAL STAGE

r1_brow0<-rbind(b0_brow0,b7_brow0,b8_brow0)

x7_brow0 <- ggplot(r1_brow0, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~case, ncol=3)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha] in brow pressure 0")+
  theme_bw()
x7_brow0 + theme(plot.title = element_text(hjust = 0.5))

# PLOT ALL THE STAGE FOR EVERY 100 YEARS

r_transition_brow0 <-rbind(b0_brow0,b1_brow0,b2_brow0,b3_brow0,b4_brow0,b5_brow0,b6_brow0,b7_brow0,b8_brow0)

x8_brow0 <- ggplot(r_transition_brow0, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~case, ncol=5)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha] in Plot L1_22")+
  theme_bw()
x8_brow0 + theme(plot.title = element_text(hjust = 0.5))


#_______________________________________________________________________________
# PIE CHARTS IN in browsing pressure 0.3 VOLUME BY SPECIES PROPORTION

#-----------------------------------------
landscape_cc_brow0.3_0 <- landscape_cc_brow0.3 %>% filter(year==0)
landscape_cc_brow0.3_100 <- landscape_cc_brow0.3 %>% filter(year==100)
landscape_sw_brow0.3_100 <- landscape_sw_brow0.3 %>% filter(year==100)
landscape_cc_brow0.3_200 <- landscape_cc_brow0.3 %>% filter(year==200)
landscape_sw_brow0.3_200 <- landscape_sw_brow0.3 %>% filter(year==200)
landscape_cc_brow0.3_300 <- landscape_cc_brow0.3 %>% filter(year==300)
landscape_sw_brow0.3_300 <- landscape_sw_brow0.3 %>% filter(year==300)
landscape_cc_brow0.3_400 <- landscape_cc_brow0.3 %>% filter(year==399)
landscape_sw_brow0.3_400 <- landscape_sw_brow0.3 %>% filter(year==399)

b0_brow0.3<-landscape_cc_brow0.3_0 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Initial_stage")
b1_brow0.3<-landscape_cc_brow0.3_100 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Clearcut year 100")
b2_brow0.3<-landscape_sw_brow0.3_100 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Shelterwood year 100")
b3_brow0.3<-landscape_cc_brow0.3_200 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Clearcut year 200")
b4_brow0.3<-landscape_sw_brow0.3_200 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Shelterwood year 200")
b5_brow0.3<-landscape_cc_brow0.3_300 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Clearcut year 300")
b6_brow0.3<-landscape_sw_brow0.3_300 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Shelterwood year 300")
b7_brow0.3<-landscape_cc_brow0.3_400 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Clearcut year 400")
b8_brow0.3<-landscape_sw_brow0.3_400 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Shelterwood year 400")

#summary(b1)
#sum(b1$perc.vol)


# Only Wind Regime
# SELETION OF THE INITIAL AND FINAL STAGE

r1_brow0.3<-rbind(b0_brow0.3,b7_brow0.3,b8_brow0.3)

x7_brow0.3 <- ggplot(r1_brow0.3, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~case, ncol=3)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha] in brow pressure 0.3")+
  theme_bw()
x7_brow0.3 + theme(plot.title = element_text(hjust = 0.5))

# PLOT ALL THE STAGE FOR EVERY 100 YEARS

r_transition_brow0.3<-rbind(b0_brow0.3,b1_brow0.3,b2_brow0.3,b3_brow0.3,b4_brow0.3,b5_brow0.3,b6_brow0.3,b7_brow0.3,b8_brow0.3)

x8_brow0.3 <- ggplot(r_transition_brow0.3, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~case, ncol=5)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha] in brow pressure 0.3")+
  theme_bw()
x8_brow0.3 + theme(plot.title = element_text(hjust = 0.5))

#_______________________________________________________________________________
# PIE CHARTS in browsing pressure 0.8 VOLUME BY SPECIES PROPORTION

#-----------------------------------------
landscape_cc_brow0.8_0 <- landscape_cc_brow0.8 %>% filter(year==0)
landscape_cc_brow0.8_100 <- landscape_cc_brow0.8 %>% filter(year==100)
landscape_sw_brow0.8_100 <- landscape_sw_brow0.8 %>% filter(year==100)
landscape_cc_brow0.8_200 <- landscape_cc_brow0.8 %>% filter(year==200)
landscape_sw_brow0.8_200 <- landscape_sw_brow0.8 %>% filter(year==200)
landscape_cc_brow0.8_300 <- landscape_cc_brow0.8 %>% filter(year==300)
landscape_sw_brow0.8_300 <- landscape_sw_brow0.8 %>% filter(year==300)
landscape_cc_brow0.8_400 <- landscape_cc_brow0.8 %>% filter(year==399)
landscape_sw_brow0.8_400 <- landscape_sw_brow0.8 %>% filter(year==399)

b0_brow0.8<-landscape_cc_brow0.8_0 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Initial_stage")
b1_brow0.8<-landscape_cc_brow0.8_100 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Clearcut year 100")
b2_brow0.8<-landscape_sw_brow0.8_100 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Shelterwood year 100")
b3_brow0.8<-landscape_cc_brow0.8_200 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Clearcut year 200")
b4_brow0.8<-landscape_sw_brow0.8_200 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Shelterwood year 200")
b5_brow0.8<-landscape_cc_brow0.8_300 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Clearcut year 300")
b6_brow0.8<-landscape_sw_brow0.8_300 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Shelterwood year 300")
b7_brow0.8<-landscape_cc_brow0.8_400 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Clearcut year 400")
b8_brow0.8<-landscape_sw_brow0.8_400 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Shelterwood year 400")

#summary(b1)
#sum(b1$perc.vol)


# Browsing 0.8
# SELETION OF THE INITIAL AND FINAL STAGE

r1_brow0.8<-rbind(b0_brow0.8,b7_brow0.8,b8_brow0.8)

x7_brow0.8 <- ggplot(r1_brow0.8, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~case, ncol=3)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha] in brow pressure 0.8")+
  theme_bw()
x7_brow0.8 + theme(plot.title = element_text(hjust = 0.5))

# PLOT ALL THE STAGE FOR EVERY 100 YEARS

r_transition_brow0.8<-rbind(b0_brow0.8,b1_brow0.8,b2_brow0.8,b3_brow0.8,b4_brow0.8,b5_brow0.8,b6_brow0.8,b7_brow0.8,b8_brow0.8)

x8_brow0.8 <- ggplot(r_transition_brow0.8, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~case, ncol=5)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha] in brow pressure 0.8")+
  theme_bw()
x8_brow0.8 + theme(plot.title = element_text(hjust = 0.5))


#_______________________________________________________________________________
# PIE CHARTS in browsing pressure 1.2 VOLUME BY SPECIES PROPORTION

#-----------------------------------------
landscape_cc_brow1.2_0 <- landscape_cc_brow1.2 %>% filter(year==0)
landscape_cc_brow1.2_100 <- landscape_cc_brow1.2 %>% filter(year==100)
landscape_sw_brow1.2_100 <- landscape_sw_brow1.2 %>% filter(year==100)
landscape_cc_brow1.2_200 <- landscape_cc_brow1.2 %>% filter(year==200)
landscape_sw_brow1.2_200 <- landscape_sw_brow1.2 %>% filter(year==200)
landscape_cc_brow1.2_300 <- landscape_cc_brow1.2 %>% filter(year==300)
landscape_sw_brow1.2_300 <- landscape_sw_brow1.2 %>% filter(year==300)
landscape_cc_brow1.2_400 <- landscape_cc_brow1.2 %>% filter(year==399)
landscape_sw_brow1.2_400 <- landscape_sw_brow1.2 %>% filter(year==399)

b0_brow1.2<-landscape_cc_brow1.2_0 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Initial_stage")
b1_brow1.2<-landscape_cc_brow1.2_100 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Clearcut year 100")
b2_brow1.2<-landscape_sw_brow1.2_100 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Shelterwood year 100")
b3_brow1.2<-landscape_cc_brow1.2_200 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Clearcut year 200")
b4_brow1.2<-landscape_sw_brow1.2_200 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Shelterwood year 200")
b5_brow1.2<-landscape_cc_brow1.2_300 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Clearcut year 300")
b6_brow1.2<-landscape_sw_brow1.2_300 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Shelterwood year 300")
b7_brow1.2<-landscape_cc_brow1.2_400 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Clearcut year 400")
b8_brow1.2<-landscape_sw_brow1.2_400 %>% mutate( sumvol=sum(volume_m3)) %>% mutate(perc.vol=100*volume_m3/sumvol, case="Shelterwood year 400")

#summary(b1)
#sum(b1$perc.vol)


# Browsing 1.2
# SELETION OF THE INITIAL AND FINAL STAGE

r1_brow1.2<-rbind(b0_brow1.2,b7_brow1.2,b8_brow1.2)

x7_brow1.2 <- ggplot(r1_brow1.2, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~case, ncol=3)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha] in brow pressure 1.2")+
  theme_bw()
x7_brow1.2 + theme(plot.title = element_text(hjust = 0.5))

# PLOT ALL THE STAGE FOR EVERY 100 YEARS

r_transition_brow1.2<-rbind(b0_brow1.2,b1_brow1.2,b2_brow1.2,b3_brow1.2,b4_brow1.2,b5_brow1.2,b6_brow1.2,b7_brow1.2,b8_brow1.2)

x8_brow1.2 <- ggplot(r_transition_brow1.2, aes(x="", y=volume_m3, fill=factor(species,levels=new_order_gg))) +
  geom_bar(stat="identity", width=1, show.legend = T) +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  facet_wrap(~case, ncol=5)+
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0( round(perc.vol, 1)  )),  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+
  ggtitle("Species proportions [%] based on landscape volume [m3/ha] in brow pressure 1.2")+
  theme_bw()
x8_brow1.2 + theme(plot.title = element_text(hjust = 0.5))

#_______________________________________________________________________________
# Understanding the Basal Area dynamic for selected species
# SPECIES specific BA:

# Browsing 0 REGIME

species.to.keep<-c("piab","pisy", "fasy","qupe")
species.to.keep

landscape_cc_brow0_ba <- landscape_cc_brow0 %>% filter(species %in% species.to.keep)
landscape_sw_brow0_ba <- landscape_sw_brow0 %>% filter(species %in% species.to.keep)
#head(landscape_cc2)

#  Example how manually color lines in ggplot2
#  ggplot(dat.m, aes(wave, value, colour = variable)) + geom_line() + 
#  scale_colour_manual(values = c('pink','orange','white'))

ba1_brow0 <-ggplot(data=landscape_cc_brow0_ba, aes(x=year, y=basal_area_m2, colour=species)) + 
  geom_line(size=1.2)+
  scale_colour_manual(values = c("#76BA1B","#006600", "#A4DE02", "orange"))+
  ggtitle("Clearcut management in brow pressure 0") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area [m2/ha]")+
  theme_bw()

ba2_brow0 <-ggplot(data=landscape_sw_brow0_ba, aes(x=year, y=basal_area_m2, colour=species)) + 
  geom_line(size=1.2) +
  scale_colour_manual(values = c("#76BA1B","#006600", "#A4DE02", "orange"))+
  ggtitle("Shelterwood management in brow pressure 0")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area [m2/ha]")+
  theme_bw()

grid.arrange(ba1_brow0,ba2_brow0,ncol=1)

#_______________________________________________________________________________
# IN BROWSING PRESSURE 0.3

landscape_cc_brow0.3_ba <- landscape_cc_brow0.3 %>% filter(species %in% species.to.keep)
landscape_sw_brow0.3_ba <- landscape_sw_brow0.3 %>% filter(species %in% species.to.keep)
#head(landscape_cc2)

#  Example how manually color lines in ggplot2
#  ggplot(dat.m, aes(wave, value, colour = variable)) + geom_line() + 
#  scale_colour_manual(values = c('pink','orange','white'))

ba3_brow0.3 <-ggplot(data=landscape_cc_brow0.3_ba, aes(x=year, y=basal_area_m2, colour=species)) + 
  geom_line(size=1.2)+
  scale_colour_manual(values = c("#76BA1B","#006600", "#A4DE02", "orange"))+
  ggtitle("Clearcut management in brow pressure 0.3") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area [m2/ha]")+
  theme_bw()

ba4_brow0.3 <-ggplot(data=landscape_sw_brow0.3_ba, aes(x=year, y=basal_area_m2, colour=species)) + 
  geom_line(size=1.2) +
  scale_colour_manual(values = c("#76BA1B","#006600", "#A4DE02", "orange"))+
  ggtitle("Shelterwood management in brow pressure 0.3")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area [m2/ha]")+
  theme_bw()

grid.arrange(ba3_brow0.3,ba4_brow0.3,ncol=1)
#_________________________________________________________________________________
# IN NOT in brow pressure 0.8

landscape_cc_brow0.8_ba <- landscape_cc_brow0.8 %>% filter(species %in% species.to.keep)
landscape_sw_brow0.8_ba <- landscape_sw_brow0.8 %>% filter(species %in% species.to.keep)
#head(landscape_cc2)

#  Example how manually color lines in ggplot2
#  ggplot(dat.m, aes(wave, value, colour = variable)) + geom_line() + 
#  scale_colour_manual(values = c('pink','orange','white'))

ba5_brow0.8 <-ggplot(data=landscape_cc_brow0.8_ba, aes(x=year, y=basal_area_m2, colour=species)) + 
  geom_line(size=1.2)+
  scale_colour_manual(values = c("#76BA1B","#006600", "#A4DE02", "orange"))+
  ggtitle("Clearcut management in brow pressure 0.8") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area [m2/ha]")+
  theme_bw()

ba6_brow0.8 <-ggplot(data=landscape_sw_brow0.8_ba, aes(x=year, y=basal_area_m2, colour=species)) + 
  geom_line(size=1.2) +
  scale_colour_manual(values = c("#76BA1B","#006600", "#A4DE02", "orange"))+
  ggtitle("Shelterwood management in brow pressure 0.8")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area [m2/ha]")+
  theme_bw()

grid.arrange(ba5_brow0.8,ba6_brow0.8,ncol=1)

#_________________________________________________________________________________
# IN in brow pressure 1.2

landscape_cc_brow1.2_ba <- landscape_cc_brow1.2 %>% filter(species %in% species.to.keep)
landscape_sw_brow1.2_ba <- landscape_sw_brow1.2 %>% filter(species %in% species.to.keep)
#head(landscape_cc2)

#  Example how manually color lines in ggplot2
#  ggplot(dat.m, aes(wave, value, colour = variable)) + geom_line() + 
#  scale_colour_manual(values = c('pink','orange','white'))

ba7_brow1.2 <-ggplot(data=landscape_cc_brow1.2_ba, aes(x=year, y=basal_area_m2, colour=species)) + 
  geom_line(size=1.2)+
  scale_colour_manual(values = c("#76BA1B","#006600", "#A4DE02", "orange"))+
  ggtitle("Clearcut management in brow pressure 1.2") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area [m2/ha]")+
  theme_bw()

ba8_brow1.2 <-ggplot(data=landscape_sw_brow1.2_ba, aes(x=year, y=basal_area_m2, colour=species)) + 
  geom_line(size=1.2) +
  scale_colour_manual(values = c("#76BA1B","#006600", "#A4DE02", "orange"))+
  ggtitle("Shelterwood management in brow pressure 1.2")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area [m2/ha]")+
  theme_bw()

grid.arrange(ba7_brow1.2,ba8_brow1.2,ncol=1)

# ALL THE 8 TOGETHER

grid.arrange(ba1_brow0, ba2_brow0, ba3_brow0.3, ba4_brow0.3, ba5_brow0.8, ba6_brow0.8, ba7_brow1.2, ba8_brow1.2, ncol=2)

#_______________________________________________________________________________
# BASAL AREA PROPORTION BASED ON DBH CLASSES

#_______________________________________________________________________________
# BASAL AREA PROPORTION BASED ON DBH CLASSES
# Make the distribution of the Basal Area by DBH classes and species composition

area<-landscape_sw_brow0$area[1]
print(area)
new_order_gg2=c("piab", "abal", "lade", "pisy", "fasy", "quro", "acps", "frex","cabe", "bepe", "qupe", "algl", "potr", "poni", "tico", "saca", "rops")


#_______________________________________________________________________________
# IN BROWSING PRESSURE 0

dynamicstand_sw_brow0_0 <- dynamicstand_sw_brow0 %>% filter(year==0)
dynamicstand_cc_brow0_400 <- dynamicstand_cc_brow0 %>% filter(year==400)
dynamicstand_sw_brow0_400 <- dynamicstand_sw_brow0 %>% filter(year==400)

ba_dbh_sw_brow0_0 <- dynamicstand_sw_brow0_0[,8:24]
ba_dbh_cc_brow0_400 <- dynamicstand_cc_brow0_400[,8:24]
ba_dbh_sw_brow0_400 <- dynamicstand_sw_brow0_400[,8:24]

row.names(ba_dbh_sw_brow0_0) <- dynamicstand_sw_brow0_0$species
colnames(ba_dbh_sw_brow0_0) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")
row.names(ba_dbh_sw_brow0_400) <- dynamicstand_sw_brow0_400$species
colnames(ba_dbh_sw_brow0_400) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")
row.names(ba_dbh_cc_brow0_400) <- dynamicstand_cc_brow0_400$species
colnames(ba_dbh_cc_brow0_400) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")


ba_dbh_sw_brow0_0<-ba_dbh_sw_brow0_0/area       # divide by the area because of the dynamic stand output
ba_dbh_sw_brow0_400<-ba_dbh_sw_brow0_400/area   # divide by the area because of the dynamic stand output
ba_dbh_cc_brow0_400<-ba_dbh_cc_brow0_400/area   # divide by the area because of the dynamic stand output


par(mfrow=c(3,1))
par(mar=c(4,4,4,15))
barplot(as.matrix(ba_dbh_sw_brow0_0), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]", ylim=c(0,5), main = "Basal area of initial forest stage", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])


barplot(as.matrix(ba_dbh_cc_brow0_400), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]",  ylim=c(0,5),main = "Basal area of forest at age 400 CC brow0", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])


barplot(as.matrix(ba_dbh_sw_brow0_400), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]",  ylim=c(0,5), main = "Basal area of forest at age 400 SW brow0", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])

legend(21,12.5,legend = new_order_gg2,cex=1,fill = cols[new_order_gg2], xpd = NA)

#_______________________________________________________________________________
#_______________________________________________________________________________
# IN BROWSING PRESSURE 0.3

dynamicstand_sw_brow0.3_0 <- dynamicstand_sw_brow0.3 %>% filter(year==0)
dynamicstand_cc_brow0.3_400 <- dynamicstand_cc_brow0.3 %>% filter(year==400)
dynamicstand_sw_brow0.3_400 <- dynamicstand_sw_brow0.3 %>% filter(year==400)

ba_dbh_sw_brow0.3_0 <- dynamicstand_sw_brow0.3_0[,8:24]
ba_dbh_cc_brow0.3_400 <- dynamicstand_cc_brow0.3_400[,8:24]
ba_dbh_sw_brow0.3_400 <- dynamicstand_sw_brow0.3_400[,8:24]

row.names(ba_dbh_sw_brow0.3_0) <- dynamicstand_sw_brow0.3_0$species
colnames(ba_dbh_sw_brow0.3_0) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")
row.names(ba_dbh_sw_brow0.3_400) <- dynamicstand_sw_brow0.3_400$species
colnames(ba_dbh_sw_brow0.3_400) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")
row.names(ba_dbh_cc_brow0.3_400) <- dynamicstand_cc_brow0.3_400$species
colnames(ba_dbh_cc_brow0.3_400) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")


ba_dbh_sw_brow0.3_0<-ba_dbh_sw_brow0.3_0/area       # divide by the area because of the dynamic stand output
ba_dbh_sw_brow0.3_400<-ba_dbh_sw_brow0.3_400/area   # divide by the area because of the dynamic stand output
ba_dbh_cc_brow0.3_400<-ba_dbh_cc_brow0.3_400/area   # divide by the area because of the dynamic stand output


par(mfrow=c(3,1))
par(mar=c(4,4,4,15))
barplot(as.matrix(ba_dbh_sw_brow0.3_0), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]", ylim=c(0,5), main = "Basal area of initial forest stage", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])


barplot(as.matrix(ba_dbh_cc_brow0.3_400), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]",  ylim=c(0,5),main = "Basal area of forest at age 400 CC brow0.3", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])

barplot(as.matrix(ba_dbh_sw_brow0.3_400), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]",  ylim=c(0,5), main = "Basal area of forest at age 400 SW brow0.3", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])

legend(21,12.5,legend = new_order_gg2,cex=0.9,fill = cols[new_order_gg2], xpd = NA)

#_______________________________________________________________________________

#_______________________________________________________________________________
# IN BROWSING PRESSURE 0.8

dynamicstand_sw_brow0.8_0 <- dynamicstand_sw_brow0.8 %>% filter(year==0)
dynamicstand_cc_brow0.8_400 <- dynamicstand_cc_brow0.8 %>% filter(year==400)
dynamicstand_sw_brow0.8_400 <- dynamicstand_sw_brow0.8 %>% filter(year==400)

ba_dbh_sw_brow0.8_0 <- dynamicstand_sw_brow0.8_0[,8:24]
ba_dbh_cc_brow0.8_400 <- dynamicstand_cc_brow0.8_400[,8:24]
ba_dbh_sw_brow0.8_400 <- dynamicstand_sw_brow0.8_400[,8:24]

row.names(ba_dbh_sw_brow0.8_0) <- dynamicstand_sw_brow0.8_0$species
colnames(ba_dbh_sw_brow0.8_0) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")
row.names(ba_dbh_sw_brow0.8_400) <- dynamicstand_sw_brow0.8_400$species
colnames(ba_dbh_sw_brow0.8_400) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")
row.names(ba_dbh_cc_brow0.8_400) <- dynamicstand_cc_brow0.8_400$species
colnames(ba_dbh_cc_brow0.8_400) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")


ba_dbh_sw_brow0.8_0<-ba_dbh_sw_brow0.8_0/area       # divide by the area because of the dynamic stand output
ba_dbh_sw_brow0.8_400<-ba_dbh_sw_brow0.8_400/area   # divide by the area because of the dynamic stand output
ba_dbh_cc_brow0.8_400<-ba_dbh_cc_brow0.8_400/area   # divide by the area because of the dynamic stand output


par(mfrow=c(3,1))
par(mar=c(4,4,4,15))
barplot(as.matrix(ba_dbh_sw_brow0.8_0), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]", ylim=c(0,5), main = "Basal area of initial forest stage", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])


barplot(as.matrix(ba_dbh_cc_brow0.8_400), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]",  ylim=c(0,5),main = "Basal area of forest at age 400 CC brow0.8", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])

barplot(as.matrix(ba_dbh_sw_brow0.8_400), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]",  ylim=c(0,5), main = "Basal area of forest at age 400 SW brow0.8", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])

legend(21,12.5,legend = new_order_gg2,cex=0.9,fill = cols[new_order_gg2], xpd = NA)

#_______________________________________________________________________________
# IN BROWSING PRESSURE 1.2

dynamicstand_sw_brow1.2_0 <- dynamicstand_sw_brow1.2 %>% filter(year==0)
dynamicstand_cc_brow1.2_400 <- dynamicstand_cc_brow1.2 %>% filter(year==400)
dynamicstand_sw_brow1.2_400 <- dynamicstand_sw_brow1.2 %>% filter(year==400)

ba_dbh_sw_brow1.2_0 <- dynamicstand_sw_brow1.2_0[,8:24]
ba_dbh_cc_brow1.2_400 <- dynamicstand_cc_brow1.2_400[,8:24]
ba_dbh_sw_brow1.2_400 <- dynamicstand_sw_brow1.2_400[,8:24]

row.names(ba_dbh_sw_brow1.2_0) <- dynamicstand_sw_brow1.2_0$species
colnames(ba_dbh_sw_brow1.2_0) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")
row.names(ba_dbh_sw_brow1.2_400) <- dynamicstand_sw_brow1.2_400$species
colnames(ba_dbh_sw_brow1.2_400) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")
row.names(ba_dbh_cc_brow1.2_400) <- dynamicstand_cc_brow1.2_400$species
colnames(ba_dbh_cc_brow1.2_400) <- c("DBH5","DBH10","DBH15","DBH20","DBH25","DBH30","DBH35","DBH40","DBH45","DBH50","DBH55","DBH60","DBH65","DBH70","DBH75","DBH80","DBH_max")


ba_dbh_sw_brow1.2_0<-ba_dbh_sw_brow1.2_0/area       # divide by the area because of the dynamic stand output
ba_dbh_sw_brow1.2_400<-ba_dbh_sw_brow1.2_400/area   # divide by the area because of the dynamic stand output
ba_dbh_cc_brow1.2_400<-ba_dbh_cc_brow1.2_400/area   # divide by the area because of the dynamic stand output


par(mfrow=c(3,1))
par(mar=c(4,4,4,15))
barplot(as.matrix(ba_dbh_sw_brow1.2_0), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]", ylim=c(0,5), main = "Basal area of initial forest stage", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])


barplot(as.matrix(ba_dbh_cc_brow1.2_400), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]",  ylim=c(0,5),main = "Basal area of forest at age 400 CC brow1.2", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])

barplot(as.matrix(ba_dbh_sw_brow1.2_400), col = cols[new_order_gg2], ylab = "Basal area [m3/ha]",  ylim=c(0,5), main = "Basal area of forest at age 400 SW brow1.2", cex.main=1, cex.lab=1)
#legend("topright",                                
#       legend = new_order_gg2,
#       cex=0.9,
#       fill = cols[new_order_gg2])

legend(21,12.5,legend = new_order_gg2,cex=0.9,fill = cols[new_order_gg2], xpd = NA)

#_______________________________________________________________________________
#_______________________________________________________________________________
library(RColorBrewer)
display.brewer.all()
library(viridis)           
library(colorRamps)

# DISTURBANCE REGIME

area<-landscape_sw_brow0.3$area[1]
print(area)

head(dynamicstand_sw_brow0.3)


dynamicstand_sw_0_brow0.3 <- dynamicstand_sw_brow0.3 %>% filter(year==0)
dynamicstand_cc_400_brow0.3 <- dynamicstand_cc_brow0.3 %>% filter(year==400)
dynamicstand_sw_400_brow0.3 <- dynamicstand_sw_brow0.3 %>% filter(year==400)

ba_dbh_sw_0_brow0.3 <- dynamicstand_sw_0_brow0.3[,8:24]
ba_dbh_cc_400_brow0.3 <- dynamicstand_cc_400_brow0.3[,8:24]
ba_dbh_sw_400_brow0.3 <- dynamicstand_sw_400_brow0.3[,8:24]

row.names(ba_dbh_sw_0_brow0.3) <- dynamicstand_sw_0_brow0.3$species
colnames(ba_dbh_sw_0_brow0.3) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")
row.names(ba_dbh_sw_400_brow0.3) <- dynamicstand_sw_400_brow0.3$species
colnames(ba_dbh_sw_400_brow0.3) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")
row.names(ba_dbh_cc_400_brow0.3) <- dynamicstand_cc_400_brow0.3$species
colnames(ba_dbh_cc_400_brow0.3) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")


ba_dbh_sw_0_brow0.3<-ba_dbh_sw_0_brow0.3/area   # divide by the area because of the dynamic stand output
ba_dbh_sw_400_brow0.3<-ba_dbh_sw_400_brow0.3/area   # divide by the area because of the dynamic stand output
ba_dbh_cc_400_brow0.3<-ba_dbh_cc_400_brow0.3/area   # divide by the area because of the dynamic stand output

rgb.palette <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F","cyan","#007FFF", "blue", "#00007F"), space = "rgb")

colors<-rgb.palette(17)

names(colors)<-dynamicstand_sw_0_brow0.3$species

colors

par(mfrow=c(3,1))
par(mar=c(4,4,4,15))
barplot(as.matrix(ba_dbh_sw_0_brow0.3), col = colors, ylab = "Basal area", main = "Year 0 in disturbance regime", cex.main=2, cex.lab=1.5, ylim=c(0,5))

barplot(as.matrix(ba_dbh_sw_400_brow0.3), col =colors, ylab = "Basal area", main = "Year 400 shelterwood in disturbance regime", cex.main=2, cex.lab=1.5, ylim=c(0,5))
barplot(as.matrix(ba_dbh_cc_400_brow0.3), col = colors, ylab = "Basal area", main = "Year 400 clearcut in disturbance regime", cex.main=2, cex.lab=1.5, ylim=c(0,5))

legend(21,18, legend = names(colors),cex=1.5,fill = colors, xpd = NA)

#_______________________________________________________________________________

area<-landscape_sw$area[1]
print(area)

head(dynamicstand_sw)


dynamicstand_sw_0 <- dynamicstand_sw %>% filter(year==0)
dynamicstand_cc_400 <- dynamicstand_cc %>% filter(year==399)
dynamicstand_sw_400 <- dynamicstand_sw %>% filter(year==399)

ba_dbh_sw_0 <- dynamicstand_sw_0[,8:24]
ba_dbh_cc_400 <- dynamicstand_cc_400[,8:24]
ba_dbh_sw_400 <- dynamicstand_sw_400[,8:24]

row.names(ba_dbh_sw_0) <- dynamicstand_sw_0$species
colnames(ba_dbh_sw_0) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")
row.names(ba_dbh_sw_400) <- dynamicstand_sw_400$species
colnames(ba_dbh_sw_400) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")
row.names(ba_dbh_cc_400) <- dynamicstand_cc_400$species
colnames(ba_dbh_cc_400) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")


ba_dbh_sw_0<-ba_dbh_sw_0/area   # divide by the area because of the dynamic stand output
ba_dbh_sw_400<-ba_dbh_sw_400/area   # divide by the area because of the dynamic stand output
ba_dbh_cc_400<-ba_dbh_cc_400/area   # divide by the area because of the dynamic stand output

#rgb.palette <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F","cyan","#007FFF", "blue", "#00007F"), space = "rgb")

#colors<-rgb.palette(17)

#names(colors)<-dynamicstand_sw_0$species

#colors

par(mfrow=c(3,1))
par(mar=c(4,4,4,15))
barplot(as.matrix(ba_dbh_sw_0), col = colors, ylab = "Basal area", main = "Year 0 ", cex.main=2, cex.lab=1.5, ylim=c(0,5))

barplot(as.matrix(ba_dbh_sw_400), col =colors, ylab = "Basal area", main = "Year 400 shelterwood ", cex.main=2, cex.lab=1.5, ylim=c(0,5))
barplot(as.matrix(ba_dbh_cc_400), col = colors, ylab = "Basal area", main = "Year 400 clearcut", cex.main=2, cex.lab=1.5, ylim=c(0,5))

legend(22,24, legend = names(colors),cex=1.5,fill = colors, xpd = NA)

#_______________________________________________________________________________

# DEEP UNDERSTANDING BA DBH DYNAMICS

# Working with DPLYR library to filter, group, mutate and arrange

#_______________________________________ CC year 400

dynamicstand_cc_400 <- dynamicstand_cc%>%
  filter(year==399)

ba_dbh_cc_400 <- dynamicstand_cc_400[,8:24]
ba_dbh_cc_400

row.names(ba_dbh_cc_400) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","qupe","algl","potr","poni","tico","saca")
ba_dbh_cc_400

colnames(ba_dbh_cc_400) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")

# barplot(as.matrix(ba_dbh_cc_400))

#_________________________________________ CC year 0

dynamicstand_cc_0 <- dynamicstand_cc%>%
  filter(year==0)

ba_dbh_cc_0 <- dynamicstand_cc_0[,8:24]
# ba_dbh_cc_0

row.names(ba_dbh_cc_0) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops")
# ba_dbh_cc_0

colnames(ba_dbh_cc_0) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")

# barplot(as.matrix(ba_dbh_cc_0))

#_________________________________________ SW year 400

dynamicstand_sw_400 <- dynamicstand_sw%>%
  filter(year==399)

ba_dbh_sw_400 <- dynamicstand_sw_400[,8:24]
# ba_dbh_cc_0

row.names(ba_dbh_sw_400) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","qupe","algl","potr","poni","tico","saca")
# ba_dbh_cc_0

colnames(ba_dbh_sw_400) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")

# barplot(as.matrix(ba_dbh_cc_0))


#_________________________________________ SW year 0

dynamicstand_sw_0 <- dynamicstand_sw%>%
  filter(year==0)

ba_dbh_sw_0 <- dynamicstand_sw_0[,8:24]


row.names(ba_dbh_sw_0) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops")


colnames(ba_dbh_sw_0) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")


#________________________________________ CC year 100

dynamicstand_cc_100 <- dynamicstand_cc%>%
  filter(year==100)

ba_dbh_cc_100 <- dynamicstand_cc_100[,8:24]

row.names(ba_dbh_cc_100) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca", "rops")

colnames(ba_dbh_cc_100) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")

#_________________________________________ CC year 200

dynamicstand_cc_200 <- dynamicstand_cc%>%
  filter(year==200)

ba_dbh_cc_200 <- dynamicstand_cc_200[,8:24]

row.names(ba_dbh_cc_200) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","qupe","algl","potr","poni","tico","saca")

colnames(ba_dbh_cc_200) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")

#_________________________________________ CC year 300

dynamicstand_cc_300 <- dynamicstand_cc%>%
  filter(year==300)

ba_dbh_cc_300 <- dynamicstand_cc_300[,8:24]

row.names(ba_dbh_cc_300) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","qupe","algl","potr","poni","tico","saca")

colnames(ba_dbh_cc_300) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")


#_________________________________________ SW year 100

dynamicstand_sw_100 <- dynamicstand_sw%>%
  filter(year==100)

ba_dbh_sw_100 <- dynamicstand_sw_100[,8:24]

row.names(ba_dbh_sw_100) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops")

colnames(ba_dbh_sw_100) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")


#_________________________________________ SW year 200

dynamicstand_sw_200 <- dynamicstand_sw%>%
  filter(year==200)

ba_dbh_sw_200 <- dynamicstand_sw_200[,8:24]

row.names(ba_dbh_sw_200) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","qupe","algl","potr","poni","tico","saca", "rops")

colnames(ba_dbh_sw_200) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")


#_________________________________________ SW year 300

dynamicstand_sw_300 <- dynamicstand_sw%>%
  filter(year==300)

ba_dbh_sw_300 <- dynamicstand_sw_300[,8:24]

row.names(ba_dbh_sw_300) <- c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","qupe","algl","potr","poni","tico","saca")

colnames(ba_dbh_sw_300) <- c("dbh5","dbh10","dbh15","dbh20","dbh25","dbh30","dbh35","dbh40","dbh45","dbh50","dbh55","dbh60","dbh65","dbh70","dbh75","db80","dbh_max")

#_______________________________________________________ Plotting in bar chart and using color palettes

# TO ADD THE COLOURS 

library(RColorBrewer)
#display.brewer.all()
library(viridis)           
library(colorRamps)

# Trying other palette

cl <- c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000", "pink", "darkgreen", "violet", "black")
cl2 <- c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000", "pink", "brown", "orange", "green")
cl3 <- c("#1B9E77","#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")

barplot(as.matrix(ba_dbh_cc_0), col = cl)
barplot(as.matrix(ba_dbh_cc_0), col = viridis(17))
barplot(as.matrix(ba_dbh_cc_0), col = rgb.palette(17))


rgb.palette <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F","cyan","#007FFF", "blue", "#00007F"),
                                space = "rgb")

dev.off()

#   BAR PLOT CC 0 __________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_cc_0), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "Clear cut Landscape Basal Area divided in DBH classes by species Tran. Period year 0", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT CC 100 __________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_cc_100), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "Clear cut Landscape Basal Area divided in DBH classes by species Tran. Period year 100", cex.main=2, cex.lab=1.5)

#   Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT CC 200 __________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_cc_200), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "Clear cut Landscape Basal Area divided in DBH classes by species Tran. Period year 200", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT CC 300 __________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_cc_300), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "Clear cut Landscape Basal Area divided in DBH classes by species Tran. Period year 300", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT CC 400 __________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_cc_400), 
        col = rgb.palette(17), 
        ylab = "Landscape BA (m2)",
        main = "Clear cut Landscape Basal Area divided in DBH classes by species Tran. Period year 400",
        cex.main=2, cex.lab=1.5)

#   Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT SW 0__________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_sw_0), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "Shelterwood Landscape Basal Area divided in DBH classes by species Tran. Period year 0", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT SW 100 __________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_sw_100), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "Shelterwood Landscape Basal Area divided in DBH classes by species Tran. Period year 100", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT SW 200__________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_sw_200), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "Shelterwood Landscape Basal Area divided in DBH classes by species Tran. Period year 200", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT SW 300__________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_sw_300), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "Shelterwood Landscape Basal Area divided in DBH classes by species Tran. Period year 300", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________

#   BAR PLOT SW 400 __________________________________________________________________________________________________________________

barplot(as.matrix(ba_dbh_sw_400), col = rgb.palette(17), ylab = "Landscape BA (m2)", main = "Shelterwood Landscape Basal Area divided in DBH classes by species Tran. Period year 400", cex.main=2, cex.lab=1.5)

# Add legend to barplot

legend("topright",                                
       legend = c("piab","abal","lade","pisy","fasy","quro","acps","frex","cabe","bepe","qupe","algl","potr","poni","tico","saca","rops"),
       cex=0.9,
       fill = rgb.palette(17))

#   BAR PLOT __________________________________________________________________________________________________________________
#_______________________________________________________________________________
# FINISHED DEEP UNDERSTANDING BA DBH DYNAMICS



# link for edit the labels and the x and y titles

######### IMPORTANT #############  http://www.sthda.com/english/wiki/add-titles-to-a-plot-in-r-software

# legend("top", c("finalcut", "thinning/regcut","salvaged"), cex=1, bty="n", fill=colors, text.font=2)
# leggend, (position, names/variables, size, bty?- contorn?, color filling, text font) 

# barplot(ba_dbh$if_dbh_5_basalarea_0_sum)  to plot only one column with bar charth

# have a look at this website   https://statisticsglobe.com/barplot-in-r


# ggplot(data_ggp, aes(x = group, y = values)) +        # Create barchart with ggplot2
#  geom_bar(stat = "identity")


# hist(dynamicStand_cc$dbh_mean, main= 'Clear Cut V-DBH Transitional Period')

# hist(dynamicstand_sw$dbh_mean, abeUnit_sw$volume, main= 'Shalterwood Vol-DBH Transitional Period')



# barplot(dynamicStand_cc$year, dynamicStand_cc$dbh_mean, main= 'Clear Cut V-DBH Transitional Period')




#_____________________________________________________________________________________________________
# Wind disturbance impact in the landscape volume in percentages
# BROWSING PRESSURE 0

killed_volume_cc_brow0 <- sum(wind_cc_brow0$killedVolume)                # 1601620 m3
killed_volume_cc_brow0
killed_volume_per_year_cc_brow0 <- killed_volume_cc_brow0/400            # 4014.085
killed_volume_per_year_cc_brow0
killed_volume_per_year_cc_ha_brow0 <- killed_volume_per_year_cc_brow0/17749.26
killed_volume_per_year_cc_ha_brow0                                       # 0.226155 m3

#_____________________________________________

killed_volume_sw_brow0 <- sum(wind_sw_brow0$killedVolume)                # 1362593 m3
killed_volume_sw_brow0
killed_volume_per_year_sw_brow0 <- killed_volume_sw_brow0/400            # 3415.021
killed_volume_per_year_sw_brow0
killed_volume_per_year_sw_ha_brow0 <- killed_volume_per_year_sw_brow0/17749.26
killed_volume_per_year_sw_ha_brow0                                       # 0.1924036 m3                        


dfnew1_cc_brow0 <- landscape_cc_brow0[,c(1,8)]

df_vol_cc_brow0 = dfnew1_cc_brow0 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')

dfnew1_sw_brow0 <- landscape_sw_brow0[,c(1,8)]

df_vol_sw_brow0 = dfnew1_sw_brow0 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')


prop_killed_vol_ha_year_cc_brow0 <- df_vol_cc_brow0 %>% mutate(perc.vol=100*killed_volume_per_year_cc_ha_brow0/tot_vol)

prop_killed_vol_ha_year_sw_brow0 <- df_vol_sw_brow0 %>% mutate(perc.vol=100*killed_volume_per_year_sw_ha_brow0/tot_vol)

par(mfrow = c(1,2))

hist(prop_killed_vol_ha_year_cc_brow0$perc.vol, main = "Landscape proportion of killed volume (m3/ha) by wind per year in CC brow 0",cex.main = 1.5, xlab = "Killed volume / Total volume (%)", ylab = "Frequency (years)",cex.lab = 1.4, col="lightblue", breaks = "FD", ylim = c(0,140))

legend("topright", c(" m3"), cex = 0.9, title = "Average killed volume (m3/ha) per year", text.font = 3, bg='lightpink')

# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(prop_killed_vol_ha_year_sw_brow0$perc.vol, main = "Landscape proportion of killed volume (m3/ha) by wind per year in SW brow 0",cex.main = 1.5, xlab = "Killed volume / Total volume (%)", ylab = "Frequency (years)",cex.lab = 1.4, col="lightblue", breaks = "FD", ylim = c(0,140))

legend("topright", c(" m3"), cex = 0.9, title = "Average killed volume (m3/ha) per year", text.font = 3, bg='lightpink')

#____________________________________________________________________________________________________________________________________
#_________________________________________________________________________________________________________________
#   NEW KILLED VOLUME CALCULATION INCLUDING BARK BEETLE


# Wind AND bark beetle disturbance impact in the landscape volume in percentages
# wind and bark beetle regime

killed_volume_cc_brow0 <- sum(wind_cc_brow0$killedVolume)                   # 5929296 m3
killed_volume_cc_brow0
killed_volume_cc_bb_brow0 <- sum(barkbeetle_cc_brow0$killedVolume)            # 4706904
killed_volume_cc_bb_brow0
killed_volume_cc_dist_brow0 <- killed_volume_cc_brow0 + killed_volume_cc_bb_brow0   # 10636200
killed_volume_cc_dist_brow0
killed_volume_per_year_cc_brow0 <- killed_volume_cc_dist_brow0/400            # 26590.5 
killed_volume_per_year_cc_brow0
killed_volume_per_year_cc_ha_brow0 <- killed_volume_per_year_cc_brow0/17749.26
killed_volume_per_year_cc_ha_brow0                                         # 1.498119 m3   # 1.747298

#_____________________________________________

killed_volume_sw_brow0 <- sum(wind_sw_brow0$killedVolume)                   # 5407252 m3
killed_volume_sw_brow0
killed_volume_sw_bb_brow0 <- sum(barkbeetle_sw_brow0$killedVolume)            # 4388046
killed_volume_sw_bb_brow0
killed_volume_sw_dist_brow0 <- killed_volume_sw_brow0 + killed_volume_sw_bb_brow0  # 9795297
killed_volume_sw_dist_brow0
killed_volume_per_year_sw_brow0 <- killed_volume_sw_dist_brow0/400            # 24488.24
killed_volume_per_year_sw_brow0
killed_volume_per_year_sw_ha_brow0 <- killed_volume_per_year_sw_brow0/17749.26
killed_volume_per_year_sw_ha_brow0                                         # 1.379677 m3                       


# FILTER AND GROUP FOR THE NEEDED COLUMNS AND GROUP BY YEAR TO CREATE NEW DATAFRAMES FOR THE % ANALSIS ON THE KILLED VOLUME PER YEAR

dfnew1_cc_brow0 <- landscape_cc_brow0[,c(1,8)]

df_vol_cc_brow0 = dfnew1_cc_brow0 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')

dfnew1_sw_brow0 <- landscape_sw_brow0[,c(1,8)]

df_vol_sw_brow0 = dfnew1_sw_brow0 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')


prop_killed_vol_ha_year_cc_brow0 <- df_vol_cc_brow0 %>% mutate(perc.vol=100*killed_volume_per_year_cc_ha_brow0/tot_vol)
prop_killed_vol_ha_year_cc_brow0
summary(prop_killed_vol_ha_year_cc_brow0)

prop_killed_vol_ha_year_sw_brow0 <- df_vol_sw_brow0 %>% mutate(perc.vol=100*killed_volume_per_year_sw_ha_brow0/tot_vol)
prop_killed_vol_ha_year_sw_brow0
summary(prop_killed_vol_ha_year_sw_brow0)

par(mfrow = c(1,2))

hist(prop_killed_vol_ha_year_cc_brow0$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in CC brow 0",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,90))

legend("topright", c("m3 / %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')

# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(prop_killed_vol_ha_year_sw_brow0$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in SW brow 0",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,90))

legend("topright", c("m3 /  %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')


### year by year relative proportion ##

# FILTER AND GROUP FOR THE NEEDED COLUMNS AND GROUP BY YEAR TO CREATE NEW DATAFRAMES FOR THE % ANALSIS ON THE KILLED VOLUME PER YEAR

bb_killvol_ha_cc_brow0 <- barkbeetle_cc_brow0$killedVolume/abeUnit_cc_brow0$area
bb_killvol_ha_sw_brow0 <- barkbeetle_sw_brow0$killedVolume/abeUnit_sw_brow0$area

# Add a column of variables in this case volume killed in % of the total ha avg
dist_per_cc_brow0 <- abeUnit_cc_brow0 %>% mutate(perc.vol=100*bb_killvol_ha_cc_brow0/abeUnit_cc_brow0$volume)
summary(dist_per_cc_brow0)

dist_per_sw_brow0 <- abeUnit_sw_brow0 %>% mutate(perc.vol=100*bb_killvol_ha_sw_brow0/abeUnit_sw_brow0$volume)
summary(dist_per_sw_brow0)

par(mfrow = c(1,2))  # 763 m3 of difference in CC than in SW

# natural logarithm is based on the "Euler's number" = e ??? 2,71828183

hist(log(dist_per_cc_brow0$perc.vol), main = "Landscape proportion of killed volume [m3/ha] by bark beetles per year in CC brow 0",cex.main = 1, xlab = " [Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD",xlim = c(-15,3), ylim = c(0,90))

legend("topright", c(" m3 /  %"), cex = 0.55, title = "Average percentage of killed volume [m3/ha/year] by bark beetle", text.font = 3, bg='lightpink')
# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(log(dist_per_sw_brow0$perc.vol), main = "Landscape proportion of killed volume [m3/ha] by bark beetles per year in SW brow 0",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD",xlim = c(-15,3), ylim = c(0,90))

legend("topright", c(" m3 /  %"), cex = 0.55, title = "Average percentage of killed volume [m3/ha/year] by bark beetle", text.font = 3, bg='lightpink')

#_________________________________________________________________________________________________
#_________________________________________________________________________________________________
# Wind disturbance impact in the landscape volume in percentages
# BROWSING PRESSURE 0.3

killed_volume_cc_brow0.3 <- sum(wind_cc_brow0.3$killedVolume)                # 1601620 m3
killed_volume_cc_brow0.3
killed_volume_per_year_cc_brow0.3 <- killed_volume_cc_brow0.3/400            # 4014.085
killed_volume_per_year_cc_brow0.3
killed_volume_per_year_cc_ha_brow0.3 <- killed_volume_per_year_cc_brow0.3/17749.26
killed_volume_per_year_cc_ha_brow0.3                                 # 0.226155 m3

#_____________________________________________

killed_volume_sw_brow0.3 <- sum(wind_sw_brow0.3$killedVolume)                # 1362593 m3
killed_volume_sw_brow0.3
killed_volume_per_year_sw_brow0.3 <- killed_volume_sw_brow0.3/400            # 3415.021
killed_volume_per_year_sw_brow0.3
killed_volume_per_year_sw_ha_brow0.3 <- killed_volume_per_year_sw_brow0.3/17749.26
killed_volume_per_year_sw_ha_brow0.3                                 # 0.1924036 m3                        


dfnew1_cc_brow0.3 <- landscape_cc_brow0.3[,c(1,8)]

df_vol_cc_brow0.3 = dfnew1_cc_brow0.3 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')

dfnew1_sw_brow0.3 <- landscape_sw_brow0.3[,c(1,8)]

df_vol_sw_brow0.3 = dfnew1_sw_brow0.3 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')


prop_killed_vol_ha_year_cc_brow0.3 <- df_vol_cc_brow0.3 %>% mutate(perc.vol=100*killed_volume_per_year_cc_ha_brow0.3/tot_vol)

prop_killed_vol_ha_year_sw_brow0.3 <- df_vol_sw_brow0.3 %>% mutate(perc.vol=100*killed_volume_per_year_sw_ha_brow0.3/tot_vol)

par(mfrow = c(1,2))

hist(prop_killed_vol_ha_year_cc_brow0.3$perc.vol, main = "Landscape proportion of killed volume (m3/ha) by wind per year in CC brow 0.3",cex.main = 1.5, xlab = "Killed volume / Total volume (%)", ylab = "Frequency (years)",cex.lab = 1.4, col="lightblue", breaks = "FD", ylim = c(0,140))

legend("topright", c(" m3"), cex = 0.9, title = "Average killed volume (m3/ha) per year", text.font = 3, bg='lightpink')

# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(prop_killed_vol_ha_year_sw_brow0.3$perc.vol, main = "Landscape proportion of killed volume (m3/ha) by wind per year in SW brow 0.3",cex.main = 1.5, xlab = "Killed volume / Total volume (%)", ylab = "Frequency (years)",cex.lab = 1.4, col="lightblue", breaks = "FD", ylim = c(0,140))

legend("topright", c(" m3"), cex = 0.9, title = "Average killed volume (m3/ha) per year", text.font = 3, bg='lightpink')

#____________________________________________________________________________________________________________________________________
#_________________________________________________________________________________________________________________
#   NEW KILLED VOLUME CALCULATION INCLUDING BARK BEETLE


# Wind AND bark beetle disturbance impact in the landscape volume in percentages
# wind and bark beetle regime

killed_volume_cc_brow0.3 <- sum(wind_cc_brow0.3$killedVolume)                   # 5929296 m3
killed_volume_cc_brow0.3
killed_volume_cc_bb_brow0.3 <- sum(barkbeetle_cc_brow0.3$killedVolume)            # 4706904
killed_volume_cc_bb_brow0.3
killed_volume_cc_dist_brow0.3 <- killed_volume_cc_brow0.3 + killed_volume_cc_bb_brow0.3   # 10636200
killed_volume_cc_dist_brow0.3
killed_volume_per_year_cc_brow0.3 <- killed_volume_cc_dist_brow0.3/400            # 26590.5 
killed_volume_per_year_cc_brow0.3
killed_volume_per_year_cc_ha_brow0.3 <- killed_volume_per_year_cc_brow0.3/17749.26
killed_volume_per_year_cc_ha_brow0.3                                         # 1.498119 m3   # 1.747298

#_____________________________________________

killed_volume_sw_brow0.3 <- sum(wind_sw_brow0.3$killedVolume)                   # 5407252 m3
killed_volume_sw_brow0.3
killed_volume_sw_bb_brow0.3 <- sum(barkbeetle_sw_brow0.3$killedVolume)            # 4388046
killed_volume_sw_bb_brow0.3
killed_volume_sw_dist_brow0.3 <- killed_volume_sw_brow0.3 + killed_volume_sw_bb_brow0.3  # 9795297
killed_volume_sw_dist_brow0.3
killed_volume_per_year_sw_brow0.3 <- killed_volume_sw_dist_brow0.3/400            # 24488.24
killed_volume_per_year_sw_brow0.3
killed_volume_per_year_sw_ha_brow0.3 <- killed_volume_per_year_sw_brow0.3/17749.26
killed_volume_per_year_sw_ha_brow0.3                                         # 1.379677 m3                       


# FILTER AND GROUP FOR THE NEEDED COLUMNS AND GROUP BY YEAR TO CREATE NEW DATAFRAMES FOR THE % ANALSIS ON THE KILLED VOLUME PER YEAR

dfnew1_cc_brow0.3 <- landscape_cc_brow0.3[,c(1,8)]

df_vol_cc_brow0.3 = dfnew1_cc_brow0.3 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')

dfnew1_sw_brow0.3 <- landscape_sw_brow0.3[,c(1,8)]

df_vol_sw_brow0.3 = dfnew1_sw_brow0.3 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')


prop_killed_vol_ha_year_cc_brow0.3 <- df_vol_cc_brow0.3 %>% mutate(perc.vol=100*killed_volume_per_year_cc_ha_brow0.3/tot_vol)
prop_killed_vol_ha_year_cc_brow0.3
summary(prop_killed_vol_ha_year_cc_brow0.3)

prop_killed_vol_ha_year_sw_brow0.3 <- df_vol_sw_brow0.3 %>% mutate(perc.vol=100*killed_volume_per_year_sw_ha_brow0.3/tot_vol)
prop_killed_vol_ha_year_sw_brow0.3
summary(prop_killed_vol_ha_year_sw_brow0.3)

par(mfrow = c(1,2))

hist(prop_killed_vol_ha_year_cc_brow0.3$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in CC brow 0.3",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,90))

legend("topright", c(" m3 / %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')

# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(prop_killed_vol_ha_year_sw_brow0.3$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in SW brow 0.3",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,90))

legend("topright", c(" m3 / %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')


### year by year relative proportion ##

# FILTER AND GROUP FOR THE NEEDED COLUMNS AND GROUP BY YEAR TO CREATE NEW DATAFRAMES FOR THE % ANALSIS ON THE KILLED VOLUME PER YEAR

bb_killvol_ha_cc_brow0.3 <- barkbeetle_cc_brow0.3$killedVolume/abeUnit_cc_brow0.3$area
bb_killvol_ha_sw_brow0.3 <- barkbeetle_sw_brow0.3$killedVolume/abeUnit_sw_brow0.3$area

# Add a column of variables in this case volume killed in % of the total ha avg
dist_per_cc_brow0.3 <- abeUnit_cc_brow0.3 %>% mutate(perc.vol=100*bb_killvol_ha_cc_brow0.3/abeUnit_cc_brow0.3$volume)
summary(dist_per_cc_brow0.3)

dist_per_sw_brow0.3 <- abeUnit_sw_brow0.3 %>% mutate(perc.vol=100*bb_killvol_ha_sw_brow0.3/abeUnit_sw_brow0.3$volume)
summary(dist_per_sw_brow0.3)

par(mfrow = c(1,2))  # 763 m3 of difference in CC than in SW

# natural logarithm is based on the "Euler's number" = e ??? 2,71828183

hist(log(dist_per_cc_brow0.3$perc.vol), main = "Landscape proportion of killed volume [m3/ha] by bark beetles per year in CC brow 0.3",cex.main = 1, xlab = " [Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD",xlim = c(-15,3), ylim = c(0,90))

legend("topright", c(" m3 /  %"), cex = 0.55, title = "Average percentage of killed volume [m3/ha/year] by bark beetle", text.font = 3, bg='lightpink')
# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(log(dist_per_sw_brow0.3$perc.vol), main = "Landscape proportion of killed volume [m3/ha] by bark beetles per year in SW brow 0.3",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD",xlim = c(-15,3), ylim = c(0,90))

legend("topright", c(" m3 /  %"), cex = 0.55, title = "Average percentage of killed volume [m3/ha/year] by bark beetle", text.font = 3, bg='lightpink')


#_________________________________________________________________________________________________
#_________________________________________________________________________________________________
# Wind disturbance impact in the landscape volume in percentages
# BROWSING PRESSURE 0.8

killed_volume_cc_brow0.8 <- sum(wind_cc_brow0.8$killedVolume)                # 1601620 m3
killed_volume_cc_brow0.8
killed_volume_per_year_cc_brow0.8 <- killed_volume_cc_brow0.8/400            # 4014.085
killed_volume_per_year_cc_brow0.8
killed_volume_per_year_cc_ha_brow0.8 <- killed_volume_per_year_cc_brow0.8/17749.26
killed_volume_per_year_cc_ha_brow0.8                                 # 0.226155 m3

#_____________________________________________

killed_volume_sw_brow0.8 <- sum(wind_sw_brow0.8$killedVolume)                # 1362593 m3
killed_volume_sw_brow0.8
killed_volume_per_year_sw_brow0.8 <- killed_volume_sw_brow0.8/400            # 3415.021
killed_volume_per_year_sw_brow0.8
killed_volume_per_year_sw_ha_brow0.8 <- killed_volume_per_year_sw_brow0.8/17749.26
killed_volume_per_year_sw_ha_brow0.8                                 # 0.1924036 m3                        


dfnew1_cc_brow0.8 <- landscape_cc_brow0.8[,c(1,8)]

df_vol_cc_brow0.8 = dfnew1_cc_brow0.8 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')

dfnew1_sw_brow0.8 <- landscape_sw_brow0.8[,c(1,8)]

df_vol_sw_brow0.8 = dfnew1_sw_brow0.8 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')


prop_killed_vol_ha_year_cc_brow0.8 <- df_vol_cc_brow0.8 %>% mutate(perc.vol=100*killed_volume_per_year_cc_ha_brow0.8/tot_vol)

prop_killed_vol_ha_year_sw_brow0.8 <- df_vol_sw_brow0.8 %>% mutate(perc.vol=100*killed_volume_per_year_sw_ha_brow0.8/tot_vol)

par(mfrow = c(1,2))

hist(prop_killed_vol_ha_year_cc_brow0.8$perc.vol, main = "Landscape proportion of killed volume (m3/ha) by wind per year in CC brow 0.8",cex.main = 1.5, xlab = "Killed volume / Total volume (%)", ylab = "Frequency (years)",cex.lab = 1.4, col="lightblue", breaks = "FD", ylim = c(0,140))

legend("topright", c(" m3"), cex = 0.9, title = "Average killed volume (m3/ha) per year", text.font = 3, bg='lightpink')

# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(prop_killed_vol_ha_year_sw_brow0.8$perc.vol, main = "Landscape proportion of killed volume (m3/ha) by wind per year in SW brow 0.8",cex.main = 1.5, xlab = "Killed volume / Total volume (%)", ylab = "Frequency (years)",cex.lab = 1.4, col="lightblue", breaks = "FD", ylim = c(0,140))

legend("topright", c(" m3"), cex = 0.9, title = "Average killed volume (m3/ha) per year", text.font = 3, bg='lightpink')

#____________________________________________________________________________________________________________________________________
#_________________________________________________________________________________________________________________
#   NEW KILLED VOLUME CALCULATION INCLUDING BARK BEETLE


# Wind AND bark beetle disturbance impact in the landscape volume in percentages
# wind and bark beetle regime

killed_volume_cc_brow0.8 <- sum(wind_cc_brow0.8$killedVolume)                   # 5929296 m3
killed_volume_cc_brow0.8
killed_volume_cc_bb_brow0.8 <- sum(barkbeetle_cc_brow0.8$killedVolume)            # 4706904
killed_volume_cc_bb_brow0.8
killed_volume_cc_dist_brow0.8 <- killed_volume_cc_brow0.8 + killed_volume_cc_bb_brow0.8   # 10636200
killed_volume_cc_dist_brow0.8
killed_volume_per_year_cc_brow0.8 <- killed_volume_cc_dist_brow0.8/400            # 26590.5 
killed_volume_per_year_cc_brow0.8
killed_volume_per_year_cc_ha_brow0.8 <- killed_volume_per_year_cc_brow0.8/17749.26
killed_volume_per_year_cc_ha_brow0.8                                            # 1.498119 m3   # 1.747298

#_____________________________________________

killed_volume_sw_brow0.8 <- sum(wind_sw_brow0.8$killedVolume)                   # 5407252 m3
killed_volume_sw_brow0.8
killed_volume_sw_bb_brow0.8 <- sum(barkbeetle_sw_brow0.8$killedVolume)            # 4388046
killed_volume_sw_bb_brow0.8
killed_volume_sw_dist_brow0.8 <- killed_volume_sw_brow0.8 + killed_volume_sw_bb_brow0.8  # 9795297
killed_volume_sw_dist_brow0.8
killed_volume_per_year_sw_brow0.8 <- killed_volume_sw_dist_brow0.8/400            # 24488.24
killed_volume_per_year_sw_brow0.8
killed_volume_per_year_sw_ha_brow0.8 <- killed_volume_per_year_sw_brow0.8/17749.26
killed_volume_per_year_sw_ha_brow0.8                                            # 1.379677 m3                       


# FILTER AND GROUP FOR THE NEEDED COLUMNS AND GROUP BY YEAR TO CREATE NEW DATAFRAMES FOR THE % ANALSIS ON THE KILLED VOLUME PER YEAR

dfnew1_cc_brow0.8 <- landscape_cc_brow0.8[,c(1,8)]

df_vol_cc_brow0.8 = dfnew1_cc_brow0.8 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')

dfnew1_sw_brow0.8 <- landscape_sw_brow0.8[,c(1,8)]

df_vol_sw_brow0.8 = dfnew1_sw_brow0.8 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')


prop_killed_vol_ha_year_cc_brow0.8 <- df_vol_cc_brow0.8 %>% mutate(perc.vol=100*killed_volume_per_year_cc_ha_brow0.8/tot_vol)
prop_killed_vol_ha_year_cc_brow0.8
summary(prop_killed_vol_ha_year_cc_brow0.8)

prop_killed_vol_ha_year_sw_brow0.8 <- df_vol_sw_brow0.8 %>% mutate(perc.vol=100*killed_volume_per_year_sw_ha_brow0.8/tot_vol)
prop_killed_vol_ha_year_sw_brow0.8
summary(prop_killed_vol_ha_year_sw_brow0.8)

par(mfrow = c(1,2))

hist(prop_killed_vol_ha_year_cc_brow0.8$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in CC brow 0.8",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,90))

legend("topright", c(" m3 /  %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')

# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(prop_killed_vol_ha_year_sw_brow0.8$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in SW brow 0.8",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,90))

legend("topright", c(" m3 /  %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')


### year by year relative proportion ##

# FILTER AND GROUP FOR THE NEEDED COLUMNS AND GROUP BY YEAR TO CREATE NEW DATAFRAMES FOR THE % ANALSIS ON THE KILLED VOLUME PER YEAR

bb_killvol_ha_cc_brow0.8 <- barkbeetle_cc_brow0.8$killedVolume/abeUnit_cc_brow0.8$area
bb_killvol_ha_sw_brow0.8 <- barkbeetle_sw_brow0.8$killedVolume/abeUnit_sw_brow0.8$area

# Add a column of variables in this case volume killed in % of the total ha avg
dist_per_cc_brow0.8 <- abeUnit_cc_brow0.8 %>% mutate(perc.vol=100*bb_killvol_ha_cc_brow0.8/abeUnit_cc_brow0.8$volume)
summary(dist_per_cc_brow0.8)

dist_per_sw_brow0.8 <- abeUnit_sw_brow0.8 %>% mutate(perc.vol=100*bb_killvol_ha_sw_brow0.8/abeUnit_sw_brow0.8$volume)
summary(dist_per_sw_brow0.8)

par(mfrow = c(1,2))  # 763 m3 of difference in CC than in SW

# natural logarithm is based on the "Euler's number" = e ??? 2,71828183

hist(log(dist_per_cc_brow0.8$perc.vol), main = "Landscape proportion of killed volume [m3/ha] by bark beetles per year in CC bro 0.8",cex.main = 1, xlab = " [Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD",xlim = c(-15,3), ylim = c(0,90))

legend("topright", c(" m3 /  %"), cex = 0.55, title = "Average percentage of killed volume [m3/ha/year] by bark beetle", text.font = 3, bg='lightpink')
# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(log(dist_per_sw_brow0.8$perc.vol), main = "Landscape proportion of killed volume [m3/ha] by bark beetles per year in SW brow 0.8",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD",xlim = c(-15,3), ylim = c(0,90))

legend("topright", c(" m3 /  %"), cex = 0.55, title = "Average percentage of killed volume [m3/ha/year] by bark beetle", text.font = 3, bg='lightpink')


#_________________________________________________________________________________________________
#_________________________________________________________________________________________________
# Wind disturbance impact in the landscape volume in percentages
# BROWSING PRESSURE 1.2

killed_volume_cc_brow1.2 <- sum(wind_cc_brow1.2$killedVolume)                # 1601620 m3
killed_volume_cc_brow1.2
killed_volume_per_year_cc_brow1.2 <- killed_volume_cc_brow1.2/400            # 4014.085
killed_volume_per_year_cc_brow1.2
killed_volume_per_year_cc_ha_brow1.2 <- killed_volume_per_year_cc_brow1.2/17749.26
killed_volume_per_year_cc_ha_brow1.2                                 # 0.226155 m3

#_____________________________________________

killed_volume_sw_brow1.2 <- sum(wind_sw_brow1.2$killedVolume)                # 1362593 m3
killed_volume_sw_brow1.2
killed_volume_per_year_sw_brow1.2 <- killed_volume_sw_brow1.2/400            # 3415.021
killed_volume_per_year_sw_brow1.2
killed_volume_per_year_sw_ha_brow1.2 <- killed_volume_per_year_sw_brow1.2/17749.26
killed_volume_per_year_sw_ha_brow1.2                                 # 0.1924036 m3                        


dfnew1_cc_brow1.2 <- landscape_cc_brow1.2[,c(1,8)]

df_vol_cc_brow1.2 = dfnew1_cc_brow1.2 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')

dfnew1_sw_brow1.2 <- landscape_sw_brow1.2[,c(1,8)]

df_vol_sw_brow1.2 = dfnew1_sw_brow1.2 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')


prop_killed_vol_ha_year_cc_brow1.2 <- df_vol_cc_brow1.2 %>% mutate(perc.vol=100*killed_volume_per_year_cc_ha_brow1.2/tot_vol)

prop_killed_vol_ha_year_sw_brow1.2 <- df_vol_sw_brow1.2 %>% mutate(perc.vol=100*killed_volume_per_year_sw_ha_brow1.2/tot_vol)

par(mfrow = c(1,2))

hist(prop_killed_vol_ha_year_cc_brow1.2$perc.vol, main = "Landscape proportion of killed volume (m3/ha) by wind per year in CC brow 1.2",cex.main = 1.5, xlab = "Killed volume / Total volume (%)", ylab = "Frequency (years)",cex.lab = 1.4, col="lightblue", breaks = "FD", ylim = c(0,140))

legend("topright", c(" m3"), cex = 0.9, title = "Average killed volume (m3/ha) per year", text.font = 3, bg='lightpink')

# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(prop_killed_vol_ha_year_sw_brow1.2$perc.vol, main = "Landscape proportion of killed volume (m3/ha) by wind per year in SW brow 1.2",cex.main = 1.5, xlab = "Killed volume / Total volume (%)", ylab = "Frequency (years)",cex.lab = 1.4, col="lightblue", breaks = "FD", ylim = c(0,140))

legend("topright", c(" m3"), cex = 0.9, title = "Average killed volume (m3/ha) per year", text.font = 3, bg='lightpink')

#____________________________________________________________________________________________________________________________________
#_________________________________________________________________________________________________________________
#   NEW KILLED VOLUME CALCULATION INCLUDING BARK BEETLE


# Wind AND bark beetle disturbance impact in the landscape volume in percentages
# wind and bark beetle regime

killed_volume_cc_brow1.2 <- sum(wind_cc_brow1.2$killedVolume)                   # 5929296 m3
killed_volume_cc_brow1.2
killed_volume_cc_bb_brow1.2 <- sum(barkbeetle_cc_brow1.2$killedVolume)            # 4706904
killed_volume_cc_bb_brow1.2
killed_volume_cc_dist_brow1.2 <- killed_volume_cc_brow1.2 + killed_volume_cc_bb_brow1.2   # 10636200
killed_volume_cc_dist_brow1.2
killed_volume_per_year_cc_brow1.2 <- killed_volume_cc_dist_brow1.2/400            # 26590.5 
killed_volume_per_year_cc_brow1.2
killed_volume_per_year_cc_ha_brow1.2 <- killed_volume_per_year_cc_brow1.2/17749.26
killed_volume_per_year_cc_ha_brow1.2                                         # 1.498119 m3   # 1.747298

#_____________________________________________

killed_volume_sw_brow1.2 <- sum(wind_sw_brow1.2$killedVolume)                   # 5407252 m3
killed_volume_sw_brow1.2
killed_volume_sw_bb_brow1.2 <- sum(barkbeetle_sw_brow1.2$killedVolume)            # 4388046
killed_volume_sw_bb_brow1.2
killed_volume_sw_dist_brow1.2 <- killed_volume_sw_brow1.2 + killed_volume_sw_bb_brow1.2  # 9795297
killed_volume_sw_dist_brow1.2
killed_volume_per_year_sw_brow1.2 <- killed_volume_sw_dist_brow1.2/400            # 24488.24
killed_volume_per_year_sw_brow1.2
killed_volume_per_year_sw_ha_brow1.2 <- killed_volume_per_year_sw_brow1.2/17749.26
killed_volume_per_year_sw_ha_brow1.2                                         # 1.379677 m3                       


# FILTER AND GROUP FOR THE NEEDED COLUMNS AND GROUP BY YEAR TO CREATE NEW DATAFRAMES FOR THE % ANALSIS ON THE KILLED VOLUME PER YEAR

dfnew1_cc_brow1.2 <- landscape_cc_brow1.2[,c(1,8)]

df_vol_cc_brow1.2 = dfnew1_cc_brow1.2 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')

dfnew1_sw_brow1.2 <- landscape_sw_brow1.2[,c(1,8)]

df_vol_sw_brow1.2 = dfnew1_sw_brow1.2 %>% group_by(year)  %>%
  summarise(tot_vol = sum(volume_m3),
            .groups = 'drop')


prop_killed_vol_ha_year_cc_brow1.2 <- df_vol_cc_brow1.2 %>% mutate(perc.vol=100*killed_volume_per_year_cc_ha_brow1.2/tot_vol)
prop_killed_vol_ha_year_cc_brow1.2
summary(prop_killed_vol_ha_year_cc_brow1.2)

prop_killed_vol_ha_year_sw_brow1.2 <- df_vol_sw_brow1.2 %>% mutate(perc.vol=100*killed_volume_per_year_sw_ha_brow1.2/tot_vol)
prop_killed_vol_ha_year_sw_brow1.2
summary(prop_killed_vol_ha_year_sw_brow1.2)

par(mfrow = c(1,2))

hist(prop_killed_vol_ha_year_cc_brow1.2$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in CC brow 1.2",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,90))

legend("topright", c(" m3 /  %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')

# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(prop_killed_vol_ha_year_sw_brow1.2$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by wind and bark beetles per year in SW brow 1.2",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD", ylim = c(0,90))

legend("topright", c(" m3 /  %"), cex = 0.55, title = "Average killed volume [m3/ha/year] / avarage % on total volume", text.font = 3, bg='lightpink')


### year by year relative proportion ##

# FILTER AND GROUP FOR THE NEEDED COLUMNS AND GROUP BY YEAR TO CREATE NEW DATAFRAMES FOR THE % ANALSIS ON THE KILLED VOLUME PER YEAR

bb_killvol_ha_cc_brow1.2 <- barkbeetle_cc_brow1.2$killedVolume/abeUnit_cc_brow1.2$area
bb_killvol_ha_sw_brow1.2 <- barkbeetle_sw_brow1.2$killedVolume/abeUnit_sw_brow1.2$area

# Add a column of variables in this case volume killed in % of the total ha avg
dist_per_cc_brow1.2 <- abeUnit_cc_brow1.2 %>% mutate(perc.vol=100*bb_killvol_ha_cc_brow1.2/abeUnit_cc_brow1.2$volume)
summary(dist_per_cc_brow1.2)

dist_per_sw_brow1.2 <- abeUnit_sw_brow1.2 %>% mutate(perc.vol=100*bb_killvol_ha_sw_brow1.2/abeUnit_sw_brow1.2$volume)
summary(dist_per_sw_brow1.2)

par(mfrow = c(1,2))  # 763 m3 of difference in CC than in SW

# natural logarithm is based on the "Euler's number" = e ??? 2,71828183

hist(dist_per_cc_brow1.2$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by bark beetles per year in CC brow 1.2",cex.main = 1, xlab = " [Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD",xlim = c(-15,3), ylim = c(0,90))

legend("topright", c(" m3 /  %"), cex = 0.55, title = "Average percentage of killed volume [m3/ha/year] by bark beetle", text.font = 3, bg='lightpink')
# legend("topleft",c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2),box.col="green", title="sample types", text.font=4,  bg='lightblue')

hist(dist_per_sw_brow1.2$perc.vol, main = "Landscape proportion of killed volume [m3/ha] by bark beetles per year in SW brow 1.2",cex.main = 1, xlab = "[Killed volume / Total volume] = [%]", ylab = "Frequency [years]",cex.lab = 1, col="lightblue", breaks = "FD",xlim = c(-15,3), ylim = c(0,90))

legend("topright", c(" m3 /  %"), cex = 0.55, title = "Average percentage of killed volume [m3/ha/year] by bark beetle", text.font = 3, bg='lightpink')


#____________________________LOOP LOOP LOOP ____________________________________
#
#
#_______________________________________________________________________________
# Loop to see into the transitional period but interately

# install.packages("RSQLite")
library(RSQLite)
library(dplyr)
library(ggplot2)
library(gridExtra)   

#_______________________________________________________________________________
# Path to search the data
dataroot <- ("C:/iLand/2022/20220420/1/")                        # Root for the selection of the data

# CREATE NEW EMPTY DATAFRAME
removals<-c()
lnd<-c()
aUnit<-c()

# NAMES OF THE DATABESES VARIABLES
cases <-c("case1a", "case1b","case2a","case2b","case3a","case3b")

#cases <- c("SW9","SW10","SW11","SW12")

# ALTERNATIE WAY
#fs <- c("aaa","bbb","ccc","ddd")                                      # ALTERNATIVE WAY AAA,BBB,CCC,DDD ARE THE NAMES OF THE DBs
#fs <- c("Cz_region_20220325_BAU_sw_management_brow_0.6")


#_______________________________________________________________________________
# FOR CYCLE FOR THE IMPORT AND ANALYSIS OF THE LANDSCAPE VOLUME AND VOLUME HARVESTED


for (i in (1:length(cases)))  {                                        # We read in the files in the loop. The "i" is for the x-> 1:i 
  
  # "for" = for argument, "(in"= in, "1"= first element of the for cycle to analysis, ": length(cases)))" = throgh the length of the object cases  
  # PAY ATTENTION FROM HERE FOR TESTING
  
  # i <- 1                                                                 # to test but remember to don't run also the }
  
  case<-cases[i]                                                      # ORDINATION OF THE CASE TO IMPORT AS DATABASE
  
  
  # Name of the database
  file <-paste0(dataroot, case, ".sqlite")   # file to read here the case is always the actual case in the loop
  
  # "file"= name of the object, "paste0"+ function to create a NAME for a computer path of selection of data/objects
  
  # ALTERNATIVE WAY
  #f <-paste0(dataroot,fs,".sqlite")
  
  print(file)
  
  # connect to the database of clearcut model
  sqlite.driver <- dbDriver("SQLite")
  db1 <- dbConnect(sqlite.driver, dbname = file)  # connect to the file
  tables.in.the.file<-dbListTables(db1)           # explore the tables in the file
  print(tables.in.the.file)
  
  
  #-----------------------------------------------
  landscape <- dbReadTable(db1,"landscape")
  abeUnit <- dbReadTable(db1, "abeUnit")
  abeStandRemoval <- dbReadTable(db1,"abeStandRemoval")
  
  #carbon <- dbReadTable(db1,"carbon")
  # wind <- dbReadTable(db1,"wind")
  #carbonflow <- dbReadTable(db1, "carbonflow")
  dbDisconnect(db1)    # close the file
  
  landscape.area<-landscape$area[1]
  
  
  
  # Make the 3 categories of removals:
  
  activity.names<-unique(abeStandRemoval$activity)    # here I list all different type of activites
  
  swcuts<- grepl("sw",activity.names)     # I look for only which has "sw" this grepl gives TRUE/FALSE
  activity.names.sw<-activity.names[swcuts] # collect the activity names with sw
  activity.names.notsw<-activity.names[!swcuts]  # collect the activity names withOUT sw
  
  #print(activity.names.sw)
  #print(activity.names.notsw)
  
  # Here I filter only the listed activity names and calculate thinning/finalcut values for every year 
  # (each line is per ha for a stand, so I scale with the area, sum up all the harvest on the landscape and then divide it with the whole area to get again per ha)
  
  ab.regcuts<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.sw)    %>% 
                            group_by(year)   %>%   summarise(volume=sum(volumeThinning*area)/landscape.area, type="regcut", run=case))
  
  ab.finalcuts<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.sw)    %>% 
                              group_by(year)   %>%   summarise(volume=sum(volumeFinal*area)/landscape.area, type="finalcut", run=case))
  
  ab.thinnig<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.notsw)    %>% 
                            group_by(year)   %>%   summarise(volume=sum(volumeThinning*area)/landscape.area, type="thinning", run=case))
  
  if (length(activity.names[swcuts])==0) {
    
    ab.finalcuts<- data.frame(abeStandRemoval %>% filter(activity %in% activity.names.notsw)    %>% 
                                group_by(year)   %>%   summarise(volume=sum(volumeFinal*area)/landscape.area, type="finalcut", run=case))
    
  }
  
  removals<-rbind(removals,ab.regcuts,ab.finalcuts,ab.thinnig)
  
  
  # Collect landscape data:
  landscape<- (landscape %>% mutate(run=case))
  lnd<-rbind(lnd, landscape)
  
  # Collect abeUnit data
  abeUnit<-(abeUnit %>% mutate(run=case))
  aUnit<-rbind(aUnit, abeUnit)
  
}  # end of loop


# TO SUMMARIZE THE CUTTING ACTIVITIES
values<-data.frame(removals %>% group_by(run, type) %>% summarise(volume=mean(volume)))
print(values)
values_sum <- values %>%
  filter(run == "case1b", type=="finalcut", type == "thinning", type == "regcut")  # to have the results you have to decide only one type at time

values_sum


# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE

pdf(paste0(dataroot, "20220420a.pdf"), height=8, width=12)
#pdf(paste0(dataroot, "20220414b.pdf"), height=5, width=15)
#pdf(paste0(dataroot, "20220414c.pdf"), height=10, width=25)



#_______________________________________________________________________________
# This tells the colors:

species.we.have<-unique(lnd$species)                                            # IT IS SAYING WHICH SPECIES WE HAVE IN THE DATABASE IN THIS CASE LANDSCAPE


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
#_______________________________________________________________________________


# STARTING PLOTS

# column diagram
ggplot(removals, aes(year, volume, fill=factor(type, levels=c( "regcut","finalcut","thinning"))))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~run, ncol=1)+
  labs(x = "Year",y="Removed volume m3/ha",fill = "Removal")+
  scale_fill_manual(values=c("#4897D8","#FFDB5C","#FA6E59"))+               #"#B7B8B6","#34675C","#B3C100" grey and greens
  theme_bw()

# Make a plot with ggplot, volume, colored by species for the transitional period for Clear cut management system
#-------------------------------------------------------------------------------

ggplot(lnd, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Clear Cut")+
  facet_wrap(~run, ncol=1)+
  labs(x = "Year",y="Volume m3/ha",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,400)+
  theme_bw()

#-------------------------------------------------------------------------------

ggplot(aUnit, aes(year,realizedHarvest, color=case))+
  geom_line(size=1.2, show.legend = F)+
  facet_wrap(~run, ncol=1)+
  ylim(4.5,15.5)+
  ggtitle("Realized Harvest Transitional Period")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Realized harvest m3/ha")+
  theme_bw()


#-------------------------------------------------------------------------------

# SPECIES specificaly BA:
species.to.keep<-c("piab", "fasy","qupe", "psme")


lnd2 <- lnd %>% filter(species %in% species.to.keep)

ggplot(data=lnd2, aes(x=year, y=basal_area_m2, color=species)) + 
  geom_line(size=1.2)+
  ggtitle("Clear cut") +
  facet_wrap(~run, ncol=1)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Basal area m2/ha")+  theme_bw()


dev.off()

