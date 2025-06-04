install.packages("vegan")
install.packages("ggplot2")

# load
data <- read.csv("data/community_matrix.csv", row.names = 1)

#-----------------------------------------------------------------------------
# SPECIES RICHNESS
r1 <- length(data$Plot_1[data$Plot_1>0])
r2 <- length(data$Plot_2[data$Plot_2>0])
r3 <- length(data$Plot_3[data$Plot_3>0])
r4 <- length(data$Plot_4[data$Plot_4>0])
r5 <- length(data$Plot_5[data$Plot_5>0])
rGessi <- length(data$Plot_Gessi[data$Plot_Gessi>0])
richness <- c(r1,r2,r3,r4,r5,rGessi)
richness

# ...faster way
apply(data, FUN = function(x)length(x[x>0]), MARGIN = 2)

# ...even faster
library(vegan)
specnumber(data, MARGIN = 2)

#-----------------------------------------------------------------------------
# SIMPSON INDEX
# calculate total cover
sums <- colSums(data)

data_sim <- data
# calculate relative cover
data_sim$Plot_1 <- data_sim$Plot_1/sums[1]
data_sim$Plot_2 <- data_sim$Plot_2/sums[2]
data_sim$Plot_3 <- data_sim$Plot_3/sums[3]
data_sim$Plot_4 <- data_sim$Plot_4/sums[4]
data_sim$Plot_5 <- data_sim$Plot_5/sums[5]
data_sim$Plot_Gessi <- data_sim$Plot_Gessi/sums[6]
# calculate the square of relative abundances
data_sim$Plot_1 <- data_sim$Plot_1^2
data_sim$Plot_2 <- data_sim$Plot_2^2
data_sim$Plot_3 <- data_sim$Plot_3^2
data_sim$Plot_4 <- data_sim$Plot_4^2
data_sim$Plot_5 <- data_sim$Plot_5^2
data_sim$Plot_Gessi <- data_sim$Plot_Gessi^2
# calculate sum of squares
sums_sq <- colSums(data_sim)
simpson <- 1-sums_sq
simpson

# ...faster way
diversity(data, index = "simpson", MARGIN = 2)

#-----------------------------------------------------------------------------
# SHANNON INDEX
data_shan <- data
# calculate relative cover
data_shan$Plot_1 <- data_shan$Plot_1/sums[1]
data_shan$Plot_2 <- data_shan$Plot_2/sums[2]
data_shan$Plot_3 <- data_shan$Plot_3/sums[3]
data_shan$Plot_4 <- data_shan$Plot_4/sums[4]
data_shan$Plot_5 <- data_shan$Plot_5/sums[5]
data_shan$Plot_Gessi <- data_shan$Plot_Gessi/sums[6]

# multiuply each relative cover by its logarithm
data_shan$Plot_1 <- data_shan$Plot_1 * log(data_shan$Plot_1)
data_shan$Plot_2 <- data_shan$Plot_2 * log(data_shan$Plot_2)
data_shan$Plot_3 <- data_shan$Plot_3 * log(data_shan$Plot_3)
data_shan$Plot_4 <- data_shan$Plot_4 * log(data_shan$Plot_4)
data_shan$Plot_5 <- data_shan$Plot_5 * log(data_shan$Plot_5)
data_shan$Plot_Gessi <- data_shan$Plot_Gessi * log(data_shan$Plot_Gessi)
sums_shan <- colSums(data_shan, na.rm = TRUE)
shannon <- -sums_shan
shannon

# ...faster way
diversity(data, index = "shannon", MARGIN = 2)

#-----------------------------------------------------------------------------
# PIELOU EVENNESS
max_shan <- log(richness)
evenness <- shannon/max_shan
evenness

# create dataframe with all variables
results <- data.frame("Richness" = richness,
                      "Shannon" = shannon,
                      "Simpson" = simpson,
                      "Evenness" = evenness)
write.csv(results, "results.csv", row.names = F)

#-----------------------------------------------------------------------------
# OVERALL BETA DIVERSITIES
data1 <- data[,c("Plot_2", "Plot_4")]
data2 <- data[,c("Plot_3", "Plot_5")]

# delete absent species
data1$sums <- rowSums(data1)
data1 <- data1[data1$sums>0,]
data1$sums <- NULL

data2$sums <- rowSums(data2)
data2 <- data2[data2$sums>0,]
data2$sums <- NULL

# calculate gamma
gamma1 <- nrow(data1)
gamma2 <- nrow(data2)

#calculate mean alpha
alpha1 <- specnumber(data1, MARGIN = 2)
m_alpha1 <- mean(alpha1)

alpha2 <- specnumber(data2, MARGIN = 2)
m_alpha2 <- mean(alpha2)

# MULTIPLICATIVE BETA DIVERSITY (WHITTAKER)
gamma1/m_alpha1
gamma2/m_alpha2

# ADDITIVE BETA DIVERSITY (LANDE)
gamma1 - m_alpha1
gamma2 - m_alpha2

#-----------------------------------------------------------------------------
# PAIRWISE BETA DIVERSITIES 
data_sub <- data[,c(1,2)]
data_sub[data_sub>1] <- 1
data_sub$sum <- rowSums(data_sub)

c <- sum(data_sub$sum == 2)
a <- sum(data_sub$sum == 1 & data_sub$Plot_1 == 1)
b <- sum(data_sub$sum == 1 & data_sub$Plot_2 == 1)

# JACCARD
j <- c/(a+b+c)
1-j

# SORENSEN
s <- 2*c/(a+b+2*c)
1-s

# ...much faster
jaccard <- vegdist(t(data), method = "jaccard")
sorensen <- vegdist(t(data), method = "bray")

jaccard
sorensen

set.seed(3)
nmds <- metaMDS(jaccard)

plot(nmds)
text(nmds$points[,1], nmds$points[,2], rownames(nmds$points))

library(ggplot2)
nmds_out <- as.data.frame(nmds$points)
nmds_out$Site <- c("Ridracoli", "Biserno", "Biserno", "Biserno", "Biserno", "Gessi")
ggplot(data = nmds_out, aes(x = MDS1, y = MDS2, color = Site)) +
  geom_point(size = 4) +
  annotate("text", x = nmds_out$MDS1, y = nmds_out$MDS2, label = rownames(nmds_out), size = 3)
