# Installa e carica i pacchetti necessari se non sono già installati
install.packages("MASS")
install.packages("cluster")
install.packages("pracma")
install.packages("ggplot2")
library(MASS)
library(cluster)
library(pracma)
library(ggplot2)

# Crea un dataframe di esempio con dati casuali
set.seed(123)  # Imposta il seed per la riproducibilità
n <- 200  # Numero di punti dati

# Genera dati casuali per "x" e "y"
df <- data.frame(
  x = rnorm(n),
  y = rnorm(n)
)

# Supponiamo che tu voglia trovare 3 cluster usando K-means
num_clusters <- 3

# Calcola la KDE (Kernel Density Estimation) usando MASS
kde <- kde2d(df$x, df$y)

# Estrai i picchi di densità usando il pacchetto pracma
peaks <- findpeaks(as.numeric(kde$z))

# Estrai le coordinate dei picchi
peak_coordinates <- data.frame(x = peaks[, 1], y = peaks[, 2])

# Esegui K-means sui picchi di densità
kmeans_result <- kmeans(peak_coordinates, centers = num_clusters)

# Ottieni gli indici dei cluster a cui appartengono i picchi
cluster_indices <- kmeans_result$cluster

# Visualizza i risultati
print(cluster_indices)

# Crea un plot dei cluster trovati
ggplot(peak_coordinates, aes(x, y, color = factor(cluster_indices))) +
  geom_point(size = 3) +
  labs(title = "Cluster Found with Density Peaks") +
  theme_minimal()
