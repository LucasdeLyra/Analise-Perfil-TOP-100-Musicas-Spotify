library(readr)
library(stats)
library(RColorBrewer)

# Carregando dados
musicas <- read_csv("C:\\Users\\dubis\\Desktop\\BSCode\\Github\\Spotify_MQAM\\tabelas\\musicas.csv")
numericos <- c("loudness", "acousticness", "danceability", "energy", "liveness", "speechiness", "tempo", "valence")
matriz_dados_numericos = musicas[numericos]

# Rodando o PCA
y <- prcomp(matriz_dados_numericos, scale=TRUE)
y$rotation <- -1 * y$rotation
y$x <- -1 * y$x

# Unique countries for color assignment
continentes <- musicas$continent
unique_countries <- unique(continentes)

# Assign light colors to points and dark colors to labels
light_colors <- brewer.pal(min(length(unique_countries), 8), "Set3")
if (length(unique_countries) > 8) {
  light_colors <- colorRampPalette(light_colors)(length(unique_countries))
}
dark_colors <- brewer.pal(min(length(unique_countries), 8), "Dark2")
if (length(unique_countries) > 8) {
  dark_colors <- colorRampPalette(dark_colors)(length(unique_countries))
}

# Mapping colors to countries
point_colors <- setNames(light_colors, unique_countries)
label_colors <- setNames(dark_colors, unique_countries)

# Function to plot PCA components with light points and dark labels
plot_pca <- function(pc_x, pc_y, title) {
  plot(pc_data[[pc_x]], pc_data[[pc_y]], 
       xlab = pc_x, ylab = pc_y, 
       main = title,
       pch = 16, col = point_colors[continentes])
  
  # Add labels with dark colors
  text(pc_data[[pc_x]], pc_data[[pc_y]], labels = continentes, pos = 3, 
       cex = 0.8, col = label_colors[continentes])
}

# Plotting PC1 vs PC2
plot_pca("PC1", "PC2", "PCA1 X PCA2 agrupado por continente")

# Plotting PC1 vs PC3
plot_pca("PC1", "PC3", "PCA1 X PCA3 agrupado por continente")

# Plotting PC2 vs PC3
plot_pca("PC2", "PC3", "PCA2 X PCA3 agrupado por continente")

# Save PCA loadings to CSV
write.csv(round(y$rotation, 8), "pca_loadings.csv", row.names = TRUE)


