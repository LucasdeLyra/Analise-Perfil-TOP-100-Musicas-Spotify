library(readr)
library(stats)
library(RColorBrewer)

# Carregando dados
musicas = read_csv("./My Games/Spotify_MQAM/tabelas/musicas.csv")
musicas$continent[is.na(musicas$continent)] <- "AN"
numericos <- c("loudness", "acousticness", "danceability", "energy", "liveness", "speechiness", "tempo", "valence")
matriz_dados_numericos = musicas[numericos]

# Rodando o PCA
y <- prcomp(matriz_dados_numericos, scale=TRUE)
y$rotation <- -1 * y$rotation
y$x <- -1 * y$x

# Unique countries for color assignment
continentes <- musicas$country
unique <- unique(continentes)

# Assign light colors to points and dark colors to labels
light_colors <- brewer.pal(min(length(unique), 8), "Set3")
if (length(unique) > 8) {
  light_colors <- colorRampPalette(dark_colors)(length(unique))
}
dark_colors <- brewer.pal(min(length(unique), 8), "Dark2")
if (length(unique) > 8) {
  dark_colors <- colorRampPalette(dark_colors)(length(unique))
}

# Mapping colors to countries
point_colors <- setNames(light_colors, unique)
label_colors <- setNames(dark_colors, unique)


# Function to plot PCA components with light points and dark labels
plot_pca <- function(pc_x, pc_y, title, l1, l2) {
  # Map colors to each point based on the continent
  colors <- point_colors[continentes]
  
  plot(pc_data[[pc_x]], pc_data[[pc_y]], 
       xlab = l1, ylab = l2, 
       main = title,
       pch = 16, col = colors)
  # Add labels with dark colors
  text(pc_data[[pc_x]], pc_data[[pc_y]], labels = continentes, pos = 3, 
       cex = 0.8, col = label_colors[continentes])
  
  #legend("topleft", legend=unique,         col=point_colors, lty=1:2, cex=0.8, box.lty=0)
}


pc_data <- data.frame(continentes, y$x[, c("PC1", "PC2")])
colnames(pc_data) = c("continentes", "PC1", "PC2")
# Plotting PC1 vs PC2
plot_pca("PC1", "PC2", "'Calma' X 'Bolero' agrupado por continente", "Calma", "Bolero")

pc_data <- data.frame(continentes, y$x[, c("PC1", "PC3")])
colnames(pc_data) = c("continentes", "PC1", "PC3")
# Plotting PC1 vs PC3
plot_pca("PC1", "PC3", "'Calma' X 'Agitação' agrupado por continente", "Calma", "Agitação")


pc_data <- data.frame(continentes, y$x[, c("PC2", "PC3")])
colnames(pc_data) = c("continentes", "PC2", "PC3")
# Plotting PC2 vs PC3
plot_pca("PC2", "PC3", "'Bolero' X 'Agitação' agrupado por país", "Bolero", "Agitação")

# Save PCA loadings to CSV
write.csv(round(y$rotation, 8), "pca_loadings.csv", row.names = TRUE)
