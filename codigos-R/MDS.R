library(readr)
library(stats)
library(RColorBrewer)



# Carregando dados
musicas = read_csv(".//Users//dubis//Desktop//BSCode//Github//Spotify_MQAM//tabelas//musicas.csv")
musicas$continent[is.na(musicas$continent)] <- "AN"
numericos <- c("loudness", "acousticness", "danceability", "energy", "liveness", "speechiness", "tempo", "valence")

matriz_dados_numericos = musicas[numericos]


#musicas$country[musicas$country %in% c("AU", "CA", "DE", "GB", "SG", "US", "NZ")] <- "DES"
#musicas$country[musicas$country %in% c("BR", "IN", "MX", "ZA", "NG")] <- "EME"

#musicas$country[musicas$country %in% c("DE", "SG", "IN", "MX","AU", "CA", "GB", "US", "NZ")] <- "R"
#musicas$country[musicas$country %in% c("ZA","NG")] <- "A"
#musicas$country[musicas$country %in% c("BR")] <- "B"

musicas$country[musicas$country %in% c("DE", "SG", "IN", "MX","AU", "CA", "GB", "US", "NZ")] <- "R"
musicas$country[musicas$country %in% c("ZA","NG")] <- "A"
musicas$country[musicas$country %in% c("BR")] <- "B"
################################################################################
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



################################################################################

# Calculando a distância
distancia_dados_euclidiana <- dist(matriz_dados_numericos)

# Aplicando Escalonamento Multidimensional (1200 x 592)
dados_escalonados_euclidiana <- cmdscale(distancia_dados_euclidiana,  
                                         k = nrow(matriz_dados_numericos) - 1, 
                                         eig = TRUE)

dados_escalonados_euclidiana_eigen <- dados_escalonados_euclidiana$eig

# Aplicando critérios propostos
cumsum (abs(dados_escalonados_euclidiana_eigen)) / sum (abs(dados_escalonados_euclidiana_eigen))
cumsum (dados_escalonados_euclidiana_eigen ^ 2) / sum (dados_escalonados_euclidiana_eigen ^ 2)

pc_data <- data.frame(continentes, dados_escalondados_euclidiana$points[,1]*(-1), dados_escalondados_euclidiana$points[,2]*(-1) )
colnames(pc_data) = c("continentes", "1", "2")

# Plotting PC1 vs PC2
plot_pca("1", "2", "Eixo 1 x Eixo 2 - Euclidiana", "Eixo 1", "Eixo 2")
################################################################################

# Calculando a distância
distancia_dados_manhattan = dist(matriz_dados_numericos, 
                                 method = "manhattan")

# Aplicando Escalonamento Multidimensional (1200 x 592)
dados_escalonados_manhattan <- cmdscale (distancia_dados_manhattan, 
                                         k = nrow(matriz_dados_numericos) - 1, 
                                         eig = TRUE)

dados_escalonados_manhattan_eigen <- dados_escalonados_manhattan$eig

# Aplicando critérios propostos
cumsum (abs(dados_escalonados_manhattan_eigen)) / sum (abs(dados_escalonados_manhattan_eigen))
cumsum (dados_escalonados_manhattan_eigen ^ 2) / sum (dados_escalonados_manhattan_eigen ^ 2)

pc_data <- data.frame(continentes, dados_escalonados_manhattan$points[,1]*(-1), dados_escalonados_manhattan$points[,2]*(-1) )
colnames(pc_data) = c("continentes", "1", "2")

# Plotting PC1 vs PC2
plot_pca("1", "2", "Eixo 1 x Eixo 2 - Manhattan", "Eixo 1", "Eixo 2")

################################################################################

# Calculando a distância
mahalanobis_distance_matrix <- as.dist(mahalanobis(
  x = matriz_dados_numericos,
  center = colMeans(matriz_dados_numericos),
  cov = cov(matriz_dados_numericos)
))

# Aplicando Escalonamento Multidimensional (1200 x 592)
dados_escalonados_mahalanobis <- cmdscale(mahalanobis_distance_matrix, 
                                          k = nrow(matriz_dados_numericos) - 1,
                                          eig = TRUE)

dados_escalonados_mahalanobis_eigen<- dados_escalonados_mahalanobis$eig

# Aplicando critérios propostos
cumsum (abs(dados_escalonados_mahalanobis_eigen)) / sum (abs(dados_escalonados_mahalanobis_eigen))
cumsum (dados_escalonados_mahalanobis_eigen ^ 2) / sum (dados_escalonados_mahalanobis_eigen ^ 2)


pc_data <- data.frame(
  continentes, dados_escalonados_mahalanobis$points[, 1] * (-1), dados_escalonados_mahalanobis$points[, 2] * (-1)
)
colnames(pc_data) <- c("continentes", "1", "2")
# Plotting the first PCA visualization
plot_pca("1", "2", "Eixo 1 x Eixo 2 - Mahalanobis", "Eixo 1", "Eixo 2")