library(readr)
library(stats)

# Carregando dados
musicas <- read_csv("./Spotify_MQAM-main/tabelas/musicas.csv")
numericos <- c("loudness", "acousticness", "danceability", "energy", "liveness", "speechiness", "tempo", "valence")
matriz_dados_numericos = musicas[numericos]


# Rodando o PCA
y <- prcomp(matriz_dados_numericos, scale=TRUE)
y$rotation <- -1*y$rotation
y$x <- -1*y$x
round(t(t(y$rotation)%*%t(matriz_dados_numericos)), digits = 3)


#Visulizando o mesmo que o professor fez
sum(diag(cov(matriz_dados_numericos)))
sum(eigen(cov(matriz_dados_numericos))$values)

eigen(cov(matriz_dados_numericos))$values/sum(diag(cov(matriz_dados_numericos)))
mean(eigen(cov(matriz_dados_numericos))$values)
cor(y$x,matriz_dados_numericos)


# Método 2 (variância explicadas)
eigen(cov(y$x))$values/sum(diag(cov(y$x)))
eigen(cov(y$x))$values
mean(eigen(cov(y$x))$values)


#Plotando
continentes <- musicas$country

pc_data <- data.frame(continentes, y$x[, c("PC1", "PC2")])
colnames(pc_data) = c("continentes", "PC1", "PC2")
# Plot PC1 vs. PC2
plot(pc_data$PC1, pc_data$PC2, 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA1 X PCA2 agrupado por país",
     pch = 16, col = "#5caa77")

# Add labels to each point using the continent names
text(pc_data$PC1, pc_data$PC2, labels = pc_data$continentes, pos = 3, cex = 0.8, col = "#1c1934")



pc_data <- data.frame(continentes, y$x[, c("PC1", "PC3")])
colnames(pc_data) = c("continentes", "PC1", "PC3")
# Plot PC1 vs. PC3
plot(pc_data$PC1, pc_data$PC3, 
     xlab = "PC1", ylab = "PC3", 
     main = "PCA1 X PCA3 agrupado por país",
     pch = 16, col = "#5caa77")

# Add labels to each point using the continent names
text(pc_data$PC1, pc_data$PC3, labels = pc_data$continentes, pos = 3, cex = 0.8, col = "#1c1934")



pc_data <- data.frame(continentes, y$x[, c("PC2", "PC3")])
colnames(pc_data) = c("continentes", "PC2", "PC3")
# Plot PC2 vs. PC3
plot(pc_data$PC2, pc_data$PC3, 
     xlab = "PC2", ylab = "PC3", 
     main = "PCA2 X PCA3 agrupado por país",
     pch = 16, col = "#5caa77")

# Add labels to each point using the continent names
text(pc_data$PC2, pc_data$PC3, labels = pc_data$continentes, pos = 3, cex = 0.8, col = "#1c1934")

