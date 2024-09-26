library(readr)
library(moments)
library(bestNormalize)

musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas_normalizadas.csv")
View(musicas)

musicasFiltrado <- musicas[musicas$country!="NG" & musicas$country!="IN" & musicas$country!="US" & musicas$country!="MX" & musicas$country!="NZ" & musicas$country!="DE",]
View(musicasFiltrado)

musicasFiltrado$valence[musicasFiltrado$valence<=0.33] <- 0.33
musicasFiltrado$valence[musicasFiltrado$valence>0.33 & musicasFiltrado$valence<=0.85] <- 0.85
musicasFiltrado$valence[musicasFiltrado$valence>0.85 & musicasFiltrado$valence<=1] <- 1

valence1 <- subset(musicasFiltrado, valence==0.33)
valence2 <- subset(musicasFiltrado, valence==0.85)
valence3 <- subset(musicasFiltrado, valence==1)

shapiro.test(valence1$danceability)
shapiro.test(valence2$danceability)
shapiro.test(valence3$danceability)

skewness(valence1$danceability)
skewness(valence2$danceability)
skewness(valence3$danceability)

par(mfcol=c(1,3))
normalized <- bestNormalize(valence1$danceability)
valence1 <- predict(normalized)
hist(valence1, probability=TRUE)
lines(density(valence1), 
      col = "red",              # Cor da curva de densidade
      lwd = 2)                  # Espessura da linha da curva
shapiro.test(valence1)


normalized <- bestNormalize(valence2$danceability)
valence2 <- predict(normalized)
hist(valence2, probability=TRUE)
lines(density(valence2), 
      col = "red",              # Cor da curva de densidade
      lwd = 2)                  # Espessura da linha da curva
shapiro.test(valence2)


normalized <- bestNormalize(valence3$danceability)
valence3 <- predict(normalized)
hist(valence3, probability=TRUE)
lines(density(valence3), 
      col = "red",              # Cor da curva de densidade
      lwd = 2)                  # Espessura da linha da curva
shapiro.test(valence3)

