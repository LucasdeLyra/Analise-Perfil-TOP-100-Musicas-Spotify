library(readr)
library(moments)
library(bestNormalize)

musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas_normalizadas.csv")
View(musicas)

musicasFiltrado <- musicas[musicas$country!="NG" & musicas$country!="IN" & musicas$country!="US" & musicas$country!="MX" & musicas$country!="NZ" & musicas$country!="DE",]
View(musicasFiltrado)

musicasFiltrado$tempo[musicasFiltrado$tempo<=0.33] <- 0.33
musicasFiltrado$tempo[musicasFiltrado$tempo>0.33 & musicasFiltrado$tempo<=0.85] <- 0.67
musicasFiltrado$tempo[musicasFiltrado$tempo>0.85 & musicasFiltrado$tempo<=1] <- 1

tempo1 <- subset(musicasFiltrado, tempo==0.33)
tempo2 <- subset(musicasFiltrado, tempo==0.67)
tempo3 <- subset(musicasFiltrado, tempo==1)

shapiro.test(tempo1$danceability)
shapiro.test(tempo2$danceability)
shapiro.test(tempo3$danceability)

skewness(tempo1$danceability)
skewness(tempo2$danceability)
skewness(tempo3$danceability)

par(mfcol=c(1,3))
normalized <- bestNormalize(tempo1$danceability)
tempo1 <- predict(normalized)
hist(tempo1, probability=TRUE)
lines(density(tempo1), 
      col = "red",              # Cor da curva de densidade
      lwd = 2)                  # Espessura da linha da curva
shapiro.test(tempo1)


normalized <- bestNormalize(tempo2$danceability)
tempo2 <- predict(normalized)
hist(tempo2, probability=TRUE)
lines(density(tempo2), 
      col = "red",              # Cor da curva de densidade
      lwd = 2)                  # Espessura da linha da curva
shapiro.test(tempo2)


normalized <- bestNormalize(tempo3$danceability)
tempo3 <- predict(normalized)
hist(tempo3, probability=TRUE)
lines(density(tempo3), 
      col = "red",              # Cor da curva de densidade
      lwd = 2)                  # Espessura da linha da curva
shapiro.test(tempo3)





