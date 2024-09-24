library(readr)
library(moments)
library(bestNormalize)

musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas_normalizadas.csv")
View(musicas)

musicas = subset(musicas, select=-c(id, name, continent, rank, loudness, acousticness, energy, instrumentalness, key, liveness, mode, speechiness, tempo, time_signature, valence, explicit, artist_id, album_name, release_date))

musicasFiltrado <- musicas[musicas$country!="NG",]
View(musicasFiltrado)

musicasFiltrado$tempo[musicasFiltrado$tempo<=0.33] <- 0.33
musicasFiltrado$tempo[musicasFiltrado$tempo>0.33 & musicasFiltrado$tempo<=0.67] <- 0.67
musicasFiltrado$tempo[musicasFiltrado$tempo>0.67 & musicasFiltrado$tempo<=1] <- 1

tempo1 <- subset(musicasFiltrado, tempo==0.33)
tempo2 <- subset(musicasFiltrado, tempo==0.67)
tempo3 <- subset(musicasFiltrado, tempo==1)

shapiro.test(tempo1$danceability)
shapiro.test(tempo2$danceability)
shapiro.test(tempo3$danceability)

skewness(tempo1$danceability)
skewness(tempo2$danceability)
skewness(tempo3$danceability)

normalized <- bestNormalize(tempo1$danceability)
predicted <- predict(normalized)
hist(predicted)
skewness(predicted)
shapiro.test(predicted)

normalized <- bestNormalize(tempo2$danceability)
predicted <- predict(normalized)
hist(predicted)
skewness(predicted)
shapiro.test(predicted)

normalized <- bestNormalize(tempo3$danceability)
predicted <- predict(normalized)
hist(predicted)
skewness(predicted)
shapiro.test(predicted)