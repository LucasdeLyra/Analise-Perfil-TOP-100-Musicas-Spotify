# Exemplo AFC - Extracao fatores comuns
library(readr)
library(MVA)
library(psych)
library(car)

musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas.csv")
View(musicas)

# SOBRE OS PRESSUPOSTOS:
# NORMALIDADE DOS DADOS CHECAMOS NO BARTLETT
hist(musicas$loudness)
hist(musicas$acousticness)
hist(musicas$danceability)
hist(musicas$energy)
hist(musicas$instrumentalness)
hist(musicas$liveness)
hist(musicas$speechiness)
hist(musicas$tempo)
hist(musicas$valence)

shapiro.test(musicas$loudness)
shapiro.test(musicas$acousticness)
shapiro.test(musicas$danceability)
shapiro.test(musicas$energy)
shapiro.test(musicas$instrumentalness)
shapiro.test(musicas$liveness)
shapiro.test(musicas$speechiness)
shapiro.test(musicas$tempo)
shapiro.test(musicas$valence)

  # NENHUMA DAS VARIAVEIS ESTA NORMAL :)
  # VAMOS TENTAR NORMALIZAR?

# VARIAVEIS QUANTITAVAS CONTINUAS
# PELO MENOS 4 A 5 VEZES MAIS DADOS QUE VARIAVEIS
  # TEMOS INICIALMENTE 9 VARIAVEIS E 1200 DADOS -> SUFICIENTE

cor(musicas[,c(6:10, 12, 14:15, 17)]) # exame da matriz correlacao

cortest.bartlett(musicas[,c(6:10, 12, 14:15, 17)]) # teste Bartlett

KMO(musicas[,c(6:10, 12, 14:15, 17)]) # teste KMO

# UTILIZANDO TODAS AS VARIAVEIS DISPONÍVEIS
scree(musicas[,c(6:10, 12, 14:15, 17)]) # scree-plot

famus1<-factanal(musicas[,c(6:10, 12, 14:15, 17)], factors = 3, rotation = "varimax") # chamada AFC para 3 fatores

print(famus1) # sumario estatistico do modelo construido

plot(famus1$loadings[,1:2]) # plot das cargas fatoriais para os 2 primeiros fatores

# REMOVENDO AS VARIAVEIS INADEQUADAS SEGUNDO O KMO (VALOR MENOR QUE 0.5)
# LIVENESS (12) E SPEECHINESS (14)
cortest.bartlett(musicas[,c(6:10, 15, 17)]) # teste Bartlett

KMO(musicas[,c(6:10, 15, 17)]) # teste KMO

scree(musicas[,c(6:10, 15, 17)]) # scree-plot

famus2<-factanal(musicas[,c(6:10, 15, 17)], factors = 3, rotation = "varimax") # chamada AFC para 3 fatores

print(famus2) # sumario estatistico do modelo construido

plot(famus2$loadings[,1:2]) # plot das cargas fatoriais para os 2 primeiros fatores

# REMOVENDO AS VARIAVEIS MARGINALMENTE ADEQUADAS SEGUNDO O KMO (VALOR MENOR QUE 0.55)
# DANCEABILITY (8) E INSTRUMENTALNESS (10)
cortest.bartlett(musicas[,c(6:7, 9, 15, 17)]) # teste Bartlett

KMO(musicas[,c(6:7, 9, 15, 17)]) # teste KMO

scree(musicas[,c(6:7, 9, 15, 17)]) # scree-plot

famus3<-factanal(musicas[,c(6:7, 9, 15, 17)], factors = 2, rotation = "varimax") # chamada AFC para 3 fatores

print(famus3) # sumário estatistico do modelo construido

plot(famus3$loadings[,1:2]) # plot das cargas fatoriais para os 2 primeiros fatores

# O MELHOR FIT FOI O MODELO 2, LOGO SEGUIREMOS ANALISANDO O MODELO COMPLETO
# E O SEGUNDO MODELO

## PRIMEIRO MODELO (COMPLETO)



