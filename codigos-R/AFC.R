# Exemplo AFC - Extracao fatores comuns
library(bestNormalize)
library(readr)
library(MVA)
library(psych)
library(car)

musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas.csv")
musicasNorm <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas.csv")
View(musicas)

# SOBRE OS PRESSUPOSTOS:
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




# TESTES SEM NORMALIDADE ####################################################################
# VARIAVEIS QUANTITAVAS CONTINUAS já normalizadas 
# PELO MENOS 4 A 5 VEZES MAIS DADOS QUE VARIAVEIS
# TEMOS INICIALMENTE 9 VARIAVEIS E 1200 DADOS -> SUFICIENTE 

cor(musicas[,c(6:10, 12, 14:15, 17)]) # exame da matriz correlacao

cortest.bartlett(musicas[,c(6:10, 12, 14:15, 17)]) # teste Bartlett

KMO(musicas[,c(6:10, 12, 14:15, 17)]) # teste KMO

# UTILIZANDO TODAS AS VARIAVEIS DISPONÍVEIS
scree(musicas[,c(6:10, 12, 14:15, 17)]) # scree-plot

#sem rotação:
famus1_semRot<-factanal(musicas[,c(6:10, 12, 14:15, 17)], factors = 3, rotation = "none") # chamada AFC para 3 fatores
print(famus1_semRot) # sumario estatistico do modelo construido
plot(famus1_semRot$loadings[,1:2]) # plot das cargas fatoriais para os fatores 1 e 2
plot(famus1_semRot$loadings[,c(1,3)]) # plot das cargas fatoriais para os fatores 1 e 3
plot(famus1_semRot$loadings[,2:3]) # plot das cargas fatoriais para os fatores 2 e 3
#com rotação:
famus1<-factanal(musicas[,c(6:10, 12, 14:15, 17)], factors = 3, rotation = "varimax") # chamada AFC para 3 fatores
print(famus1) # sumario estatistico do modelo construido
plot(famus1$loadings[,1:2]) # plot das cargas fatoriais para os fatores 1 e 2
plot(famus1$loadings[,c(1,3)]) # plot das cargas fatoriais para os fatores 1 e 3
plot(famus1$loadings[,2:3]) # plot das cargas fatoriais para os fatores 2 e 3

#experimentando combinações, percebemos que com a remoção de instrumentalness o modelo parece se ajustar melhor
cor(musicas[,c(6:9, 12, 14:15, 17)]) # exame da matriz correlacao

cortest.bartlett(musicas[,c(6:9, 12, 14:15, 17)]) # teste Bartlett

KMO(musicas[,c(6:9, 12, 14:15, 17)]) # teste KMO

# UTILIZANDO TODAS AS VARIAVEIS NORMAIS DISPONÍVEIS (REMOVE INSTRUMENTALNESS [10])
scree(musicas[,c(6:9, 12, 14:15, 17)]) # scree-plot

#sem rotação:
famus1_semRot<-factanal(musicas[,c(6:9, 12, 14:15, 17)], factors = 3, rotation = "none") # chamada AFC para 3 fatores
print(famus1_semRot) # sumario estatistico do modelo construido
plot(famus1_semRot$loadings[,1:2]) # plot das cargas fatoriais para os fatores 1 e 2
plot(famus1_semRot$loadings[,c(1,3)]) # plot das cargas fatoriais para os fatores 1 e 3
plot(famus1_semRot$loadings[,2:3]) # plot das cargas fatoriais para os fatores 2 e 3
#com rotação:
famus1<-factanal(musicas[,c(6:9, 12, 14:15, 17)], factors = 3, rotation = "varimax") # chamada AFC para 3 fatores
print(famus1) # sumario estatistico do modelo construido
plot(famus1$loadings[,1:2]) # plot das cargas fatoriais para os fatores 1 e 2
plot(famus1$loadings[,c(1,3)]) # plot das cargas fatoriais para os fatores 1 e 3
plot(famus1$loadings[,2:3]) # plot das cargas fatoriais para os fatores 2 e 3




# REMOVENDO AS VARIAVEIS INADEQUADAS SEGUNDO O KMO (VALOR MENOR QUE 0.5)
# LIVENESS (12) E SPEECHINESS (14)
cortest.bartlett(musicas[,c(6:10, 15, 17)]) # teste Bartlett

KMO(musicas[,c(6:10, 15, 17)]) # teste KMO

scree(musicas[,c(6:10, 15, 17)]) # scree-plot

#linhas adicionadas para comparaçaõ netre análise fatorial com e sem rotação
famus2_semRot<-factanal(musicas[,c(6:10, 15, 17)], factors = 3, rotation = "none") # chamada AFC para 3 fatores
print(famus2_semRot) # sumario estatistico do modelo construido
plot(famus2_semRot$loadings[,1:2]) # plot das cargas fatoriais para os fatores 1 e 2
plot(famus2_semRot$loadings[,c(1,3)]) # plot das cargas fatoriais para os fatores 1 e 3
plot(famus2_semRot$loadings[,2:3]) # plot das cargas fatoriais para os fatores 2 e 3
#com rotação:
famus2<-factanal(musicas[,c(6:10, 15, 17)], factors = 3, rotation = "varimax") # chamada AFC para 3 fatores
print(famus2) # sumario estatistico do modelo construido
plot(famus2$loadings[,1:2]) # plot das cargas fatoriais para os fatores 1 e 2
plot(famus2$loadings[,c(1,3)]) # plot das cargas fatoriais para os fatores 1 e 3
plot(famus2$loadings[,2:3]) # plot das cargas fatoriais para os fatores 2 e 3

#sem instrumentalness:
cortest.bartlett(musicas[,c(6:9, 15, 17)]) # teste Bartlett

KMO(musicas[,c(6:9, 15, 17)]) # teste KMO

scree(musicas[,c(6:9, 15, 17)]) # scree-plot

#linhas adicionadas para comparaçaõ netre análise fatorial com e sem rotação
famus2_semRot<-factanal(musicas[,c(6:9, 15, 17)], factors = 3, rotation = "none") # chamada AFC para 3 fatores
print(famus2_semRot) # sumario estatistico do modelo construido
plot(famus2_semRot$loadings[,1:2]) # plot das cargas fatoriais para os fatores 1 e 2
plot(famus2_semRot$loadings[,c(1,3)]) # plot das cargas fatoriais para os fatores 1 e 3
plot(famus2_semRot$loadings[,2:3]) # plot das cargas fatoriais para os fatores 2 e 3
#com rotação:
famus2<-factanal(musicas[,c(6:9, 15, 17)], factors = 3, rotation = "varimax") # chamada AFC para 3 fatores
print(famus2) # sumario estatistico do modelo construido
plot(famus2$loadings[,1:2]) # plot das cargas fatoriais para os fatores 1 e 2
plot(famus2$loadings[,c(1,3)]) # plot das cargas fatoriais para os fatores 1 e 3
plot(famus2$loadings[,2:3]) # plot das cargas fatoriais para os fatores 2 e 3




# REMOVENDO AS VARIAVEIS MARGINALMENTE ADEQUADAS SEGUNDO O KMO (VALOR MENOR QUE 0.55)
# DANCEABILITY (8) E INSTRUMENTALNESS (10)
cortest.bartlett(musicas[,c(6:7, 9, 15, 17)]) # teste Bartlett

KMO(musicas[,c(6:7, 9, 15, 17)]) # teste KMO

scree(musicas[,c(6:7, 9, 15, 17)]) # scree-plot
#linhas adicionadas para comparaçaõ netre análise fatorial com e sem rotação
famus3_semRot<-factanal(musicas[,c(6:7, 9, 15, 17)], factors = 2, rotation = "none") # chamada AFC para 3 fatores
print(famus3_semRot)
plot(famus3_semRot$loadings[,1:2]) # plot das cargas fatoriais para os fatores 1 e 2
plot(famus3_semRot$loadings[,c(1,3)]) # plot das cargas fatoriais para os fatores 1 e 3
plot(famus3_semRot$loadings[,2:3]) # plot das cargas fatoriais para os fatores 2 e 3
#com rotação:
famus3<-factanal(musicas[,c(6:7, 9, 15, 17)], factors = 2, rotation = "varimax") # chamada AFC para 3 fatores
print(famus3) # sumário estatistico do modelo construido
plot(famus3$loadings[,1:2]) # plot das cargas fatoriais para os fatores 1 e 2
plot(famus3$loadings[,c(1,3)]) # plot das cargas fatoriais para os fatores 1 e 3
plot(famus3$loadings[,2:3]) # plot das cargas fatoriais para os fatores 2 e 3

# O MELHOR FIT FOI O MODELO 2, LOGO SEGUIREMOS ANALISANDO O MODELO COMPLETO
# E O SEGUNDO MODELO






# TESTES COM NORMALIDADE #################################################################
# VAMOS TENTAR NORMALIZAR? -->vamos!
bestnorm_obj <- bestNormalize(musicasNorm$loudness)
shapiro.test(bestnorm_obj$x.t)
musicasNorm$loudness <- bestnorm_obj$x.t

bestnorm_obj <- bestNormalize(musicasNorm$acousticness)
shapiro.test(bestnorm_obj$x.t)
musicasNorm$acousticness <- bestnorm_obj$x.t

bestnorm_obj <- bestNormalize(musicasNorm$danceability)
shapiro.test(bestnorm_obj$x.t)
musicasNorm$danceability <- bestnorm_obj$x.t

bestnorm_obj <- bestNormalize(musicasNorm$energy)
shapiro.test(bestnorm_obj$x.t)
musicasNorm$energy <- bestnorm_obj$x.t

#instrumentalness é "inormalizável" :(
bestnorm_obj <- bestNormalize(musicasNorm$instrumentalness)
shapiro.test(bestnorm_obj$x.t)
musicasNorm$instrumentalness <- bestnorm_obj$x.t

bestnorm_obj <- bestNormalize(musicasNorm$liveness)
shapiro.test(bestnorm_obj$x.t)
musicasNorm$liveness <- bestnorm_obj$x.t

bestnorm_obj <- bestNormalize(musicasNorm$speechiness)
shapiro.test(bestnorm_obj$x.t)
musicasNorm$speechiness <- bestnorm_obj$x.t

bestnorm_obj <- bestNormalize(musicasNorm$tempo)
shapiro.test(bestnorm_obj$x.t)
musicasNorm$tempo <- bestnorm_obj$x.t

bestnorm_obj <- bestNormalize(musicasNorm$valence)
shapiro.test(bestnorm_obj$x.t)
musicasNorm$valence <- bestnorm_obj$x.t


# VARIAVEIS QUANTITAVAS CONTINUAS NORMALIZADAS 
# PELO MENOS 4 A 5 VEZES MAIS DADOS QUE VARIAVEIS
# TEMOS INICIALMENTE 9 VARIAVEIS E 1200 DADOS -> SUFICIENTE 

cor(musicasNorm[,c(6:10, 12, 14:15, 17)]) # exame da matriz correlacao

cortest.bartlett(musicasNorm[,c(6:10, 12, 14:15, 17)]) # teste Bartlett

KMO(musicasNorm[,c(6:10, 12, 14:15, 17)]) # teste KMO

# UTILIZANDO TODAS AS VARIAVEIS DISPONÍVEIS
scree(musicasNorm[,c(6:10, 12, 14:15, 17)]) # scree-plot

#sem rotação:
famusnorm1_semRot<-factanal(musicasNorm[,c(6:10, 12, 14:15, 17)], factors = 3, rotation = "none") # chamada AFC para 3 fatores
print(famusnorm1_semRot) # sumario estatistico do modelo construido
plot(famusnorm1_semRot$loadings[,1:2]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm1_semRot$loadings[,c(1,3)]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm1_semRot$loadings[,2:3]) # plot das cargas fatoriais para os 2 primeiros fatores
#com rotação:
famusnorm1<-factanal(musicasNorm[,c(6:10, 12, 14:15, 17)], factors = 3, rotation = "varimax") # chamada AFC para 3 fatores
print(famusnorm1) # sumario estatistico do modelo construido
plot(famusnorm1$loadings[,1:2]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm1$loadings[,c(1,3)]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm1$loadings[,2:3]) # plot das cargas fatoriais para os 2 primeiros fatores

#experimentando combinações, percebemos que com a remoção de instrumentalness o modelo parece se ajustar melhor
cor(musicasNorm[,c(6:9, 12, 14:15, 17)]) # exame da matriz correlacao

cortest.bartlett(musicasNorm[,c(6:9, 12, 14:15, 17)]) # teste Bartlett

KMO(musicasNorm[,c(6:9, 12, 14:15, 17)]) # teste KMO

# UTILIZANDO TODAS AS VARIAVEIS NORMAIS DISPONÍVEIS (REMOVE INSTRUMENTALNESS [10])
scree(musicasNorm[,c(6:9, 12, 14:15, 17)]) # scree-plot

#sem rotação:
famusnorm1_semRot<-factanal(musicasNorm[,c(6:9, 12, 14:15, 17)], factors = 3, rotation = "none") # chamada AFC para 3 fatores
print(famusnorm1_semRot) # sumario estatistico do modelo construido
plot(famusnorm1_semRot$loadings[,1:2]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm1_semRot$loadings[,c(1,3)]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm1_semRot$loadings[,2:3]) # plot das cargas fatoriais para os 2 primeiros fatores
#com rotação:
famusnorm1<-factanal(musicasNorm[,c(6:9, 12, 14:15, 17)], factors = 3, rotation = "varimax") # chamada AFC para 3 fatores
print(famusnorm1) # sumario estatistico do modelo construido
plot(famusnorm1$loadings[,1:2]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm1$loadings[,c(1,3)]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm1$loadings[,2:3]) # plot das cargas fatoriais para os 2 primeiros fatores




# REMOVENDO AS VARIAVEIS INADEQUADAS SEGUNDO O KMO (VALOR MENOR QUE 0.5)
# SPEECHINESS (14)
cortest.bartlett(musicasNorm[,c(6:10, 12, 15, 17)]) # teste Bartlett

KMO(musicasNorm[,c(6:10, 12, 15, 17)]) # teste KMO

scree(musicasNorm[,c(6:10, 12, 15, 17)]) # scree-plot

#linhas adicionadas para comparaçaõ netre análise fatorial com e sem rotação
famusnorm2_semRot<-factanal(musicasNorm[,c(6:10, 12, 15, 17)], factors = 3, rotation = "none") # chamada AFC para 3 fatores
print(famusnorm2_semRot) # sumario estatistico do modelo construido
plot(famusnorm2_semRot$loadings[,1:2]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm2_semRot$loadings[,c(1,3)]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm2_semRot$loadings[,2:3]) # plot das cargas fatoriais para os 2 primeiros fatores
#com rotação:
famusnorm2<-factanal(musicasNorm[,c(6:10, 12, 15, 17)], factors = 3, rotation = "varimax") # chamada AFC para 3 fatores
print(famusnorm2) # sumario estatistico do modelo construido
plot(famusnorm2$loadings[,1:2]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm2$loadings[,c(1,3)]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm2$loadings[,2:3]) # plot das cargas fatoriais para os 2 primeiros fatores

#sem instrumentalness:
cortest.bartlett(musicasNorm[,c(6:9, 12, 15, 17)]) # teste Bartlett

KMO(musicasNorm[,c(6:9, 12, 15, 17)]) # teste KMO

scree(musicasNorm[,c(6:9, 12, 15, 17)]) # scree-plot

#linhas adicionadas para comparaçaõ netre análise fatorial com e sem rotação
famusnorm2_semRot<-factanal(musicasNorm[,c(6:9, 12, 15, 17)], factors = 3, rotation = "none") # chamada AFC para 3 fatores
print(famusnorm2_semRot) # sumario estatistico do modelo construido
plot(famusnorm2_semRot$loadings[,1:2]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm2_semRot$loadings[,c(1,3)]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm2_semRot$loadings[,2:3]) # plot das cargas fatoriais para os 2 primeiros fatores
#com rotação:
famusnorm2<-factanal(musicasNorm[,c(6:9, 12, 15, 17)], factors = 3, rotation = "varimax") # chamada AFC para 3 fatores
print(famusnorm2) # sumario estatistico do modelo construido
plot(famusnorm2$loadings[,1:2]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm2$loadings[,c(1,3)]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm2$loadings[,2:3]) # plot das cargas fatoriais para os 2 primeiros fatores




# REMOVENDO AS VARIAVEIS MARGINALMENTE ADEQUADAS SEGUNDO O KMO (VALOR MENOR QUE 0.55)
cortest.bartlett(musicasNorm[,c(6:7, 9, 15)]) # teste Bartlett

KMO(musicasNorm[,c(6:7, 9, 15)]) # teste KMO

scree(musicasNorm[,c(6:7, 9, 15)]) # scree-plot
#linhas adicionadas para comparação netre análise fatorial com e sem rotação
famusnorm3_semRot<-factanal(musicasNorm[,c(6:7, 9, 15)], factors = 1, rotation = "none") # chamada AFC para 3 fatores
print(famusnorm3_semRot)
plot(famusnorm3_semRot$loadings[,1:2]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm3_semRot$loadings[,c(1,3)]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm3_semRot$loadings[,2:3]) # plot das cargas fatoriais para os 2 primeiros fatores
#com rotação:
famusnorm3<-factanal(musicasNorm[,c(6, 7, 9, 15)], factors = 1, rotation = "varimax") # chamada AFC para 3 fatores
print(famusnorm3) # sumário estatistico do modelo construido
plot(famusnorm3$loadings[,1:2]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm3$loadings[,c(1,3)]) # plot das cargas fatoriais para os 2 primeiros fatores
plot(famusnorm3$loadings[,2:3]) # plot das cargas fatoriais para os 2 primeiros fatores


