# IMPORTAR DATABASE
library(readr)
musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas_normalizadas.csv")
View(musicas)

# AGRUPAR OS VALORES PARA TEMPO
musicas$tempo[musicas$tempo<0.1] <- 0
musicas$tempo[musicas$tempo>=0.1 & musicas$tempo<0.2] <- 0.1
musicas$tempo[musicas$tempo>=0.2 & musicas$tempo<0.3] <- 0.2
musicas$tempo[musicas$tempo>=0.3 & musicas$tempo<0.4] <- 0.3
musicas$tempo[musicas$tempo>=0.4 & musicas$tempo<0.5] <- 0.4
musicas$tempo[musicas$tempo>=0.5 & musicas$tempo<0.6] <- 0.5
musicas$tempo[musicas$tempo>=0.6 & musicas$tempo<0.7] <- 0.6
musicas$tempo[musicas$tempo>=0.7 & musicas$tempo<0.8] <- 0.7
musicas$tempo[musicas$tempo>=0.8 & musicas$tempo<0.9] <- 0.8
musicas$tempo[musicas$tempo>=0.9 & musicas$tempo<1] <- 0.9
# musicas$tempo[musicas$tempo==1] <- 1 já está feito

# DIVIDIR DATABASE EM DUAS (TESTE CRUZADO SUGERIDO PELO PROFESSOR)
n <- 1200 # number of original database
select <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.5, 0.5))
musicas1 <- musicas[select, ]
musicas2 <- musicas[!select, ]

# ANOVA PARA MUSICAS1 (SAMPLE 1)
# utilizamos ANOVA de dois fatores, pois temos duas variaveis independentes
# e uma variavel dependende
# independentes: tempo, pais
# dependente: dancabilidade

# para modificar variaveis:
# dependente fica antes do '~'
# independendetes ficam depois, separadas por '+'
anovaMusicas1 <- aov(danceability ~ tempo + country, data = musicas1)
summary(anovaMusicas1)

# ANOVA PARA MUSICAS2 (SAMPLE 2)
anovaMusicas2 <- aov(danceability ~ tempo + country, data = musicas2)
summary(anovaMusicas2)

# ANOVA PARA MUSICAS (FULL SAMPLE)
anovaMusicasFull <- aov(danceability ~ tempo + country, data = musicas)
summary(anovaMusicasFull)

