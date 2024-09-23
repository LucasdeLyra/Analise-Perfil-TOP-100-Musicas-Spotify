# IMPORTAR DATABASE
library(readr)
musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas_normalizadas.csv")
View(musicas)

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

