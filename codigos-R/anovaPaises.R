# IMPORTANDO A DATABASE
library(readr)
musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas_normalizadas.csv")
View(musicas)

# FILTRANDO OS NÃO-PARAMETRIZÁVEIS
musicasFiltrado <- musicas[musicas$country!="NG",]

# FAZENDO A ANOVA DE UM FATOR
anovaMusicasPaises <- aov(danceability ~ country, data = musicasFiltrado)
summary(anovaMusicasPaises)

# DIVIDIR DATABASE EM DUAS (TESTE CRUZADO SUGERIDO PELO PROFESSOR)
n <- 1100 # number of original database
select <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.5, 0.5))
musicas1 <- musicasFiltrado[select, ]
musicas2 <- musicasFiltrado[!select, ]

# ANOVA PARA MUSICAS1 (SAMPLE 1)
anovaMusicas1 <- aov(danceability ~ country, data = musicas1)
summary(anovaMusicas1)

# ANOVA PARA MUSICAS2 (SAMPLE 2)
anovaMusicas2 <- aov(danceability ~ country, data = musicas2)
summary(anovaMusicas2)
