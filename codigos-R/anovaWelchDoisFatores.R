# IMPORTAR DATABASE
library(readr)
musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas_normalizadas.csv")
View(musicas)

# FILTRANDO OS DADOS NÃO-PARAMETRIZÁVEIS
musicasFiltrado <- musicas[musicas$country!="NG" & musicas$country!="IN" & musicas$country!="US" & musicas$country!="MX" & musicas$country!="NZ" & musicas$country!="DE",]

# AGRUPAR OS VALORES PARA TEMPO
musicasFiltrado$tempo[musicasFiltrado$tempo<=0.33] <- 0.33
musicasFiltrado$tempo[musicasFiltrado$tempo>0.33 & musicasFiltrado$tempo<=0.67] <- 0.85
musicasFiltrado$tempo[musicasFiltrado$tempo>0.67 & musicasFiltrado$tempo<=1] <- 1

# AGRUPAR OS VALORES PARA VALÊNCIA
musicasFiltrado$valence[musicasFiltrado$valence<=0.33] <- 0.33
musicasFiltrado$valence[musicasFiltrado$valence>0.33 & musicasFiltrado$valence<=0.85] <- 0.85
musicasFiltrado$valence[musicasFiltrado$valence>0.85 & musicasFiltrado$valence<=1] <- 1

# DIVIDIR DATABASE EM DUAS (TESTE CRUZADO SUGERIDO PELO PROFESSOR)
n <- 600 # number of original database
select <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.5, 0.5))
musicas1 <- musicasFiltrado[select, ]
musicas2 <- musicasFiltrado[!select, ]

# ANOVA PARA MUSICAS (FULL SAMPLE)
anovaWelchMusicasFull <- oneway.test(danceability ~ tempo + country, data = musicasFiltrado)
summary(anovaMusicasFull)

