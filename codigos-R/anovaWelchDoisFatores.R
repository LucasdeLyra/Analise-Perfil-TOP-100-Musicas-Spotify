# IMPORTAR DATABASE
library(readr)
musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas_normalizadas.csv")
View(musicas)

# FILTRANDO OS DADOS NÃO-PARAMETRIZÁVEIS
musicasFiltrado <- musicas[musicas$country!="NG",]

# AGRUPAR OS VALORES PARA TEMPO
musicasFiltrado$tempo[musicasFiltrado$tempo<=0.33] <- 0.33
musicasFiltrado$tempo[musicasFiltrado$tempo>0.33 & musicasFiltrado$tempo<=0.67] <- 0.67
musicasFiltrado$tempo[musicasFiltrado$tempo>0.67 & musicasFiltrado$tempo<=1] <- 1
# musicas$tempo[musicas$tempo==1] <- 1 já está feito

# DIVIDIR DATABASE EM DUAS (TESTE CRUZADO SUGERIDO PELO PROFESSOR)
n <- 1100 # number of original database
select <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.5, 0.5))
musicas1 <- musicasFiltrado[select, ]
musicas2 <- musicasFiltrado[!select, ]

# ANOVA PARA MUSICAS (FULL SAMPLE)
# utilizamos ANOVA de dois fatores, pois temos duas variaveis independentes
# e uma variavel dependende
# independentes: tempo, pais
# dependente: dancabilidade

# para modificar variaveis:
# dependente fica antes do '~'
# independendetes ficam depois, separadas por '+'
anovaWelchMusicasFull <- oneway.test(danceability ~ tempo + country, data = musicasFiltrado)
summary(anovaMusicasFull)
