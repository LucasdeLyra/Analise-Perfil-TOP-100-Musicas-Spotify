# IMPORTAR DATABASE
library(readr)
musicas <- read_csv("./MQAM/MQAM/tabelas/musicas_normalizadas.csv")
View(musicas)

# FILTRANDO OS DADOS NÃO-PARAMETRIZÁVEIS
musicasFiltrado <- musicas[musicas$country!="NG" & musicas$country!="IN" & musicas$country!="US" & musicas$country!="MX" & musicas$country!="NZ" & musicas$country!="DE",]

# AGRUPAR OS VALORES PARA TEMPO
musicasFiltrado$tempo[musicasFiltrado$tempo<=0.33] <- "0.33"
musicasFiltrado$tempo[musicasFiltrado$tempo>0.33 & musicasFiltrado$tempo<=0.67] <- "0.85"
musicasFiltrado$tempo[musicasFiltrado$tempo>0.67 & musicasFiltrado$tempo<=1] <- "1"

# AGRUPAR OS VALORES PARA VALÊNCIA
musicasFiltrado$valence[musicasFiltrado$valence<=0.33] <- "0.33"
musicasFiltrado$valence[musicasFiltrado$valence>0.33 & musicasFiltrado$valence<=0.85] <- "0.85"
musicasFiltrado$valence[musicasFiltrado$valence>0.85 & musicasFiltrado$valence<=1] <- "1"

# DIVIDIR DATABASE EM DUAS (TESTE CRUZADO SUGERIDO PELO PROFESSOR)
n <- 600 # number of original database
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
anovaMusicasFull <- aov(danceability ~ tempo + country + valence, data = musicasFiltrado)
summary(anovaMusicasFull)
# anovaMusicasFull$coefficients

# ANOVA PARA MUSICAS1 (SAMPLE 1)
anovaMusicas1 <- aov(danceability ~ tempo + country + valence, data = musicas1)
summary(anovaMusicas1)
# anovaMusicas1$coefficients

# ANOVA PARA MUSICAS2 (SAMPLE 2)
anovaMusicas2 <- aov(danceability ~ tempo + country + valence, data = musicas2)
summary(anovaMusicas2)
# anovaMusicas2$coefficients

par(mfcol=c(2,2))
plot(anovaMusicasFull)
par(mfcol=c(1,1))

library(car)
leveneTest(danceability ~ country, data = musicasFiltrado)
leveneTest(danceability ~ tempo, data = musicasFiltrado)
leveneTest(danceability ~ valence, data = musicasFiltrado)