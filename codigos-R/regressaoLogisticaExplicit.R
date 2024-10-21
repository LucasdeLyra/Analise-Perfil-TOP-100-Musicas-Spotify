library(readr)
library(dplyr)

musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas.csv")

summary(musicas)

# REGRESSÃO DO EXPLICIT

# 1 a 1
modrl=glm(mode ~ rank, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ loudness, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ tempo, data = musicas, family = binomial)
summary(modrl)

# 2 a 2
modrl=glm(mode ~ rank + valence, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ rank + loudness, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ rank + tempo, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + loudness, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + tempo, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ loudness + tempo, data = musicas, family = binomial)
summary(modrl)

# 3 a 3
modrl=glm(mode ~ rank + valence + loudness, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ rank + valence + tempo, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ rank + loudness + tempo, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + loudness + tempo, data = musicas, family = binomial)
summary(modrl)

# 4 a 4
modrl=glm(mode ~ rank + valence + loudness + tempo, data = musicas, family = binomial)
summary(modrl)

# prev predict plot
prev.musicas <- predict(modrl, musicas, type="response") # predito
plot(prev.musicas, musicas$mode)

# não sei pra que isso serve exatamente mas tô tentando descobrir
newdata=data.frame(Rnovo=seq(min(musicas$mode), max(musicas$mode), len=1200))
newdata$pred.mode=predict(modrl, musicas, type="response")
str(newdata)
plot(sort(pred.mode) ~ Rnovo, newdata) 