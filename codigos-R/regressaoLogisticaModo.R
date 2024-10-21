library(readr)
library(dplyr)

musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas.csv")

summary(musicas)

# REGRESSÃO DO MODO

# 1 a 1
modrl=glm(mode ~ valence, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ energy, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ tempo, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ time_signature, data = musicas, family = binomial)
summary(modrl)

# 2 a 2
modrl=glm(mode ~ valence + danceability, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + energy, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + tempo, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability + energy, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability + tempo, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability + key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ energy + tempo, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ energy + key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ energy + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ tempo + key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ tempo + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ key + time_signature, data = musicas, family = binomial)
summary(modrl)

# 3 a 3
modrl=glm(mode ~ valence + danceability + energy, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + danceability + tempo, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + danceability + key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + danceability + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + energy + tempo, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + energy + key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + energy + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + tempo + key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + tempo + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + key + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability + energy + tempo, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability + energy + key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability + energy + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability + tempo + key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability + tempo + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability + key + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ energy + tempo + key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ energy + tempo + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ energy + key + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ tempo + key + time_signature, data = musicas, family = binomial)
summary(modrl)

# 4 a 4
modrl=glm(mode ~ valence + danceability + energy + tempo, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + danceability + energy + key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + danceability + energy + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + danceability + tempo + key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + danceability + tempo + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + danceability + key + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + energy + tempo + key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + energy + tempo + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + energy + key + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + tempo + key + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability + energy + tempo + key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability + energy + tempo + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability + energy + key + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability + tempo + key + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ energy + tempo + key + time_signature, data = musicas, family = binomial)
summary(modrl)

# 5 a 5
modrl=glm(mode ~ valence + danceability + energy + tempo + key, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + danceability + energy + tempo + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + danceability + energy + key + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + danceability + tempo + key + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ valence + energy + tempo + key + time_signature, data = musicas, family = binomial)
summary(modrl)

modrl=glm(mode ~ danceability + energy + tempo + key + time_signature, data = musicas, family = binomial)
summary(modrl)

# 6 a 6
modrl=glm(mode ~ valence + danceability + energy + tempo + key + time_signature, data = musicas, family = binomial)
summary(modrl)

# prev predict plot
prev.musicas <- predict(modrl, musicas, type="response") # predito
plot(prev.musicas, musicas$mode)

# não sei pra que isso serve exatamente mas tô tentando descobrir
newdata=data.frame(Rnovo=seq(min(musicas$mode), max(musicas$mode), len=1200)) # ele usa uma independente aqui???
newdata$pred.mode=predict(modrl, musicas, type="response")
str(newdata)
plot(sort(pred.mode) ~ Rnovo, newdata) 

# mais coisa que não sei pra que serve
plot(musicas$mode,prev.musicas, col="steelblue")
plot(musicas$mode,fitted(modrl))

plot(Rnovo$mode,prev.musicas, col="steelblue")
plot(musicas$mode,fitted(modrl))