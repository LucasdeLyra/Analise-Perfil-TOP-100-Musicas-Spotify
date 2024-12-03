library(readr)
library(dplyr)

musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas.csv")

summary(musicas)

# MODELO SENDO AVALIADO
modrl <- glm(mode ~ valence + danceability + energy + tempo + key + time_signature, data = musicas, family = binomial)

# LOG-LIKELIHOOD
# Obter a verossimilhança logarítmica do modelo
log_likelihood <- logLik(modrl)
print(log_likelihood)

# Criar o modelo nulo
modnull <- glm(mode ~ 1, data = musicas, family = binomial)

# calcular o R^2 logit
