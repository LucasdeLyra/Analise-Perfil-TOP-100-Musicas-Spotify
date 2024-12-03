library(readr)
library(ggplot2)
library(pROC)

musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas.csv")

modrl <- glm(mode ~ danceability + tempo + key, data = musicas, family = binomial)

# AJUSTES DO MODELO
# Obter as previsões
musicas$predicted_probs <- predict(modrl, type = "response")

# Gráfico para 'danceability'
ggplot(musicas, aes(x = danceability, y = predicted_probs)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Probabilidade Prevista de 'mode' vs. danceability",
       x = "Danceability",
       y = "Probabilidade Prevista")

# Gráfico para 'tempo'
ggplot(musicas, aes(x = tempo, y = predicted_probs)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Probabilidade Prevista de 'mode' vs. tempo",
       x = "Tempo",
       y = "Probabilidade Prevista")

# Gráfico para 'key'
ggplot(musicas, aes(x = key, y = predicted_probs)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Probabilidade Prevista de 'mode' vs. key",
       x = "Key",
       y = "Probabilidade Prevista")

# Criar a curva ROC
roc_curve <- roc(musicas$mode, musicas$predicted_probs)
plot(roc_curve, main = "Curva ROC")
print(roc_curve$auc)

