library(ggplot2)
library(gridExtra)
library(gtable)
library(ggpubr)

# Carregando o dataset
musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas.csv")

# Construção do modelo de regressão
modrl <- glm(mode ~ danceability + tempo + key, data = musicas, family = binomial)

# Mode predito
prev.musicas <- predict(modrl, musicas, type="response")

# Plot mode x mode predito
plot(prev.musicas,musicas$mode)

# Plot mode predito x Rnovo
newdata=data.frame(Rnovo=seq(min(musicas$danceability), max(musicas$danceability), len=1200))
newdata$modePred=predict(modrl, musicas, type="response")
str(newdata)
plot(sort(modePred) ~ Rnovo, newdata)

par(mfcol=c(2,2))
# Plot de mode x mode predito com o modelo completo
plot(musicas$mode,prev.musicas, col="steelblue")

# Plot de danceability x mode predito com o modelo completo
plot(musicas$danceability,prev.musicas, col="steelblue")

# Plot de tempo x mode predito com o modelo completo
plot(musicas$tempo,prev.musicas, col="steelblue")

# Plot de key x mode predito com o modelo completo
plot(musicas$key,prev.musicas, col="steelblue")

par(mfcol=c(2,2))
# Plot de fitted
plot(musicas$mode,fitted(modrl))
plot(musicas$mode,fitted(glm(mode~danceability,musicas,family=binomial)))
plot(musicas$mode,fitted(glm(mode~tempo,musicas,family=binomial)))
plot(musicas$mode,fitted(glm(mode~key,musicas,family=binomial)))

# Curva do modelo completo
modComp <- ggplot(musicas, aes(x=danceability+tempo+key, y=mode)) +
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE,
              method.args = list(family=binomial))

# Curva do modelo por variável
modDanc <- ggplot(musicas, aes(x=danceability, y=mode)) +
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE,
                method.args = list(family=binomial))

modTem <- ggplot(musicas, aes(x=tempo, y=mode)) +
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE,
              method.args = list(family=binomial))

modKey <- ggplot(musicas, aes(x=key, y=mode)) +
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE,
              method.args = list(family=binomial))

# Preparando a figura com 4 gráficos
figure <- ggarrange(modComp, modDanc, modTem, modKey,
                    ncol=2, nrow=2)
figure
