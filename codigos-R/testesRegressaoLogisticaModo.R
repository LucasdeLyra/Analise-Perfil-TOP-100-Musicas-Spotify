library(readr)
library(ResourceSelection)
library(lmtest)
library(DescTools)
library(pscl)

musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas.csv")

modrl <- glm(mode ~ danceability + tempo + key, data = musicas, family = binomial)
summary(modrl)

# HOSMER-LEMESHOW
hl <- hoslem.test(musicas$mode, fitted(modrl), g = 10)
hl
# WALD
wald <- waldtest(modrl, terms=c("danceability", "tempo", "key"))
print(wald)
wald <- waldtest(modrl, terms=c("danceability", "tempo"))
print(wald)
wald <- waldtest(modrl, terms=c("danceability", "key"))
print(wald)
wald <- waldtest(modrl, terms=c("tempo", "key"))
print(wald)
wald <- waldtest(modrl, terms=c("danceability"))
print(wald)
wald <- waldtest(modrl, terms=c("tempo"))
print(wald)
wald <- waldtest(modrl, terms=c("key"))
print(wald)



# COX-SNELL
PseudoR2(modrl, which = "CoxSnell")

# NAGELKERKE
PseudoR2(modrl, which = "Nagelkerke")

# MATRIZ DE CONFUSÃO E ACURÁCIA
# Fazendo previsões
previsoes <- round(predict(modrl, musicas, type = "response"))

# Criando a matriz de confusão
matconf <- table(musicas$mode, previsoes)

# Calculando a acurácia
acuracia <- sum(diag(matconf)) / sum(matconf)
acuracia

###LL:
modelo_nulo <- glm(mode ~ 1, data = musicas, family = binomial)

llh <- logLik(modrl)           # Log-likelihood do modelo ajustado
llh
llh_nulo <- logLik(modelo_nulo) # Log-likelihood do modelo nulo
llh_nulo


###R² logit:
R2_logit <- (-2 * as.numeric(llh_nulo) - (-2 * as.numeric(llh))) / (-2 * as.numeric(llh_nulo))
print(R2_logit) 

###pseudo-R² & MacFadden:
# MCFADDEN
resultado <- pR2(modrl)
resultado

