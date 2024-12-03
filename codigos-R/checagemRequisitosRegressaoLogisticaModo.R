library(readr)
library(dplyr)
library(lmtest)
library(car)
library(tidyverse)
library(tidyquant)
library(corrplot)
library(ggplot2)

musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas.csv")

modrl <- glm(mode ~ danceability + tempo + key, data = musicas, family = binomial)

# Valor esperado do erro = 0
residuos <- residuals(modrl, type="deviance")
mean(residuos)
## passou!!

# Inexistência de autocorrelação entre erros; e entre estes e as variáveis independentes
# Usando teste de Durbin-Watson
dw_test <- dwtest(modrl)
print(dw_test)
# Utilizando teste de Breusch-Godfrey
bg_test <- bgtest(modrl)
print(bg_test)
# Autocorrelação dos resíduos
acf(residuos, main = "Função de Autocorrelação dos Resíduos")
# Plot
plot(modrl$fitted.values, residuos)
abline(h = 0, col="red")
## passou!!

# Ausência de multicolinearidade perfeita entre as variáveis independentes
# Calcular VIF
vif_values <- vif(modrl)
print(vif_values) # esperado que não sejam maiores que 10
# Verificar se algum VIF é maior que 10
high_vif <- vif_values[vif_values > 10]
print(high_vif) # esperado que fale que nn tem
## passou!!