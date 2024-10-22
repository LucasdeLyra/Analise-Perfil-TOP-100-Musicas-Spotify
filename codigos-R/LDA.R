#instalar e carregar o pacote
#install.packages("MVN")
#install.packages("readr")
#install.packages("bestNormalize")
#install.packages("corrplot")
#install.packages("rstatix")
#install.packages("car")
#install.packages("dplyr")
#install.packages("QuantPsyc")
#install.packages("energy")

library(energy)
library(QuantPsyc)
library(car)    
library(dplyr)   
library(rstatix)
library(corrplot)
library(MVN)
library(readr)
library(bestNormalize)

#função para transformar outliers em NA (https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset)
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

#colunas numéricas
colunas <- c("continent","loudness", "acousticness", "danceability", "energy", "liveness", "speechiness", "tempo", "valence")
numericos <- c("loudness", "acousticness", "danceability", "energy", "liveness", "speechiness", "tempo", "valence")

#----------------
musicas <- read_csv("./My Games/Spotify_MQAM/tabelas/musicas.csv")
colunas_selecionadas <- musicas[colunas]

plot(colunas_selecionadas)
#aplica função
for (i in colnames(colunas_selecionadas)){
  if (i != "continent"){
    colunas_selecionadas[i] <- remove_outliers(colunas_selecionadas[[i]])
  }
  
}
colunas_selecionadas
#remove todas as linhas com valors NA
musicas_sem_outliers <- colunas_selecionadas[complete.cases(colunas_selecionadas), ]
plot(musicas_sem_outliers)

for (i in colnames(musicas_sem_outliers)){
  if (i != "continent"){
    musicas_sem_outliers[i] <- predict(bestNormalize(musicas_sem_outliers[[i]]))
  }
}

plot(musicas_sem_outliers)
print(musicas_sem_outliers, n=661)

musicas_sem_outliers$continent[musicas_sem_outliers$continent=="SA"] <- 0
musicas_sem_outliers$continent[musicas_sem_outliers$continent=="OC"] <- 1
musicas_sem_outliers$continent[musicas_sem_outliers$continent=="NA"] <- 2
musicas_sem_outliers$continent[musicas_sem_outliers$continent=="EU"] <- 3
musicas_sem_outliers$continent[musicas_sem_outliers$continent=="AS"] <- 4
musicas_sem_outliers$continent[musicas_sem_outliers$continent=="AF"] <- 5

vif_values <- vif(lm(continent~., data=musicas_sem_outliers))
vif_values

for (i in colnames(musicas_sem_outliers)){
  if (i != "continent"){
    print(i)
    print(box_m(musicas_sem_outliers[,i], musicas_sem_outliers$continent))
  }
}


musicas_sem_outliers_somente_numericos = musicas_sem_outliers[numericos]
res <- cor(musicas_sem_outliers_somente_numericos, method = "pearson")

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

cov(musicas_sem_outliers_somente_numericos)

hz_test <- mvn(musicas_sem_outliers_somente_numericos, mvnTest = "hz")
print(hz_test)

mardia <- mult.norm(musicas_sem_outliers_somente_numericos)
print(mardia)

energy_m <- mvnorm.etest(musicas_sem_outliers_somente_numericos, R= 100)
print(energy_m)