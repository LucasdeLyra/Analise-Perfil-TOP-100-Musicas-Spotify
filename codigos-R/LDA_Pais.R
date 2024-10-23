#instalar e carregar o pacote
#install.packages("MVN")
#install.packages("readr")
#install.packages("bestNormalize")
#install.packages("corrplot")
#install.packages("rstatix")
#install.packages("car")
#install.packages("dplyr")
#install.packages("MASS")
#install.packages("heplots")
library(heplots)
library(car)    
library(dplyr)   
library(rstatix)
library(corrplot)
library(MVN)
library(readr)
library(bestNormalize)
library(MASS)
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
colunas <- c("country","continent","loudness", "acousticness", "danceability", "energy", "liveness", "speechiness", "tempo", "valence")
numericos <- c("loudness", "acousticness", "danceability", "energy", "liveness", "speechiness", "tempo", "valence")

#----------------
musicas = read_csv("./My Games/Spotify_MQAM/tabelas/musicas.csv")
musicas$continent[is.na(musicas$continent)] <- "AN"

musicas_BR = head(musicas, 100)
musicas_AT = musicas[101:200, ]
musicas_CA = musicas[201:300, ]
musicas_DE <- musicas[301:400, ]
musicas_GB <- musicas[401:500, ]
musicas_IN <- musicas[501:600, ]
musicas_MX <- musicas[601:700, ]
musicas_NG <- musicas[701:800, ]
musicas_SG <- musicas[801:900, ]
musicas_US <- musicas[901:1000, ]
musicas_NZ <- musicas[1001:1100, ]
musicas_ZA <- musicas[1101:1200, ]

msc_oficial <- rbind(musicas_BR,musicas_CA, musicas_ZA)
colunas_selecionadas <- msc_oficial[colunas]

#aplica função
for (i in colnames(colunas_selecionadas)){
  if (i != "continent"){
    if (i != "country"){
      colunas_selecionadas[i] <- remove_outliers(colunas_selecionadas[[i]])
    }
  }
}
#remove todas as linhas com valors NA
musicas_sem_outliers <- colunas_selecionadas[complete.cases(colunas_selecionadas), ]

for (i in colnames(musicas_sem_outliers)){
  if (i != "continent"){
    if (i != "country"){
      musicas_sem_outliers[i] <- predict(bestNormalize(musicas_sem_outliers[[i]]))
    }
  }
}

musicas_sem_outliers$continent[musicas_sem_outliers$continent=="SA"] <- 0
musicas_sem_outliers$continent[musicas_sem_outliers$continent=="OC"] <- 1
musicas_sem_outliers$continent[musicas_sem_outliers$continent=="AN"] <- 2
musicas_sem_outliers$continent[musicas_sem_outliers$continent=="EU"] <- 3
musicas_sem_outliers$continent[musicas_sem_outliers$continent=="AS"] <- 4
musicas_sem_outliers$continent[musicas_sem_outliers$continent=="AF"] <- 5

vif_values <- vif(lm(continent~., data=musicas_sem_outliers))
vif_values

#boxM(cbind(loudness, acousticness, danceability, energy, valence) ~ continent, data=musicas_sem_outliers)

musicas_sem_outliers$continent[musicas_sem_outliers$continent==0] <- "SA"
musicas_sem_outliers$continent[musicas_sem_outliers$continent==1] <- "OC"
musicas_sem_outliers$continent[musicas_sem_outliers$continent==2] <- "AN"
musicas_sem_outliers$continent[musicas_sem_outliers$continent==3] <- "EU"
musicas_sem_outliers$continent[musicas_sem_outliers$continent==4] <- "AS"
musicas_sem_outliers$continent[musicas_sem_outliers$continent==5] <- "AF"

for (i in colnames(musicas_sem_outliers)){
  if (i != "continent"){
    if (i != "country"){
      print(i)
      print(box_m(musicas_sem_outliers[,i], musicas_sem_outliers$continent))
    }
  }
}

hz_test <- mvn(musicas_sem_outliers_somente_numericos, mvnTest = "hz")
print(hz_test)

musicas_sem_outliers_somente_numericos = musicas_sem_outliers[numericos]
res <- cor(musicas_sem_outliers_somente_numericos, method = "pearson")

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

cov(musicas_sem_outliers_somente_numericos)

#mardia <- mult.norm(musicas_sem_outliers_somente_numericos)
#print(mardia)

#energy_m <- mvnorm.etest(musicas_sem_outliers_somente_numericos, R= 100)
#print(energy_m)
#-------------------------------------------------------------------------------

# Dividir os dados em treinamento (60%) e teste (40%)
set.seed(123)  # Definir uma semente para reprodutibilidade
ind <- sample(2, nrow(musicas_sem_outliers), replace = TRUE, prob = c(0.6, 0.4))

# Criar conjunto de teste e treinamento
training <- musicas_sem_outliers[ind == 1, ] #60% para treinamento
testing <- musicas_sem_outliers[ind == 2, ] #40% para teste

# Verificar a dimensão dos conjuntos criados
dim(training)  # Conjunto de treinamento (60% das observações)
dim(testing)   # Conjunto de teste (40% das observações)
lda.musicas <- lda(country~ loudness + acousticness + danceability + energy + liveness + speechiness + tempo + valence, training) # chamando a função lda do pacote MASS que calcula as 
#funções discriminantes, isto é, cria o modelo LDA

lda.musicas

previsto.training = predict(lda.musicas, training) 


ldahist(data = previsto.training$x[,2], g=training$country) 
ldahist(data = previsto.training$x[,1],g=training$country)


previsto1 = previsto.training$class # mostra as classes (categorias) previstas para cada flor no sub-set training
tab_confusao.training = table(previsto1, training$country) # compara as categorias previstas X categorias realmente existentes no sub-set training
previsto.testing = predict(lda.musicas, testing) # agora aplicando o modelo ao sub-set testing
previsto2 = previsto.testing$class # mostra as classes (categorias) previstas para cada flor no sub-set testing
tab_confusao.testing = table(previsto2, testing$country) # compara as categorias previstas X categorias realmente existentes no sub-set testing
tab_confusao.training
tab_confusao.testing
par(mfrow=c(1,1))
plot(previsto.training$x[,1],previsto.training$x[,2]) # plota LD1xLD2
text(previsto.training$x[,1],previsto.training$x[,2],training$country,pos=4,cex=0.5,col="blue")
