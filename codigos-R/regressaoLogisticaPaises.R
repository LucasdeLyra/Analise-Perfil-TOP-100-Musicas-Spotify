library(readr)
library(dplyr)

musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas.csv")

summary(musicas)

### SEPARANDO TABELAS DOS CONTINENTES
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

la = rbind(musicas_BR, musicas_MX)
na = rbind(musicas_US, musicas_CA)
eu = rbind(musicas_DE, musicas_GB)
as = rbind(musicas_SG, musicas_IN)
af = rbind(musicas_ZA, musicas_NG)
oc = rbind(musicas_NZ, musicas_AT)

la$country[la$country == "BR"] <- 0
la$country[la$country == "MX"] <- 1
na$country[na$country == "CA"] <- 0
na$country[na$country == "US"] <- 1
eu$country[na$country == "DE"] <- 0
eu$country[na$country == "GB"] <- 1
as$country[as$country == "IN"] <- 0
as$country[as$country == "sg"] <- 1
af$country[af$country == "NG"] <- 0
af$country[af$country == "ZA"] <- 1
oc$country[oc$country == "AT"] <- 0
oc$country[oc$country == "NZ"] <- 1

# FAZENDO AS REGRESSOES
