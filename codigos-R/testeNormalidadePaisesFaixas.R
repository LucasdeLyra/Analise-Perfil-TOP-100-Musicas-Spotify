library(readr)
library(moments)
library(bestNormalize)

musicas <- read_csv("Documents/faculdade/Spotify_MQAM/tabelas/musicas_normalizadas.csv")
View(musicas)

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

shapiro.test(musicas_IN$danceability)
shapiro.test(musicas_NG$danceability)
shapiro.test(musicas_US$danceability)
shapiro.test(musicas_ZA$danceability)

skewness(musicas_IN$danceability)
skewness(musicas_NG$danceability)
skewness(musicas_US$danceability)
skewness(musicas_ZA$danceability)

normalized <- bestNormalize(musicas_IN$danceability)
predicted <- predict(normalized)
hist(predicted)
skewness(predicted)
shapiro.test(predicted)

normalized <- bestNormalize(musicas_NG$danceability)
predicted <- predict(normalized)
hist(predicted)
skewness(predicted)
shapiro.test(predicted)

normalized <- bestNormalize(musicas_US$danceability)
predicted <- predict(normalized)
hist(predicted)
skewness(predicted)
shapiro.test(predicted)

normalized <- bestNormalize(musicas_ZA$danceability)
predicted <- predict(normalized)
hist(predicted)
skewness(predicted)
shapiro.test(predicted)
