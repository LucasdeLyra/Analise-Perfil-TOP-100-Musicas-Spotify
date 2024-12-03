library(readr)
library(caret)

# ANALISE DAS VARIAVEIS E DOS OBJETOS
# Seleção de variáveis, identificação de outliers
# • Cabe ao pesquisador selecionar as variáveis relevantes
# • A técnica é muito sensível a outliers
#   – Deve-se localizar os outliers de cada variável
#   – Cabe analisar se devem ou não ser retirados
# • É comum que os outliers formem grupos isolados

# PADRONIZACAO DAS VARIAVEIS
# • Medidas/escalas diferentes distorcem a estrutura do agrupamento
# • Solução → Padronização resolve problema de diferentes escalas ou magnitudes das variáveis
# • Padronização faz com que seja atribuído o mesmo peso para cada variável

# TIPOS DE PADRONIZACAO
# • z-score
# • Método range: -1 a +1
# • Método range: 0 a 1
# • Método da máxima amplitude
# • Método da média =1
# • Método do desvio-padrão =1


# IMPORTANDO A BIBLIOTECA
musicas <- read_csv("Documents/faculdade/4sem/Spotify_MQAM/tabelas/musicas.csv")
View(musicas)

# VARIAVEIS QUE PODEM SER UTEIS
# acusticidade, dancabilidade, energia, valencia, tempo

# acusticidade tem 0 outliers
AQ1 <- quantile(musicas$acousticness, .25)
AQ3 <- quantile(musicas$acousticness, .75)
AIQR <- IQR(musicas$acousticness)
outliers <- subset(musicas, musicas$acousticness<(AQ1 - 1.5*AIQR) | musicas$acousticness>(AQ3 + 1.5*AIQR))

# dancabilidade tem 1 outlier
DQ1 <- quantile(musicas$danceability, .25)
DQ3 <- quantile(musicas$danceability, .75)
DIQR <- IQR(musicas$danceability)
outliers <- subset(musicas, musicas$danceability<(DQ1 - 1.5*DIQR) | musicas$danceability>(DQ3 + 1.5*DIQR))

# energia tem 2 outliers
EQ1 <- quantile(musicas$energy, .25)
EQ3 <- quantile(musicas$energy, .75)
EIQR <- IQR(musicas$energy)
outliers <- subset(musicas, musicas$energy<(EQ1 - 1.5*EIQR) | musicas$energy>(EQ3 + 1.5*EIQR))

# valencia tem 0 outliers
VQ1 <- quantile(musicas$valence, .25)
VQ3 <- quantile(musicas$valence, .75)
VIQR <- IQR(musicas$valence)
outliers <- subset(musicas, musicas$valence<(VQ1 - 1.5*VIQR) | musicas$valence>(VQ3 + 1.5*VIQR))

# tempo tem 7 outliers
TQ1 <- quantile(musicas$tempo, .25)
TQ3 <- quantile(musicas$tempo, .75)
TIQR <- IQR(musicas$tempo)
outliers <- subset(musicas, musicas$tempo<(TQ1 - 1.5*TIQR) | musicas$tempo>(TQ3 + 1.5*TIQR))

# speechiness tem 158 outliers
# loudness tem 29 outliers
# liveness tem 128 outliers
# instrumentalness tem 254 outliers

# PEGANDO APENAS O QUE TEM POUCOS OUTLIERS:
musicasSemOutliers <- subset(musicas,
                             (musicas$acousticness>=(AQ1 - 1.5*AIQR) & musicas$acousticness<=(AQ3 + 1.5*AIQR)) &
                             (musicas$danceability>=(DQ1 - 1.5*DIQR) & musicas$danceability<=(DQ3 + 1.5*DIQR)) &
                             (musicas$energy>=(EQ1 - 1.5*EIQR) & musicas$energy<=(EQ3 + 1.5*EIQR)) &
                             (musicas$valence>=(VQ1 - 1.5*VIQR) & musicas$valence<=(VQ3 + 1.5*VIQR)) &
                             (musicas$tempo>=(TQ1 - 1.5*TIQR) & musicas$tempo<=(TQ3 + 1.5*TIQR)))
# TOTAL DE OUTLIERS REMOVIDAS: 10

# PADRONIZANDO TEMPO
rangeStand <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

musicasSemOutliers[15] <- as.data.frame(lapply(musicasSemOutliers[15], rangeStand))

# POSSIBILIDADE 1: AGRUPAR POR PAISES
# possivel problema: poucos dados/pais (100)
# possivel solucao: agrupar por continente


# POSSIBILIDADE 2: AGRUPAR POR CONTINENTE
# possivel problema: poucos dados/continente (200)
# possivel solucao: agrupar por PIB (em quartos e em metades)


# POSSIBILIDADE 3: AGRUPAR POR PIB - 4 GRUPOS

# importando os pibs dos paises (fonte: FMI,https://www.imf.org/en/Publications/WEO/weo-database/2023/October/weo-report?c=512,914,612,171,614,311,213,911,314,193,122,912,313,419,513,316,913,124,339,638,514,218,963,616,223,516,918,748,618,624,522,622,156,626,628,228,924,233,632,636,634,238,662,960,423,935,128,611,321,243,248,469,253,642,643,939,734,644,819,172,132,646,648,915,134,652,174,328,258,656,654,336,263,268,532,944,176,534,536,429,433,178,436,136,343,158,439,916,664,826,542,967,443,917,544,941,446,666,668,672,946,137,546,674,676,548,556,678,181,867,682,684,273,868,921,948,943,686,688,518,728,836,558,138,196,278,692,694,962,142,449,564,565,283,853,288,293,566,964,182,359,453,968,922,714,862,135,716,456,722,942,718,724,576,936,961,813,726,199,733,184,524,361,362,364,732,366,144,146,463,528,923,738,578,537,742,866,369,744,186,925,869,746,926,466,112,111,298,927,846,299,582,487,474,754,698,&s=NGDPD,&sy=2021&ey=2028&ssm=0&scsm=1&scc=0&ssd=1&ssc=0&sic=0&sort=country&ds=.&br=1)
pibPaises <- read_csv("Documents/faculdade/4sem/Spotify_MQAM/tabelas/pibPaises.csv")
View(pibPaises)

sorted_tab <- pibPaises[order(pibPaises$PIB2024),] 

pibPaises <- sorted_tab

pibPaises$GrupoQuartos[1:3] <- 1
pibPaises$GrupoQuartos[4:6] <- 2
pibPaises$GrupoQuartos[7:9] <- 3
pibPaises$GrupoQuartos[10:12] <- 4

# POSSIBILIDADE 4: AGRUPAR POR PIB - 2 GRUPOS
pibPaises$GrupoMetade[1:6] <- 1
pibPaises$GrupoMetade[7:12] <- 2

# POSSIBILIDADE 5: AGRUPAR POR LINGUA MAIS FALADA NO PAIS (INGLES X OUTRAS)


