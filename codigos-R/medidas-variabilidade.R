musicas = read.csv("C:\\Users\\gialb\\Downloads\\musicas (1).csv")
genero = read.csv("C:\\Users\\gialb\\Downloads\\genero (1).csv")
artistas = read.csv("C:\\Users\\gialb\\Downloads\\artistas (1).csv")

# SEPARANDO AS OBSERVAÇÕES POR PAÍS
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

# MEDIDAS DE VARIABILIDADE (VARIÂNCIA, DESVIO PADRÃO E COEFICIENTE DE VARIAÇÃO)
# DE TODAS AS AMOSTRAS
# Calculando medidas de variabilidade para o conjunto de dados completo

# Variância
var_loudness <- var(musicas$loudness)
var_acousticness <- var(musicas$acousticness)
var_danceability <- var(musicas$danceability)
var_energy <- var(musicas$energy)
var_instrumentalness <- var(musicas$instrumentalness)
var_tempo <- var(musicas$tempo)
var_valence <- var(musicas$valence)
var_liveness <- var(musicas$liveness)
var_speechiness <- var(musicas$speechiness)

# Variância para popularidade (dados dos artistas)
var_popularity <- var(artistas$popularity)

# Desvio padrão
sd_loudness <- sd(musicas$loudness)
sd_acousticness <- sd(musicas$acousticness)
sd_danceability <- sd(musicas$danceability)
sd_energy <- sd(musicas$energy)
sd_instrumentalness <- sd(musicas$instrumentalness)
sd_tempo <- sd(musicas$tempo)
sd_valence <- sd(musicas$valence)
sd_liveness <- sd(musicas$liveness)
sd_speechiness <- sd(musicas$speechiness)

# Desvio padrão para popularidade (dados dos artistas)
sd_popularity <- sd(artistas$popularity)

# Coeficiente de variação (CV = desvio padrão / média)
cv_loudness <- sd_loudness / mean(musicas$loudness)
cv_acousticness <- sd_acousticness / mean(musicas$acousticness)
cv_danceability <- sd_danceability / mean(musicas$danceability)
cv_energy <- sd_energy / mean(musicas$energy)
cv_instrumentalness <- sd_instrumentalness / mean(musicas$instrumentalness)
cv_tempo <- sd_tempo / mean(musicas$tempo)
cv_valence <- sd_valence / mean(musicas$valence)
cv_liveness <- sd_liveness / mean(musicas$liveness)
cv_speechiness <- sd_speechiness / mean(musicas$speechiness)

# Coeficiente de variação para popularidade (dados dos artistas)
cv_popularity <- sd_popularity / mean(artistas$popularity)

# Exibindo os resultados
var_loudness
var_acousticness
var_danceability
var_energy
var_instrumentalness
var_tempo
var_valence
var_liveness
var_speechiness
var_popularity

sd_loudness
sd_acousticness
sd_danceability
sd_energy
sd_instrumentalness
sd_tempo
sd_valence
sd_liveness
sd_speechiness
sd_popularity

cv_loudness
cv_acousticness
cv_danceability
cv_energy
cv_instrumentalness
cv_tempo
cv_valence
cv_liveness
cv_speechiness
cv_popularity

# MEDIDAS DE VARIABILIDADE (VARIÂNCIA, DESVIO PADRÃO E COEFICIENTE DE VARIAÇÃO)
# BRASIL

# Calculando medidas de variabilidade para o conjunto de dados do Brasil (musicas_BR)

# Variância
var_loudness_BR <- var(musicas_BR$loudness)
var_acousticness_BR <- var(musicas_BR$acousticness)
var_danceability_BR <- var(musicas_BR$danceability)
var_energy_BR <- var(musicas_BR$energy)
var_instrumentalness_BR <- var(musicas_BR$instrumentalness)
var_liveness_BR <- var(musicas_BR$liveness)
var_speechiness_BR <- var(musicas_BR$speechiness)
var_tempo_BR <- var(musicas_BR$tempo)
var_valence_BR <- var(musicas_BR$valence)

# Desvio padrão
sd_loudness_BR <- sd(musicas_BR$loudness)
sd_acousticness_BR <- sd(musicas_BR$acousticness)
sd_danceability_BR <- sd(musicas_BR$danceability)
sd_energy_BR <- sd(musicas_BR$energy)
sd_instrumentalness_BR <- sd(musicas_BR$instrumentalness)
sd_liveness_BR <- sd(musicas_BR$liveness)
sd_speechiness_BR <- sd(musicas_BR$speechiness)
sd_tempo_BR <- sd(musicas_BR$tempo)
sd_valence_BR <- sd(musicas_BR$valence)

# Coeficiente de variação (CV = desvio padrão / média)
cv_loudness_BR <- sd_loudness_BR / mean(musicas_BR$loudness)
cv_acousticness_BR <- sd_acousticness_BR / mean(musicas_BR$acousticness)
cv_danceability_BR <- sd_danceability_BR / mean(musicas_BR$danceability)
cv_energy_BR <- sd_energy_BR / mean(musicas_BR$energy)
cv_instrumentalness_BR <- sd_instrumentalness_BR / mean(musicas_BR$instrumentalness)
cv_liveness_BR <- sd_liveness_BR / mean(musicas_BR$liveness)
cv_speechiness_BR <- sd_speechiness_BR / mean(musicas_BR$speechiness)
cv_tempo_BR <- sd_tempo_BR / mean(musicas_BR$tempo)
cv_valence_BR <- sd_valence_BR / mean(musicas_BR$valence)

# Exibindo os resultados
var_loudness_BR
var_acousticness_BR
var_danceability_BR
var_energy_BR
var_instrumentalness_BR
var_liveness_BR
var_speechiness_BR
var_tempo_BR
var_valence_BR

sd_loudness_BR
sd_acousticness_BR
sd_danceability_BR
sd_energy_BR
sd_instrumentalness_BR
sd_liveness_BR
sd_speechiness_BR
sd_tempo_BR
sd_valence_BR

cv_loudness_BR
cv_acousticness_BR
cv_danceability_BR
cv_energy_BR
cv_instrumentalness_BR
cv_liveness_BR
cv_speechiness_BR
cv_tempo_BR
cv_valence_BR

# Calculando medidas de variabilidade para o conjunto de dados da Austrália (musicas_AT)

# Variância
var_loudness_AT <- var(musicas_AT$loudness)
var_acousticness_AT <- var(musicas_AT$acousticness)
var_danceability_AT <- var(musicas_AT$danceability)
var_energy_AT <- var(musicas_AT$energy)
var_instrumentalness_AT <- var(musicas_AT$instrumentalness)
var_liveness_AT <- var(musicas_AT$liveness)
var_speechiness_AT <- var(musicas_AT$speechiness)
var_tempo_AT <- var(musicas_AT$tempo)
var_valence_AT <- var(musicas_AT$valence)

# Desvio padrão
sd_loudness_AT <- sd(musicas_AT$loudness)
sd_acousticness_AT <- sd(musicas_AT$acousticness)
sd_danceability_AT <- sd(musicas_AT$danceability)
sd_energy_AT <- sd(musicas_AT$energy)
sd_instrumentalness_AT <- sd(musicas_AT$instrumentalness)
sd_liveness_AT <- sd(musicas_AT$liveness)
sd_speechiness_AT <- sd(musicas_AT$speechiness)
sd_tempo_AT <- sd(musicas_AT$tempo)
sd_valence_AT <- sd(musicas_AT$valence)

# Coeficiente de variação (CV = desvio padrão / média)
cv_loudness_AT <- sd_loudness_AT / mean(musicas_AT$loudness)
cv_acousticness_AT <- sd_acousticness_AT / mean(musicas_AT$acousticness)
cv_danceability_AT <- sd_danceability_AT / mean(musicas_AT$danceability)
cv_energy_AT <- sd_energy_AT / mean(musicas_AT$energy)
cv_instrumentalness_AT <- sd_instrumentalness_AT / mean(musicas_AT$instrumentalness)
cv_liveness_AT <- sd_liveness_AT / mean(musicas_AT$liveness)
cv_speechiness_AT <- sd_speechiness_AT / mean(musicas_AT$speechiness)
cv_tempo_AT <- sd_tempo_AT / mean(musicas_AT$tempo)
cv_valence_AT <- sd_valence_AT / mean(musicas_AT$valence)

# Exibindo os resultados
var_loudness_AT
var_acousticness_AT
var_danceability_AT
var_energy_AT
var_instrumentalness_AT
var_liveness_AT
var_speechiness_AT
var_tempo_AT
var_valence_AT

sd_loudness_AT
sd_acousticness_AT
sd_danceability_AT
sd_energy_AT
sd_instrumentalness_AT
sd_liveness_AT
sd_speechiness_AT
sd_tempo_AT
sd_valence_AT

cv_loudness_AT
cv_acousticness_AT
cv_danceability_AT
cv_energy_AT
cv_instrumentalness_AT
cv_liveness_AT
cv_speechiness_AT
cv_tempo_AT
cv_valence_AT

# Calculando medidas de variabilidade para o conjunto de dados do Canadá (musicas_CA)

# Variância
var_loudness_CA <- var(musicas_CA$loudness)
var_acousticness_CA <- var(musicas_CA$acousticness)
var_danceability_CA <- var(musicas_CA$danceability)
var_energy_CA <- var(musicas_CA$energy)
var_instrumentalness_CA <- var(musicas_CA$instrumentalness)
var_liveness_CA <- var(musicas_CA$liveness)
var_speechiness_CA <- var(musicas_CA$speechiness)
var_tempo_CA <- var(musicas_CA$tempo)
var_valence_CA <- var(musicas_CA$valence)

# Desvio padrão
sd_loudness_CA <- sd(musicas_CA$loudness)
sd_acousticness_CA <- sd(musicas_CA$acousticness)
sd_danceability_CA <- sd(musicas_CA$danceability)
sd_energy_CA <- sd(musicas_CA$energy)
sd_instrumentalness_CA <- sd(musicas_CA$instrumentalness)
sd_liveness_CA <- sd(musicas_CA$liveness)
sd_speechiness_CA <- sd(musicas_CA$speechiness)
sd_tempo_CA <- sd(musicas_CA$tempo)
sd_valence_CA <- sd(musicas_CA$valence)

# Coeficiente de variação (CV = desvio padrão / média)
cv_loudness_CA <- sd_loudness_CA / mean(musicas_CA$loudness)
cv_acousticness_CA <- sd_acousticness_CA / mean(musicas_CA$acousticness)
cv_danceability_CA <- sd_danceability_CA / mean(musicas_CA$danceability)
cv_energy_CA <- sd_energy_CA / mean(musicas_CA$energy)
cv_instrumentalness_CA <- sd_instrumentalness_CA / mean(musicas_CA$instrumentalness)
cv_liveness_CA <- sd_liveness_CA / mean(musicas_CA$liveness)
cv_speechiness_CA <- sd_speechiness_CA / mean(musicas_CA$speechiness)
cv_tempo_CA <- sd_tempo_CA / mean(musicas_CA$tempo)
cv_valence_CA <- sd_valence_CA / mean(musicas_CA$valence)

# Exibindo os resultados
var_loudness_CA
var_acousticness_CA
var_danceability_CA
var_energy_CA
var_instrumentalness_CA
var_liveness_CA
var_speechiness_CA
var_tempo_CA
var_valence_CA

sd_loudness_CA
sd_acousticness_CA
sd_danceability_CA
sd_energy_CA
sd_instrumentalness_CA
sd_liveness_CA
sd_speechiness_CA
sd_tempo_CA
sd_valence_CA

cv_loudness_CA
cv_acousticness_CA
cv_danceability_CA
cv_energy_CA
cv_instrumentalness_CA
cv_liveness_CA
cv_speechiness_CA
cv_tempo_CA
cv_valence_CA

# Calculando medidas de variabilidade para o conjunto de dados da Alemanha (musicas_DE)

# Variância
var_loudness_DE <- var(musicas_DE$loudness)
var_acousticness_DE <- var(musicas_DE$acousticness)
var_danceability_DE <- var(musicas_DE$danceability)
var_energy_DE <- var(musicas_DE$energy)
var_instrumentalness_DE <- var(musicas_DE$instrumentalness)
var_liveness_DE <- var(musicas_DE$liveness)
var_speechiness_DE <- var(musicas_DE$speechiness)
var_tempo_DE <- var(musicas_DE$tempo)
var_valence_DE <- var(musicas_DE$valence)

# Desvio padrão
sd_loudness_DE <- sd(musicas_DE$loudness)
sd_acousticness_DE <- sd(musicas_DE$acousticness)
sd_danceability_DE <- sd(musicas_DE$danceability)
sd_energy_DE <- sd(musicas_DE$energy)
sd_instrumentalness_DE <- sd(musicas_DE$instrumentalness)
sd_liveness_DE <- sd(musicas_DE$liveness)
sd_speechiness_DE <- sd(musicas_DE$speechiness)
sd_tempo_DE <- sd(musicas_DE$tempo)
sd_valence_DE <- sd(musicas_DE$valence)

# Coeficiente de variação (CV = desvio padrão / média)
cv_loudness_DE <- sd_loudness_DE / mean(musicas_DE$loudness)
cv_acousticness_DE <- sd_acousticness_DE / mean(musicas_DE$acousticness)
cv_danceability_DE <- sd_danceability_DE / mean(musicas_DE$danceability)
cv_energy_DE <- sd_energy_DE / mean(musicas_DE$energy)
cv_instrumentalness_DE <- sd_instrumentalness_DE / mean(musicas_DE$instrumentalness)
cv_liveness_DE <- sd_liveness_DE / mean(musicas_DE$liveness)
cv_speechiness_DE <- sd_speechiness_DE / mean(musicas_DE$speechiness)
cv_tempo_DE <- sd_tempo_DE / mean(musicas_DE$tempo)
cv_valence_DE <- sd_valence_DE / mean(musicas_DE$valence)

# Exibindo os resultados
var_loudness_DE
var_acousticness_DE
var_danceability_DE
var_energy_DE
var_instrumentalness_DE
var_liveness_DE
var_speechiness_DE
var_tempo_DE
var_valence_DE

sd_loudness_DE
sd_acousticness_DE
sd_danceability_DE
sd_energy_DE
sd_instrumentalness_DE
sd_liveness_DE
sd_speechiness_DE
sd_tempo_DE
sd_valence_DE

cv_loudness_DE
cv_acousticness_DE
cv_danceability_DE
cv_energy_DE
cv_instrumentalness_DE
cv_liveness_DE
cv_speechiness_DE
cv_tempo_DE
cv_valence_DE

# Calculando medidas de variabilidade para o conjunto de dados do Reino Unido (musicas_GB)

# Variância
var_loudness_GB <- var(musicas_GB$loudness)
var_acousticness_GB <- var(musicas_GB$acousticness)
var_danceability_GB <- var(musicas_GB$danceability)
var_energy_GB <- var(musicas_GB$energy)
var_instrumentalness_GB <- var(musicas_GB$instrumentalness)
var_liveness_GB <- var(musicas_GB$liveness)
var_speechiness_GB <- var(musicas_GB$speechiness)
var_tempo_GB <- var(musicas_GB$tempo)
var_valence_GB <- var(musicas_GB$valence)

# Desvio padrão
sd_loudness_GB <- sd(musicas_GB$loudness)
sd_acousticness_GB <- sd(musicas_GB$acousticness)
sd_danceability_GB <- sd(musicas_GB$danceability)
sd_energy_GB <- sd(musicas_GB$energy)
sd_instrumentalness_GB <- sd(musicas_GB$instrumentalness)
sd_liveness_GB <- sd(musicas_GB$liveness)
sd_speechiness_GB <- sd(musicas_GB$speechiness)
sd_tempo_GB <- sd(musicas_GB$tempo)
sd_valence_GB <- sd(musicas_GB$valence)

# Coeficiente de variação (CV = desvio padrão / média)
cv_loudness_GB <- sd_loudness_GB / mean(musicas_GB$loudness)
cv_acousticness_GB <- sd_acousticness_GB / mean(musicas_GB$acousticness)
cv_danceability_GB <- sd_danceability_GB / mean(musicas_GB$danceability)
cv_energy_GB <- sd_energy_GB / mean(musicas_GB$energy)
cv_instrumentalness_GB <- sd_instrumentalness_GB / mean(musicas_GB$instrumentalness)
cv_liveness_GB <- sd_liveness_GB / mean(musicas_GB$liveness)
cv_speechiness_GB <- sd_speechiness_GB / mean(musicas_GB$speechiness)
cv_tempo_GB <- sd_tempo_GB / mean(musicas_GB$tempo)
cv_valence_GB <- sd_valence_GB / mean(musicas_GB$valence)

# Exibindo os resultados
var_loudness_GB
var_acousticness_GB
var_danceability_GB
var_energy_GB
var_instrumentalness_GB
var_liveness_GB
var_speechiness_GB
var_tempo_GB
var_valence_GB

sd_loudness_GB
sd_acousticness_GB
sd_danceability_GB
sd_energy_GB
sd_instrumentalness_GB
sd_liveness_GB
sd_speechiness_GB
sd_tempo_GB
sd_valence_GB

cv_loudness_GB
cv_acousticness_GB
cv_danceability_GB
cv_energy_GB
cv_instrumentalness_GB
cv_liveness_GB
cv_speechiness_GB
cv_tempo_GB
cv_valence_GB

# Calculando medidas de variabilidade para o conjunto de dados da Índia (musicas_IN)

# Variância
var_loudness_IN <- var(musicas_IN$loudness)
var_acousticness_IN <- var(musicas_IN$acousticness)
var_danceability_IN <- var(musicas_IN$danceability)
var_energy_IN <- var(musicas_IN$energy)
var_instrumentalness_IN <- var(musicas_IN$instrumentalness)
var_liveness_IN <- var(musicas_IN$liveness)
var_speechiness_IN <- var(musicas_IN$speechiness)
var_tempo_IN <- var(musicas_IN$tempo)
var_valence_IN <- var(musicas_IN$valence)

# Desvio padrão
sd_loudness_IN <- sd(musicas_IN$loudness)
sd_acousticness_IN <- sd(musicas_IN$acousticness)
sd_danceability_IN <- sd(musicas_IN$danceability)
sd_energy_IN <- sd(musicas_IN$energy)
sd_instrumentalness_IN <- sd(musicas_IN$instrumentalness)
sd_liveness_IN <- sd(musicas_IN$liveness)
sd_speechiness_IN <- sd(musicas_IN$speechiness)
sd_tempo_IN <- sd(musicas_IN$tempo)
sd_valence_IN <- sd(musicas_IN$valence)

# Coeficiente de variação (CV = desvio padrão / média)
cv_loudness_IN <- sd_loudness_IN / mean(musicas_IN$loudness)
cv_acousticness_IN <- sd_acousticness_IN / mean(musicas_IN$acousticness)
cv_danceability_IN <- sd_danceability_IN / mean(musicas_IN$danceability)
cv_energy_IN <- sd_energy_IN / mean(musicas_IN$energy)
cv_instrumentalness_IN <- sd_instrumentalness_IN / mean(musicas_IN$instrumentalness)
cv_liveness_IN <- sd_liveness_IN / mean(musicas_IN$liveness)
cv_speechiness_IN <- sd_speechiness_IN / mean(musicas_IN$speechiness)
cv_tempo_IN <- sd_tempo_IN / mean(musicas_IN$tempo)
cv_valence_IN <- sd_valence_IN / mean(musicas_IN$valence)

# Exibindo os resultados
var_loudness_IN
var_acousticness_IN
var_danceability_IN
var_energy_IN
var_instrumentalness_IN
var_liveness_IN
var_speechiness_IN
var_tempo_IN
var_valence_IN

sd_loudness_IN
sd_acousticness_IN
sd_danceability_IN
sd_energy_IN
sd_instrumentalness_IN
sd_liveness_IN
sd_speechiness_IN
sd_tempo_IN
sd_valence_IN

cv_loudness_IN
cv_acousticness_IN
cv_danceability_IN
cv_energy_IN
cv_instrumentalness_IN
cv_liveness_IN
cv_speechiness_IN
cv_tempo_IN
cv_valence_IN

# Calculando medidas de variabilidade para o conjunto de dados do México (musicas_MX)

# Variância
var_loudness_MX <- var(musicas_MX$loudness)
var_acousticness_MX <- var(musicas_MX$acousticness)
var_danceability_MX <- var(musicas_MX$danceability)
var_energy_MX <- var(musicas_MX$energy)
var_instrumentalness_MX <- var(musicas_MX$instrumentalness)
var_liveness_MX <- var(musicas_MX$liveness)
var_speechiness_MX <- var(musicas_MX$speechiness)
var_tempo_MX <- var(musicas_MX$tempo)
var_valence_MX <- var(musicas_MX$valence)

# Desvio padrão
sd_loudness_MX <- sd(musicas_MX$loudness)
sd_acousticness_MX <- sd(musicas_MX$acousticness)
sd_danceability_MX <- sd(musicas_MX$danceability)
sd_energy_MX <- sd(musicas_MX$energy)
sd_instrumentalness_MX <- sd(musicas_MX$instrumentalness)
sd_liveness_MX <- sd(musicas_MX$liveness)
sd_speechiness_MX <- sd(musicas_MX$speechiness)
sd_tempo_MX <- sd(musicas_MX$tempo)
sd_valence_MX <- sd(musicas_MX$valence)

# Coeficiente de variação (CV = desvio padrão / média)
cv_loudness_MX <- sd_loudness_MX / mean(musicas_MX$loudness)
cv_acousticness_MX <- sd_acousticness_MX / mean(musicas_MX$acousticness)
cv_danceability_MX <- sd_danceability_MX / mean(musicas_MX$danceability)
cv_energy_MX <- sd_energy_MX / mean(musicas_MX$energy)
cv_instrumentalness_MX <- sd_instrumentalness_MX / mean(musicas_MX$instrumentalness)
cv_liveness_MX <- sd_liveness_MX / mean(musicas_MX$liveness)
cv_speechiness_MX <- sd_speechiness_MX / mean(musicas_MX$speechiness)
cv_tempo_MX <- sd_tempo_MX / mean(musicas_MX$tempo)
cv_valence_MX <- sd_valence_MX / mean(musicas_MX$valence)

# Exibindo os resultados
var_loudness_MX
var_acousticness_MX
var_danceability_MX
var_energy_MX
var_instrumentalness_MX
var_liveness_MX
var_speechiness_MX
var_tempo_MX
var_valence_MX

sd_loudness_MX
sd_acousticness_MX
sd_danceability_MX
sd_energy_MX
sd_instrumentalness_MX
sd_liveness_MX
sd_speechiness_MX
sd_tempo_MX
sd_valence_MX

cv_loudness_MX
cv_acousticness_MX
cv_danceability_MX
cv_energy_MX
cv_instrumentalness_MX
cv_liveness_MX
cv_speechiness_MX
cv_tempo_MX
cv_valence_MX

# Calculando medidas de variabilidade para o conjunto de dados da Nigéria (musicas_NG)

# Variância
var_loudness_NG <- var(musicas_NG$loudness)
var_acousticness_NG <- var(musicas_NG$acousticness)
var_danceability_NG <- var(musicas_NG$danceability)
var_energy_NG <- var(musicas_NG$energy)
var_instrumentalness_NG <- var(musicas_NG$instrumentalness)
var_liveness_NG <- var(musicas_NG$liveness)
var_speechiness_NG <- var(musicas_NG$speechiness)
var_tempo_NG <- var(musicas_NG$tempo)
var_valence_NG <- var(musicas_NG$valence)

# Desvio padrão
sd_loudness_NG <- sd(musicas_NG$loudness)
sd_acousticness_NG <- sd(musicas_NG$acousticness)
sd_danceability_NG <- sd(musicas_NG$danceability)
sd_energy_NG <- sd(musicas_NG$energy)
sd_instrumentalness_NG <- sd(musicas_NG$instrumentalness)
sd_liveness_NG <- sd(musicas_NG$liveness)
sd_speechiness_NG <- sd(musicas_NG$speechiness)
sd_tempo_NG <- sd(musicas_NG$tempo)
sd_valence_NG <- sd(musicas_NG$valence)

# Coeficiente de variação (CV = desvio padrão / média)
cv_loudness_NG <- sd_loudness_NG / mean(musicas_NG$loudness)
cv_acousticness_NG <- sd_acousticness_NG / mean(musicas_NG$acousticness)
cv_danceability_NG <- sd_danceability_NG / mean(musicas_NG$danceability)
cv_energy_NG <- sd_energy_NG / mean(musicas_NG$energy)
cv_instrumentalness_NG <- sd_instrumentalness_NG / mean(musicas_NG$instrumentalness)
cv_liveness_NG <- sd_liveness_NG / mean(musicas_NG$liveness)
cv_speechiness_NG <- sd_speechiness_NG / mean(musicas_NG$speechiness)
cv_tempo_NG <- sd_tempo_NG / mean(musicas_NG$tempo)
cv_valence_NG <- sd_valence_NG / mean(musicas_NG$valence)

# Exibindo os resultados
var_loudness_NG
var_acousticness_NG
var_danceability_NG
var_energy_NG
var_instrumentalness_NG
var_liveness_NG
var_speechiness_NG
var_tempo_NG
var_valence_NG

sd_loudness_NG
sd_acousticness_NG
sd_danceability_NG
sd_energy_NG
sd_instrumentalness_NG
sd_liveness_NG
sd_speechiness_NG
sd_tempo_NG
sd_valence_NG

cv_loudness_NG
cv_acousticness_NG
cv_danceability_NG
cv_energy_NG
cv_instrumentalness_NG
cv_liveness_NG
cv_speechiness_NG
cv_tempo_NG
cv_valence_NG

# Calculando medidas de variabilidade para o conjunto de dados da Nova Zelândia(musicas_NZ)

# Variância
var_loudness_NZ <- var(musicas_NZ$loudness)
var_acousticness_NZ <- var(musicas_NZ$acousticness)
var_danceability_NZ <- var(musicas_NZ$danceability)
var_energy_NZ <- var(musicas_NZ$energy)
var_instrumentalness_NZ <- var(musicas_NZ$instrumentalness)
var_liveness_NZ <- var(musicas_NZ$liveness)
var_speechiness_NZ <- var(musicas_NZ$speechiness)
var_tempo_NZ <- var(musicas_NZ$tempo)
var_valence_NZ <- var(musicas_NZ$valence)

# Desvio padrão
sd_loudness_NZ <- sd(musicas_NZ$loudness)
sd_acousticness_NZ <- sd(musicas_NZ$acousticness)
sd_danceability_NZ <- sd(musicas_NZ$danceability)
sd_energy_NZ <- sd(musicas_NZ$energy)
sd_instrumentalness_NZ <- sd(musicas_NZ$instrumentalness)
sd_liveness_NZ <- sd(musicas_NZ$liveness)
sd_speechiness_NZ <- sd(musicas_NZ$speechiness)
sd_tempo_NZ <- sd(musicas_NZ$tempo)
sd_valence_NZ <- sd(musicas_NZ$valence)

# Coeficiente de variação (CV = desvio padrão / média)
cv_loudness_NZ <- sd_loudness_NZ / mean(musicas_NZ$loudness)
cv_acousticness_NZ <- sd_acousticness_NZ / mean(musicas_NZ$acousticness)
cv_danceability_NZ <- sd_danceability_NZ / mean(musicas_NZ$danceability)
cv_energy_NZ <- sd_energy_NZ / mean(musicas_NZ$energy)
cv_instrumentalness_NZ <- sd_instrumentalness_NZ / mean(musicas_NZ$instrumentalness)
cv_liveness_NZ <- sd_liveness_NZ / mean(musicas_NZ$liveness)
cv_speechiness_NZ <- sd_speechiness_NZ / mean(musicas_NZ$speechiness)
cv_tempo_NZ <- sd_tempo_NZ / mean(musicas_NZ$tempo)
cv_valence_NZ <- sd_valence_NZ / mean(musicas_NZ$valence)

# Exibindo os resultados
var_loudness_NZ
var_acousticness_NZ
var_danceability_NZ
var_energy_NZ
var_instrumentalness_NZ
var_liveness_NZ
var_speechiness_NZ
var_tempo_NZ
var_valence_NZ

sd_loudness_NZ
sd_acousticness_NZ
sd_danceability_NZ
sd_energy_NZ
sd_instrumentalness_NZ
sd_liveness_NZ
sd_speechiness_NZ
sd_tempo_NZ
sd_valence_NZ

cv_loudness_NZ
cv_acousticness_NZ
cv_danceability_NZ
cv_energy_NZ
cv_instrumentalness_NZ
cv_liveness_NZ
cv_speechiness_NZ
cv_tempo_NZ
cv_valence_NZ

# Calculando medidas de variabilidade para o conjunto de dados da SINGAPURA (musicas_SG)

# Variância
var_loudness_SG <- var(musicas_SG$loudness)
var_acousticness_SG <- var(musicas_SG$acousticness)
var_danceability_SG <- var(musicas_SG$danceability)
var_energy_SG <- var(musicas_SG$energy)
var_instrumentalness_SG <- var(musicas_SG$instrumentalness)
var_liveness_SG <- var(musicas_SG$liveness)
var_speechiness_SG <- var(musicas_SG$speechiness)
var_tempo_SG <- var(musicas_SG$tempo)
var_valence_SG <- var(musicas_SG$valence)

# Desvio padrão
sd_loudness_SG <- sd(musicas_SG$loudness)
sd_acousticness_SG <- sd(musicas_SG$acousticness)
sd_danceability_SG <- sd(musicas_SG$danceability)
sd_energy_SG <- sd(musicas_SG$energy)
sd_instrumentalness_SG <- sd(musicas_SG$instrumentalness)
sd_liveness_SG <- sd(musicas_SG$liveness)
sd_speechiness_SG <- sd(musicas_SG$speechiness)
sd_tempo_SG <- sd(musicas_SG$tempo)
sd_valence_SG <- sd(musicas_SG$valence)

# Coeficiente de variação (CV = desvio padrão / média)
cv_loudness_SG <- sd_loudness_SG / mean(musicas_SG$loudness)
cv_acousticness_SG <- sd_acousticness_SG / mean(musicas_SG$acousticness)
cv_danceability_SG <- sd_danceability_SG / mean(musicas_SG$danceability)
cv_energy_SG <- sd_energy_SG / mean(musicas_SG$energy)
cv_instrumentalness_SG <- sd_instrumentalness_SG / mean(musicas_SG$instrumentalness)
cv_liveness_SG <- sd_liveness_SG / mean(musicas_SG$liveness)
cv_speechiness_SG <- sd_speechiness_SG / mean(musicas_SG$speechiness)
cv_tempo_SG <- sd_tempo_SG / mean(musicas_SG$tempo)
cv_valence_SG <- sd_valence_SG / mean(musicas_SG$valence)

# Exibindo os resultados
var_loudness_SG
var_acousticness_SG
var_danceability_SG
var_energy_SG
var_instrumentalness_SG
var_liveness_SG
var_speechiness_SG
var_tempo_SG
var_valence_SG

sd_loudness_SG
sd_acousticness_SG
sd_danceability_SG
sd_energy_SG
sd_instrumentalness_SG
sd_liveness_SG
sd_speechiness_SG
sd_tempo_SG
sd_valence_SG

cv_loudness_SG
cv_acousticness_SG
cv_danceability_SG
cv_energy_SG
cv_instrumentalness_SG
cv_liveness_SG
cv_speechiness_SG
cv_tempo_SG
cv_valence_SG

# Calculando medidas de variabilidade para o conjunto de dados dos ESTADOS UNIDOS (musicas_US)

# Variância
var_loudness_US <- var(musicas_US$loudness)
var_acousticness_US <- var(musicas_US$acousticness)
var_danceability_US <- var(musicas_US$danceability)
var_energy_US <- var(musicas_US$energy)
var_instrumentalness_US <- var(musicas_US$instrumentalness)
var_liveness_US <- var(musicas_US$liveness)
var_speechiness_US <- var(musicas_US$speechiness)
var_tempo_US <- var(musicas_US$tempo)
var_valence_US <- var(musicas_US$valence)

# Desvio padrão
sd_loudness_US <- sd(musicas_US$loudness)
sd_acousticness_US <- sd(musicas_US$acousticness)
sd_danceability_US <- sd(musicas_US$danceability)
sd_energy_US <- sd(musicas_US$energy)
sd_instrumentalness_US <- sd(musicas_US$instrumentalness)
sd_liveness_US <- sd(musicas_US$liveness)
sd_speechiness_US <- sd(musicas_US$speechiness)
sd_tempo_US <- sd(musicas_US$tempo)
sd_valence_US <- sd(musicas_US$valence)

# Coeficiente de variação (CV = desvio padrão / média)
cv_loudness_US <- sd_loudness_US / mean(musicas_US$loudness)
cv_acousticness_US <- sd_acousticness_US / mean(musicas_US$acousticness)
cv_danceability_US <- sd_danceability_US / mean(musicas_US$danceability)
cv_energy_US <- sd_energy_US / mean(musicas_US$energy)
cv_instrumentalness_US <- sd_instrumentalness_US / mean(musicas_US$instrumentalness)
cv_liveness_US <- sd_liveness_US / mean(musicas_US$liveness)
cv_speechiness_US <- sd_speechiness_US / mean(musicas_US$speechiness)
cv_tempo_US <- sd_tempo_US / mean(musicas_US$tempo)
cv_valence_US <- sd_valence_US / mean(musicas_US$valence)

# Exibindo os resultados
var_loudness_US
var_acousticness_US
var_danceability_US
var_energy_US
var_instrumentalness_US
var_liveness_US
var_speechiness_US
var_tempo_US
var_valence_US

sd_loudness_US
sd_acousticness_US
sd_danceability_US
sd_energy_US
sd_instrumentalness_US
sd_liveness_US
sd_speechiness_US
sd_tempo_US
sd_valence_US

cv_loudness_US
cv_acousticness_US
cv_danceability_US
cv_energy_US
cv_instrumentalness_US
cv_liveness_US
cv_speechiness_US
cv_tempo_US
cv_valence_US

# Calculando medidas de variabilidade para o conjunto de dados da ÁFRICA DO SUL (musicas_ZA)

# Variância
var_loudness_ZA <- var(musicas_ZA$loudness)
var_acousticness_ZA <- var(musicas_ZA$acousticness)
var_danceability_ZA <- var(musicas_ZA$danceability)
var_energy_ZA <- var(musicas_ZA$energy)
var_instrumentalness_ZA <- var(musicas_ZA$instrumentalness)
var_liveness_ZA <- var(musicas_ZA$liveness)
var_speechiness_ZA <- var(musicas_ZA$speechiness)
var_tempo_ZA <- var(musicas_ZA$tempo)
var_valence_ZA <- var(musicas_ZA$valence)

# Desvio padrão
sd_loudness_ZA <- sd(musicas_ZA$loudness)
sd_acousticness_ZA <- sd(musicas_ZA$acousticness)
sd_danceability_ZA <- sd(musicas_ZA$danceability)
sd_energy_ZA <- sd(musicas_ZA$energy)
sd_instrumentalness_ZA <- sd(musicas_ZA$instrumentalness)
sd_liveness_ZA <- sd(musicas_ZA$liveness)
sd_speechiness_ZA <- sd(musicas_ZA$speechiness)
sd_tempo_ZA <- sd(musicas_ZA$tempo)
sd_valence_ZA <- sd(musicas_ZA$valence)

# Coeficiente de variação (CV = desvio padrão / média)
cv_loudness_ZA <- sd_loudness_ZA / mean(musicas_ZA$loudness)
cv_acousticness_ZA <- sd_acousticness_ZA / mean(musicas_ZA$acousticness)
cv_danceability_ZA <- sd_danceability_ZA / mean(musicas_ZA$danceability)
cv_energy_ZA <- sd_energy_ZA / mean(musicas_ZA$energy)
cv_instrumentalness_ZA <- sd_instrumentalness_ZA / mean(musicas_ZA$instrumentalness)
cv_liveness_ZA <- sd_liveness_ZA / mean(musicas_ZA$liveness)
cv_speechiness_ZA <- sd_speechiness_ZA / mean(musicas_ZA$speechiness)
cv_tempo_ZA <- sd_tempo_ZA / mean(musicas_ZA$tempo)
cv_valence_ZA <- sd_valence_ZA / mean(musicas_ZA$valence)

# Exibindo os resultados
var_loudness_ZA
var_acousticness_ZA
var_danceability_ZA
var_energy_ZA
var_instrumentalness_ZA
var_liveness_ZA
var_speechiness_ZA
var_tempo_ZA
var_valence_ZA

sd_loudness_ZA
sd_acousticness_ZA
sd_danceability_ZA
sd_energy_ZA
sd_instrumentalness_ZA
sd_liveness_ZA
sd_speechiness_ZA
sd_tempo_ZA
sd_valence_ZA

cv_loudness_ZA
cv_acousticness_ZA
cv_danceability_ZA
cv_energy_ZA
cv_instrumentalness_ZA
cv_liveness_ZA
cv_speechiness_ZA
cv_tempo_ZA
cv_valence_ZA

#BOX-PLOTS!!!
#GLOBAL
boxplot(musicas$loudness,
        main = "Volume - Globalmente",
        ylab = "Volume",
        col = "lightgreen")

boxplot(musicas_BR$loudness,
        main = "Volume - Brasil",
        ylab = "Volume",
        col = "lightgreen")