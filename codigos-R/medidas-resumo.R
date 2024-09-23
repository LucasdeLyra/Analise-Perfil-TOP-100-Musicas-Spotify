file.choose()
musicas = read.csv("C:\\Users\\gialb\\Downloads\\musicas (1).csv")
file.choose()
genero = read.csv("C:\\Users\\gialb\\Downloads\\genero (1).csv")
file.choose()
artistas = read.csv("C:\\Users\\gialb\\Downloads\\artistas (1).csv")

# MEDIDAS RESUMO DE TODAS AS AMOSTRAS

summary(musicas)
summary(artistas)

calc_moda <- function(vetor) {
  uniqv <- unique(vetor)
  uniqv[which.max(tabulate(match(vetor, uniqv)))]
}

moda_loudness <- calc_moda(musicas$loudness)
moda_acousticness <- calc_moda(musicas$acousticness)
moda_danceability <- calc_moda(musicas$danceability)
moda_energy <- calc_moda(musicas$energy)
moda_instrumentalness <- calc_moda(musicas$instrumentalness)
moda_key <- calc_moda(musicas$key)
moda_mode = calc_moda(musicas$mode)
moda_time_signature = calc_moda(musicas$time_signature)
moda_liveness <- calc_moda(musicas$liveness)
moda_danceability <- calc_moda(musicas$danceability)
moda_speechiness <- calc_moda(musicas$speechiness)
moda_tempo <- calc_moda(musicas$tempo)
moda_valence <- calc_moda(musicas$valence)
moda_popularity <- calc_moda(artistas$popularity)

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


# CALCULANDO AS MEDIDA-RESUMO PARA CADA PAÍS
# BRASIL
summary(musicas_BR)
moda_loudness_BR <- calc_moda(musicas_BR$loudness)
moda_acousticness_BR <- calc_moda(musicas_BR$acousticness)
moda_danceability_BR <- calc_moda(musicas_BR$danceability)
moda_energy_BR <- calc_moda(musicas_BR$energy)
moda_instrumentalness_BR <- calc_moda(musicas_BR$instrumentalness)
moda_key_BR <- calc_moda(musicas_BR$key)
moda_mode_BR = calc_moda(musicas_BR$mode)
moda_time_signature_BR = calc_moda(musicas_BR$time_signature)
moda_liveness_BR <- calc_moda(musicas_BR$liveness)
moda_danceability_BR <- calc_moda(musicas_BR$danceability)
moda_speechiness_BR <- calc_moda(musicas_BR$speechiness)
moda_tempo_BR <- calc_moda(musicas_BR$tempo)
moda_valence_BR <- calc_moda(musicas_BR$valence)

#AUSTRÁLIA
summary(musicas_AT)
moda_loudness_AT <- calc_moda(musicas_AT$loudness)
moda_acousticness_AT <- calc_moda(musicas_AT$acousticness)
moda_danceability_AT <- calc_moda(musicas_AT$danceability)
moda_energy_AT <- calc_moda(musicas_AT$energy)
moda_instrumentalness_AT <- calc_moda(musicas_AT$instrumentalness)
moda_key_AT <- calc_moda(musicas_AT$key)
moda_mode_AT = calc_moda(musicas_AT$mode)
moda_time_signature_AT = calc_moda(musicas_AT$time_signature)
moda_liveness_AT <- calc_moda(musicas_AT$liveness)
moda_danceability_AT <- calc_moda(musicas_AT$danceability)
moda_speechiness_AT <- calc_moda(musicas_AT$speechiness)
moda_tempo_AT <- calc_moda(musicas_AT$tempo)
moda_valence_AT <- calc_moda(musicas_AT$valence)


#CANADÁ
summary(musicas_CA)
moda_loudness_CA <- calc_moda(musicas_CA$loudness)
moda_acousticness_CA <- calc_moda(musicas_CA$acousticness)
moda_danceability_CA <- calc_moda(musicas_CA$danceability)
moda_energy_CA <- calc_moda(musicas_CA$energy)
moda_instrumentalness_CA <- calc_moda(musicas_CA$instrumentalness)
moda_key_CA <- calc_moda(musicas_CA$key)
moda_mode_CA = calc_moda(musicas_CA$mode)
moda_time_signature_CA = calc_moda(musicas_CA$time_signature)
moda_liveness_CA <- calc_moda(musicas_CA$liveness)
moda_danceability_CA <- calc_moda(musicas_CA$danceability)
moda_speechiness_CA <- calc_moda(musicas_CA$speechiness)
moda_tempo_CA <- calc_moda(musicas_CA$tempo)
moda_valence_CA <- calc_moda(musicas_CA$valence)

#ALEMANHA
summary(musicas_DE)
moda_loudness_DE <- calc_moda(musicas_DE$loudness)
moda_acousticness_DE <- calc_moda(musicas_DE$acousticness)
moda_danceability_DE <- calc_moda(musicas_DE$danceability)
moda_energy_DE <- calc_moda(musicas_DE$energy)
moda_instrumentalness_DE <- calc_moda(musicas_DE$instrumentalness)
moda_key_DE <- calc_moda(musicas_DE$key)
moda_mode_DE = calc_moda(musicas_DE$mode)
moda_time_signature_DE = calc_moda(musicas_DE$time_signature)
moda_liveness_DE <- calc_moda(musicas_DE$liveness)
moda_danceability_DE <- calc_moda(musicas_DE$danceability)
moda_speechiness_DE <- calc_moda(musicas_DE$speechiness)
moda_tempo_DE <- calc_moda(musicas_DE$tempo)
moda_valence_DE <- calc_moda(musicas_DE$valence)

#REINO UNIDO
summary(musicas_GB)
moda_loudness_GB <- calc_moda(musicas_GB$loudness)
moda_acousticness_GB <- calc_moda(musicas_GB$acousticness)
moda_danceability_GB <- calc_moda(musicas_GB$danceability)
moda_energy_GB <- calc_moda(musicas_GB$energy)
moda_instrumentalness_GB <- calc_moda(musicas_GB$instrumentalness)
moda_key_GB <- calc_moda(musicas_GB$key)
moda_mode_GB = calc_moda(musicas_GB$mode)
moda_time_signature_GB = calc_moda(musicas_GB$time_signature)
moda_liveness_GB <- calc_moda(musicas_GB$liveness)
moda_danceability_GB <- calc_moda(musicas_GB$danceability)
moda_speechiness_GB <- calc_moda(musicas_GB$speechiness)
moda_tempo_GB <- calc_moda(musicas_GB$tempo)
moda_valence_GB <- calc_moda(musicas_GB$valence)

#INDIA
summary(musicas_IN)
moda_loudness_IN <- calc_moda(musicas_IN$loudness)
moda_acousticness_IN <- calc_moda(musicas_IN$acousticness)
moda_danceability_IN <- calc_moda(musicas_IN$danceability)
moda_energy_IN <- calc_moda(musicas_IN$energy)
moda_instrumentalness_IN <- calc_moda(musicas_IN$instrumentalness)
moda_key_IN <- calc_moda(musicas_IN$key)
moda_mode_IN = calc_moda(musicas_IN$mode)
moda_time_signature_IN = calc_moda(musicas_IN$time_signature)
moda_liveness_IN <- calc_moda(musicas_IN$liveness)
moda_danceability_IN <- calc_moda(musicas_IN$danceability)
moda_speechiness_IN <- calc_moda(musicas_IN$speechiness)
moda_tempo_IN <- calc_moda(musicas_IN$tempo)
moda_valence_IN <- calc_moda(musicas_IN$valence)

# MX - México
summary(musicas_MX)
moda_loudness_MX <- calc_moda(musicas_MX$loudness)
moda_acousticness_MX <- calc_moda(musicas_MX$acousticness)
moda_danceability_MX <- calc_moda(musicas_MX$danceability)
moda_energy_MX <- calc_moda(musicas_MX$energy)
moda_instrumentalness_MX <- calc_moda(musicas_MX$instrumentalness)
moda_key_MX <- calc_moda(musicas_MX$key)
moda_mode_MX = calc_moda(musicas_MX$mode)
moda_time_signature_MX = calc_moda(musicas_MX$time_signature)
moda_liveness_MX <- calc_moda(musicas_MX$liveness)
moda_danceability_MX <- calc_moda(musicas_MX$danceability)
moda_speechiness_MX <- calc_moda(musicas_MX$speechiness)
moda_tempo_MX <- calc_moda(musicas_MX$tempo)
moda_valence_MX <- calc_moda(musicas_MX$valence)

#NG - Nigéria
summary(musicas_NG)
moda_loudness_NG <- calc_moda(musicas_NG$loudness)
moda_acousticness_NG <- calc_moda(musicas_NG$acousticness)
moda_danceability_NG <- calc_moda(musicas_NG$danceability)
moda_energy_NG <- calc_moda(musicas_NG$energy)
moda_instrumentalness_NG <- calc_moda(musicas_NG$instrumentalness)
moda_key_NG <- calc_moda(musicas_NG$key)
moda_mode_NG = calc_moda(musicas_NG$mode)
moda_time_signature_NG = calc_moda(musicas_NG$time_signature)
moda_liveness_NG <- calc_moda(musicas_NG$liveness)
moda_danceability_NG <- calc_moda(musicas_NG$danceability)
moda_speechiness_NG <- calc_moda(musicas_NG$speechiness)
moda_tempo_NG <- calc_moda(musicas_NG$tempo)
moda_valence_NG <- calc_moda(musicas_NG$valence)


# SG - Cingapura
summary(musicas_SG)
moda_loudness_SG <- calc_moda(musicas_SG$loudness)
moda_acousticness_SG <- calc_moda(musicas_SG$acousticness)
moda_danceability_SG <- calc_moda(musicas_SG$danceability)
moda_energy_SG <- calc_moda(musicas_SG$energy)
moda_instrumentalness_SG <- calc_moda(musicas_SG$instrumentalness)
moda_key_SG <- calc_moda(musicas_SG$key)
moda_mode_SG = calc_moda(musicas_SG$mode)
moda_time_signature_SG = calc_moda(musicas_SG$time_signature)
moda_liveness_SG <- calc_moda(musicas_SG$liveness)
moda_danceability_SG <- calc_moda(musicas_SG$danceability)
moda_speechiness_SG <- calc_moda(musicas_SG$speechiness)
moda_tempo_SG <- calc_moda(musicas_SG$tempo)
moda_valence_SG <- calc_moda(musicas_SG$valence)

# US - Estados Unidos
summary(musicas_US)
moda_loudness_US <- calc_moda(musicas_US$loudness)
moda_acousticness_US <- calc_moda(musicas_US$acousticness)
moda_danceability_US <- calc_moda(musicas_US$danceability)
moda_energy_US <- calc_moda(musicas_US$energy)
moda_instrumentalness_US <- calc_moda(musicas_US$instrumentalness)
moda_key_US <- calc_moda(musicas_US$key)
moda_mode_US = calc_moda(musicas_US$mode)
moda_time_signature_US = calc_moda(musicas_US$time_signature)
moda_liveness_US <- calc_moda(musicas_US$liveness)
moda_danceability_US <- calc_moda(musicas_US$danceability)
moda_speechiness_US <- calc_moda(musicas_US$speechiness)
moda_tempo_US <- calc_moda(musicas_US$tempo)
moda_valence_US <- calc_moda(musicas_US$valence)

# NZ - NOVA ZELÂNDIA
summary(musicas_NZ)
moda_loudness_NZ <- calc_moda(musicas_NZ$loudness)
moda_acousticness_NZ <- calc_moda(musicas_NZ$acousticness)
moda_danceability_NZ <- calc_moda(musicas_NZ$danceability)
moda_energy_NZ <- calc_moda(musicas_NZ$energy)
moda_instrumentalness_NZ <- calc_moda(musicas_NZ$instrumentalness)
moda_key_NZ <- calc_moda(musicas_NZ$key)
moda_mode_NZ = calc_moda(musicas_NZ$mode)
moda_time_signature_NZ = calc_moda(musicas_NZ$time_signature)
moda_liveness_NZ <- calc_moda(musicas_NZ$liveness)
moda_danceability_NZ <- calc_moda(musicas_NZ$danceability)
moda_speechiness_NZ <- calc_moda(musicas_NZ$speechiness)
moda_tempo_NZ <- calc_moda(musicas_NZ$tempo)
moda_valence_NZ <- calc_moda(musicas_NZ$valence)

#ZA - ÁFRICA DO SUL
summary(musicas_ZA)
moda_loudness_ZA <- calc_moda(musicas_ZA$loudness)
moda_acousticness_ZA <- calc_moda(musicas_ZA$acousticness)
moda_danceability_ZA <- calc_moda(musicas_ZA$danceability)
moda_energy_ZA <- calc_moda(musicas_ZA$energy)
moda_instrumentalness_ZA <- calc_moda(musicas_ZA$instrumentalness)
moda_key_ZA <- calc_moda(musicas_ZA$key)
moda_mode_ZA = calc_moda(musicas_ZA$mode)
moda_time_signature_ZA = calc_moda(musicas_ZA$time_signature)
moda_liveness_ZA <- calc_moda(musicas_ZA$liveness)
moda_danceability_ZA <- calc_moda(musicas_ZA$danceability)
moda_speechiness_ZA <- calc_moda(musicas_ZA$speechiness)
moda_tempo_ZA <- calc_moda(musicas_ZA$tempo)
moda_valence_ZA <- calc_moda(musicas_ZA$valence)




