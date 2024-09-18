import spotipy
import json
import pandas as pd
from spotipy.oauth2 import SpotifyClientCredentials
from time import sleep

# Define credenciais do spotify, obtidas em "https://developer.spotify.com/dashboard" > clica num app > settings
client_id = ''
client_secret = ''

# Se autentica na API do Spotify
client_credentials_manager = SpotifyClientCredentials(client_id=client_id, client_secret=client_secret)
# Cria objeto para chamada dos endpoints com as credenciais já autenticadas
sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)

def get_music_table(country_code: str, continent_code: str, first_run: bool = False):
    """Cria a tabela de músicas, além de preencher algumas informações na tabela de artistas

    Args:
        country_code (str): Código do país. Usado para ler o arquivo de TOP100 correto na pasta input e inserir a sigla de país na tabela
        
        continent_code (str): Código do continente. Usado para inserir sigla do continene na tabela
        
        first_run (bool): Define se é a primeira vez que irá rodar. Se True, cria a tabela com header. Se False lê a tabela já salva.
    """
    if first_run:
        # Define colunas da tabela de artistas
        index_artists = ['id',
                        'name']
        # Cria tabela de artistas com as colunas acima
        artist_data = pd.DataFrame(columns=index_artists)
        # Define colunas para a tabela de músicas
        index_musics = ['id',
                        'name',
                        'country', 
                        'rank',
                        'continent', 
                        'loudness', 
                        'acousticness',
                        'danceability',
                        'energy',
                        'instrumentalness',
                        'key',
                        'liveness',
                        'mode',
                        'speechiness',
                        'tempo',
                        'time_signature',
                        'valence',
                        'explicit',
                        'artist_id'
                        ]
        # Cria tabela de músicas com as colunas acima
        music_data = pd.DataFrame(columns=index_musics)
    else:
        # Lê tabelas já existentes
        artist_data = pd.read_csv('./tabelas/artistas.csv')
        music_data = pd.read_csv('./tabelas/musicas.csv')
        
    
    # Abre arquivo com os IDs das músicas do TOP 200
    with open(f'./input/spotify_codes_{country_code}.json', 'r') as file:
        # Pega somente as 100 primeiras músicas
        lista_musica = json.load(file)['spotify_codes'][:100]

    
    for i, id_musica in enumerate(lista_musica):
        
        # Informações fixas
        country = country_code
        id = country + '-' + id_musica
        rank = i
        continent = continent_code
        
        # Faz chamada à API no endpoint "Get Track's Audio Features"
        audio_features = sp.audio_features(id_musica)
        
        # A partir do retorno da chamada acima, obtem os dados que queremos
        loudness = audio_features[0]['loudness']
        acousticness = audio_features[0]['acousticness']
        danceability = audio_features[0]['danceability']
        energy = audio_features[0]['energy']
        instrumentalness = audio_features[0]['instrumentalness']
        key = audio_features[0]['key']
        liveness = audio_features[0]['liveness']
        mode = audio_features[0]['mode']
        speechiness = audio_features[0]['speechiness']
        tempo = audio_features[0]['tempo']
        time_signature = audio_features[0]['time_signature']
        valence = audio_features[0]['valence']

        # Faz chamada à API no endpoint "Get Track"
        track_data = sp.track(id_musica)
        
        # A partir da última chamada, obtem dados de nome da música e alguns dados do artista
        music_name = track_data['name']
        artist_id = track_data['album']['artists'][0]['id']
        explicit = track_data['explicit']
        artist_name = track_data['album']['artists'][0]['name']
        
        # Cria registro na tabela de músicas
        music_row = [id, music_name, country, rank, continent, loudness, acousticness, danceability, energy, instrumentalness, key, liveness, mode, speechiness, tempo, time_signature, valence, explicit, artist_id]
        # Cria registro parcial na tabela de artistas
        artist_row = [artist_id, artist_name]
        
        # Salva registros na tabela
        music_data.loc[len(music_data)] = music_row
        artist_data.loc[len(artist_data)] = artist_row
        
    # Apaga duplicatas da tabela de artistas
    artist_data = artist_data.drop_duplicates(subset=['id'], keep='first')
    music_data = music_data.drop_duplicates(subset=['id'], keep='first')
    
    # Salva tabelas    
    artist_data.to_csv('./tabelas/artistas.csv', index=False, header=True)
    music_data.to_csv('./tabelas/musicas.csv', index=False, header=True)

# Comandos utilizados para criar tabelas de musicas e artistas 
#get_music_table('BR', 'SA', True)
#get_music_table('AT', 'EU')
#get_music_table('AU', 'OC')
#get_music_table('CA', 'NA')
#get_music_table('CH', 'EU')
#get_music_table('DE', 'EU')
#get_music_table('ES', 'EU')
#get_music_table('GB', 'EU')
#get_music_table('IE', 'EU')
#get_music_table('IN', 'AS')
#get_music_table('IS', 'EU')
#get_music_table('IT', 'EU')
#get_music_table('LU', 'EU')
#get_music_table('MX', 'NA')
#get_music_table('NG', 'AF')
#get_music_table('NL', 'EU')
#get_music_table('NZ', 'OC')
#get_music_table('NO', 'EU')
#get_music_table('PL', 'EU')
#get_music_table('SE', 'EU')
#get_music_table('SG', 'AS')
#get_music_table('US', 'NA')
#get_music_table('ZA', 'AF')


def get_artists_and_genre_data(first: bool = False):
    artist_data = pd.read_csv('./tabelas/artistas.csv')
    
    if first:
        # Define colunas da tabela de artistas
        index_genre_by_artis = ['id_genero',
                                'id_artista']
        # Cria tabela de artistas com as colunas acima
        genre_by_artist = pd.DataFrame(columns=index_genre_by_artis)
    else:
        genre_by_artist = pd.read_csv('./tabelas/generos.csv')

    
    # Lista para adicionar a popularidade de cada artista
    popularities = []
    
    # Iterando por cada artista
    for i in range(len(artist_data)):
        artist_id = artist_data.loc[i]['id']
        # Faz chamada à API no endpoint "Get Artist"
        artist_info = sp.artist(artist_id)
        # Espera 0.2 segundos. Necessário pois estava dando timeout por excesso de chamadas por segundo
        sleep(0.2)
        
        popularity = artist_info['popularity']
        popularities.append(popularity)
        
        # Relaciona gênero e artista na tabela nova
        genres = artist_info['genres']
        for genre in genres:
            genre_row = (genre, artist_id)
            genre_by_artist.loc[len(genre_by_artist)] = genre_row
    
    # Cria coluna de popularidade na tabela de artistas
    artist_data = artist_data.assign(popularity=popularities)
    
    # Salva tabelas
    genre_by_artist.to_csv('./tabelas/genero.csv', index=False, header=True)
    artist_data.to_csv('./tabelas/artistas.csv', index=False, header=True)
    
    
#get_artists_and_genre_data(True)


def get_muisc_data(first: bool = False):
    music_data = pd.read_csv('./tabelas/musicas.csv')

    album_names = []
    release_dates = []
    
    # Iterando por cada artista
    for i in range(len(music_data)):
        music_id = music_data.loc[i]['id'][3:]
        # Faz chamada à API no endpoint "Get Artist"
        track = sp.track(music_id)
        
        album_name = track['album']['name']
        release_date = track['album']['release_date']
        
        album_names.append(album_name)
        release_dates.append(release_date)
        
    
    # Cria coluna de popularidade na tabela de musicas
    music_data = music_data.assign(album_name=album_names)
    music_data = music_data.assign(release_date=release_dates)
    
    music_data.to_csv('./tabelas/musicas.csv', index=False, header=True)
    
    
get_muisc_data()
    