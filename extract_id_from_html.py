import re
import json


def get_id(country_code:str):
    # Define o caminho do html a ser lido
    file_path = f'./htmls/{country_code}.html'

    # Lê o HTML
    with open(file_path, 'r', encoding='utf-8') as file:
        html_content = file.read()

    # ReGex para procurar por ids nos links do spotify (e.g., 0bPP5cDG1ZnbAVCEa3ZbQ1)
    pattern = r'spotify.com/track/([a-zA-Z0-9]+)'

    # Procura pelos ids a partir do regex acima
    spotify_codes = list(re.findall(pattern, html_content))

    
    # Exclui os ids duplicados
    aux = 0
    for i in range(len(spotify_codes)):
        if i%2 == 0:
            spotify_codes.pop(aux)
            aux+=1

    # Converte a lista de ids em um JSON
    json_data = json.dumps({"spotify_codes": spotify_codes}, indent=4)

    #Define o caminho de gravação do JSON
    output_file_path = f'./input/spotify_codes_{country_code}.json'
    
    # Salva o JSON em um arquivo
    with open(output_file_path, 'w', encoding='utf-8') as output_file:
        output_file.write(json_data)
        
countries = ['AT', 'AU', 'CA', 'CH', 'DE', 'ES', 'GB', 'IE', 'IN', 'IS', 'IT', 'LU', 'MX', 'NL', 'NO', 'PL', 'SE', 'SG', 'US',]

for country_code in countries:
    get_id(country_code)