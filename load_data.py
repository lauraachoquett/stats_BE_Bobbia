import pandas as pd
import requests

def download_data():
    # URL du dataset météo
    url = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/donnees-synop-essentielles-omm/exports/csv?lang=fr&qv1=(date%3A%5B2015-08-03T22%3A00%3A00Z%20TO%202025-09-03T21%3A59%3A59Z%5D)&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"

    # Nom du fichier local
    local_file = "meteo.csv"

    # Téléchargement et sauvegarde du fichier
    print("Téléchargement en cours...")
    response = requests.get(url)
    with open(local_file, "wb") as f:
        f.write(response.content)
    print(f"Fichier sauvegardé sous {local_file}")

def load_data():
    # Lecture du fichier sauvegardé avec pandas
    df = pd.read_csv("meteo.csv", delimiter=";")

    # Infos sur le dataset
    print(df.shape)
    print(df.columns)
    print(df.head())
    return df

if __name__ == "__main__":
    df = load_data()
    