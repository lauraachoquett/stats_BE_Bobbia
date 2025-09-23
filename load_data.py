import pandas as pd
import requests
from tqdm import tqdm

def download_data():

    url = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/donnees-synop-essentielles-omm/exports/csv?lang=fr&qv1=(date%3A%5B2015-08-03T22%3A00%3A00Z%20TO%202025-09-03T21%3A59%3A59Z%5D)&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"
    local_file = "meteo.csv"
    chunk_size = 1024 * 1024  # 1 MiB

    with requests.get(url, stream=True) as r:
        r.raise_for_status()
        total = int(r.headers.get("content-length", 0))
        with open(local_file, "wb") as f, tqdm(
            total=total, unit="iB", unit_scale=True, unit_divisor=1024
        ) as bar:
            for chunk in r.iter_content(chunk_size=chunk_size):
                f.write(chunk)
                bar.update(len(chunk))

    print("Téléchargement terminé :", local_file)

def load_data():
    # Lecture du fichier sauvegardé avec pandas
    df = pd.read_csv("meteo.csv", delimiter=";")

    # Infos sur le dataset
    print("Shape :",df.shape)
    print("Colonne name :",df.columns)
    print("Head :",df.head())
    return df

if __name__ == "__main__":
    load_data()
    