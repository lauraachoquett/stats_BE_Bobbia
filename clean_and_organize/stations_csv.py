import csv
import os

# Chemin vers le fichier CSV de base
fichier_base = "/home/jmaubian/SDD/Projet_stat_meteo/observations.csv"

# Dossier pour stocker les fichiers par station
dossier_sortie = "/home/jmaubian/SDD/Projet_stat_meteo/stations_csv"
os.makedirs(dossier_sortie, exist_ok=True)

# Dictionnaire pour stocker les fichiers ouverts
fichiers_stations = {}

# Lecture ligne par ligne pour économiser la RAM
with open(fichier_base, newline='', encoding='utf-8') as csvfile:
    reader = csv.DictReader(csvfile)
    for ligne in reader:
        station_id = ligne['ID.OMM.station']
        
        # Si le fichier pour cette station n'existe pas encore, on le crée
        if station_id not in fichiers_stations:
            nom_fichier = os.path.join(dossier_sortie, f"station_{station_id}.csv")
            f = open(nom_fichier, 'w', newline='', encoding='utf-8')
            writer = csv.DictWriter(f, fieldnames=reader.fieldnames)
            writer.writeheader()
            fichiers_stations[station_id] = (f, writer)
        
        # Écrire la ligne dans le fichier correspondant
        fichiers_stations[station_id][1].writerow(ligne)

# Fermer tous les fichiers ouverts
for f, writer in fichiers_stations.values():
    f.close()

print("Fichiers créés pour chaque station dans le dossier 'stations_csv'.")
