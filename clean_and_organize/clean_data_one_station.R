library(dplyr)

# Charger les données
data <- read.csv("data/meteo.csv", sep = ";", nrows = 10000)

# 1) Colonnes à exclure (tu les avais déjà identifiées)
cols_to_remove <- c(
  "Type.de.tendance.barométrique.1",
  "Temps.passé.1.1",
  "Temps.présent.1",
  "Phénomène.spécial.1",
  "Phénomène.spécial.2",
  "Phénomène.spécial.3",
  "Coordonnees",
  "Nom"
)

# 2) Colonnes "station" à mettre dans une base séparée
station_cols <- c(
  "ID.OMM.station", "Latitude", "Longitude", "Altitude",
  "communes..name.", "communes..code.", "EPCI..name.", "EPCI..code.",
  "department..name.", "department..code.", "region..name.", "region..code."
)

# 3) Base filtrée pour la station 7005 (observations)
station_7005_obs <- data %>%
  filter(ID.OMM.station == 7005) %>%
  select(-all_of(cols_to_remove)) %>%
  select(-all_of(setdiff(
    intersect(names(.), station_cols[-1]),
    c("Latitude", "Longitude", "Altitude")
  )))
# 4) Base métadonnées pour la station 7005
station_7005_meta <- data %>%
  filter(ID.OMM.station == 7005) %>%
  select(all_of(intersect(names(.), station_cols))) %>%
  distinct()   # au cas où plusieurs lignes dupliquent les infos station

# 5) Sauvegardes
# write.csv(station_7005_obs, "station_7005_obs.csv", row.names = FALSE)
# write.csv(station_7005_meta, "station_7005_meta.csv", row.names = FALSE)
write.csv(station_7005_obs, "data/station_7005_obs.csv", row.names = FALSE)
write.csv(station_7005_meta, "data/station_7005_meta.csv", row.names = FALSE)

cat("Observations :", nrow(station_7005_obs), "lignes et", ncol(station_7005_obs), "colonnes\n")
cat("Métadonnées  :", nrow(station_7005_meta), "lignes et", ncol(station_7005_meta), "colonnes\n")