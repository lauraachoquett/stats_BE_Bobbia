library(dplyr)

# 0) Charger TOUTES les données (enlève nrows=10000)
data <- read.csv("meteo.csv", sep = ";",nrows=1000)

# 1) Colonnes à retirer (parasites / doublons)
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

# 2) Colonnes "station" (métadonnées fixes par station)
station_cols <- c(
  "ID.OMM.station", "Latitude", "Longitude", "Altitude",
  "communes..name.", "communes..code.", "EPCI..name.", "EPCI..code.",
  "department..name.", "department..code.", "region..name.", "region..code."
)

# Sécuriser: ne garder que les colonnes qui existent réellement
cols_to_remove_present <- intersect(names(data), cols_to_remove)
station_cols_present   <- intersect(names(data), station_cols)

# --- A) Table STATIONS (1 ligne par station) -------------------------------

# Règle d'agrégation: garder la première valeur non-NA par station pour chaque colonne
stations <- data %>%
  select(all_of(station_cols_present)) %>%
  group_by(ID.OMM.station) %>%
  summarise(
    across(
      everything(),
      ~ { u <- unique(na.omit(.x)); if (length(u) == 0) NA else u[1] },
      .names = "{.col}"
    ),
    .groups = "drop"
  )

# --- B) Table OBSERVATIONS (toutes les mesures) ----------------------------
# On garde ID + Lat/Lon/Alt dans les observations; on enlève le reste des métadonnées station
keep_in_obs <- intersect(names(data), c("ID.OMM.station", "Latitude", "Longitude", "Altitude"))
drop_from_obs <- setdiff(station_cols_present, keep_in_obs)

observations <- data %>%
  # retirer colonnes parasites
  select(-all_of(cols_to_remove_present)) %>%
  # retirer métadonnées station sauf ID, Lat, Lon, Alt
  select(-all_of(drop_from_obs))

# --- C) Écritures -----------------------------------------------------------
write.csv(stations,     "stations.csv",     row.names = FALSE)
write.csv(observations, "observations.csv", row.names = FALSE)

# --- D) Checks rapides ------------------------------------------------------
cat("Stations uniques     :", nrow(stations), "\n")
cat("IDs uniques (source) :", dplyr::n_distinct(data$ID.OMM.station), "\n")
cat("Observations lignes  :", nrow(observations), "\n")
cat("Colonnes observations:", ncol(observations), "\n")

# Exemple de jointure si besoin plus tard:
# observations_enrichies <- observations %>% left_join(stations, by = "ID.OMM.station")