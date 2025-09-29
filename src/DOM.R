library(dplyr)
library(readr)

classify_outremer <- function(stations_csv = "csv/stations.csv",
                              out_csv = "csv/stations_outre_mer_labeled.csv",
                              save = TRUE) {

  # Boîte France métropolitaine (inclut la Corse)
  FR_META_LAT <- c(41.0, 51.7)
  FR_META_LON <- c(-5.8, 10.5)

  df <- read.csv(stations_csv, sep = ",", stringsAsFactors = FALSE) %>%
    mutate(
      Latitude  = as.numeric(Latitude),
      Longitude = as.numeric(Longitude)
    )

  # Marque métropole
  df <- df %>%
    mutate(
      in_metropole = !is.na(Latitude) & !is.na(Longitude) &
        Latitude  >= FR_META_LAT[1] & Latitude  <= FR_META_LAT[2] &
        Longitude >= FR_META_LON[1] & Longitude <= FR_META_LON[2]
    )

  # Attribution territoire
  df <- df %>%
    mutate(
      territoire = dplyr::case_when(
        # Antilles
        between(Latitude, 15.8, 16.6) & between(Longitude, -61.9, -61.0) ~ "Guadeloupe",
        between(Latitude, 14.3, 14.9) & between(Longitude, -61.3, -60.8) ~ "Martinique",
        between(Latitude, 18.00, 18.15) & between(Longitude, -63.20, -62.95) ~ "Saint-Martin",
        between(Latitude, 17.85, 17.95) & between(Longitude, -62.95, -62.80) ~ "Saint-Barthélemy",
        # Saint-Pierre-et-Miquelon
        between(Latitude, 46.7, 47.2) & between(Longitude, -56.6, -56.1) ~ "Saint-Pierre-et-Miquelon",
        # Guyane
        between(Latitude, 2.1, 6.0) & between(Longitude, -54.0, -51.6) ~ "Guyane",
        # Océan Indien
        between(Latitude, -21.4, -20.8) & between(Longitude, 55.2, 55.9) ~ "La Réunion",
        between(Latitude, -13.1, -12.6) & between(Longitude, 45.0, 45.4) ~ "Mayotte",
        # Pacifique
        between(Latitude, -23.0, -18.0) & between(Longitude, 163.5, 169.5) ~ "Nouvelle-Calédonie",
        between(Latitude, -28.0,  -7.0) & between(Longitude, -154.0, -134.0) ~ "Polynésie française",
        between(Latitude, -14.4, -13.1) & between(Longitude, -178.4, -176.1) ~ "Wallis-et-Futuna",
        # TAAF
        between(Latitude, -50.5, -37.0) & between(Longitude,  66.0,  78.0) ~ "TAAF (Kerguelen/Amsterdam/Crozet)",
        between(Latitude, -23.5, -10.5) & between(Longitude,  39.5,  55.5) ~ "TAAF (Éparses/Tromelin)",
        # Métropole
        in_metropole ~ "Métropole",
        # Sinon = on garde l’ID mais territoire inconnu
        TRUE ~ "Inconnu"
      )
    ) %>%
    mutate(
      outre_mer = territoire != "Métropole"
    )

  # Sélection finale
  outre_mer_df <- df %>%
    filter(outre_mer) %>%
    select(`ID.OMM.station`, Latitude, Longitude, Altitude,
           territoire, `department..name.`, `region..name.`)

  if (save) {
    write.csv(outre_mer_df, out_csv, row.names = FALSE)
  }

  message("Stations outre-mer trouvées: ", nrow(outre_mer_df))
  return(outre_mer_df)
}

# Exemple d'utilisation :
res <- classify_outremer("csv/stations.csv")
head(res)