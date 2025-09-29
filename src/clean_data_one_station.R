#!/usr/bin/env Rscript

library(dplyr)

extract_station <- function(file_path, station_id, out_dir = "csv") {
  # Lecture
  data <- read.csv(file_path, sep = ";")

  # Colonnes à exclure
cols_to_remove <- c(
  "Type.de.tendance.barométrique.1",
  "Temps.passé.1.1",
  "Temps.présent.1",
  "Phénomène.spécial.1",
  "Phénomène.spécial.2",
  "Phénomène.spécial.3",
  "Coordonnees",
    "Nom",
    "Température...C."
)

  # Colonnes station
station_cols <- c(
  "ID.OMM.station", "Latitude", "Longitude", "Altitude",
  "communes..name.", "communes..code.", "EPCI..name.", "EPCI..code.",
  "department..name.", "department..code.", "region..name.", "region..code."
)

  # Observations
  station_obs <- data %>%
    filter(ID.OMM.station == station_id) %>%
    select(-any_of(cols_to_remove)) %>%
    select(-all_of(intersect(names(.), station_cols[-1])))
  
  # Métadonnées
  station_meta <- data %>%
    filter(ID.OMM.station == station_id) %>%
  select(all_of(intersect(names(.), station_cols))) %>%
    distinct()
  
  # Fichiers de sortie (automatiques)
  out_obs <- file.path(out_dir, paste0("station_", station_id, "_obs.csv"))
  out_station <- file.path(out_dir, paste0("station_", station_id, "_station.csv"))
  
  write.csv(station_obs, out_obs, row.names = FALSE)
  write.csv(station_meta, out_station, row.names = FALSE)

  cat("Station", station_id, "\n")
  cat("Observations :", nrow(station_obs), "lignes et", ncol(station_obs), "colonnes →", out_obs, "\n")
  cat("Métadonnées  :", nrow(station_meta), "lignes et", ncol(station_meta), "colonnes →", out_station, "\n")
}

# --- Partie exécutable ---
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Usage: Rscript extract_station.R <station_id>")
}

station_id <- as.integer(args[1])

# fichier source fixe + dossier sortie
file_path <- "csv/meteo.csv"
out_dir <- "csv"

extract_station(file_path, station_id, out_dir)