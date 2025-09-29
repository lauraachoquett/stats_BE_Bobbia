# 1) Lecture (ok) – on capte déjà pas mal de cas
data <- read.csv(
  "csv/station_7591_obs.csv",
  header = TRUE,
  sep = ",",
  na.strings = c("NA", "NaN", "", " ", "-", "None", "NULL", "N/A")
)

# 2) Normaliser les colonnes texte : trim, enlever espaces insécables, re-transformer "NA" -> NA
data[] <- lapply(data, function(col) {
  if (is.factor(col)) col <- as.character(col)
  if (is.character(col)) {
    col <- gsub("\u00A0", " ", col, useBytes = TRUE) # NBSP -> espace normal
    col <- gsub("\t", " ", col, useBytes = TRUE)     # tab -> espace
    col <- trimws(gsub(" +", " ", col))              # squeeze + trim
    low <- tolower(col)
    col[low %in% c("na","nan","n/a","none","null","-","")] <- NA_character_
  }
  col
})

# 3) (Option) Convertir automatiquement les colonnes texte qui sont numériques en vrai numeric
looks_numeric <- function(x) all(is.na(x) | grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", x))
data[] <- lapply(data, function(col) {
  if (is.character(col) && looks_numeric(col)) as.numeric(col) else col
})

# 4) Diagnostic NA par colonne
missing_values <- colSums(is.na(data))
cat("NA per column:\n"); print(missing_values)
cat("\n% NA per column:\n"); print(round(100*missing_values/nrow(data),2))

# 5) Drop colonnes >30% NA
threshold <- 0.3 * nrow(data)
cols_to_drop <- names(missing_values[missing_values > threshold])
data_cleaned <- data[, !(names(data) %in% cols_to_drop), drop = FALSE]
cat("\nColonnes supprimées (>30% NA):\n"); print(cols_to_drop)

# 6) Sauvegarde
write.csv(data_cleaned, "csv/station_7591_obs_cleaned.csv", row.names = FALSE)