library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# --- Lecture ---
data <- read.csv(
  "csv/observations_cleaned.csv",
  sep = ",", header = TRUE,
  na.strings = c("NA","NaN",""," "),
  strip.white = TRUE
)

# --- Dates ---
data <- data %>%
  mutate(Date = ymd_hms(Date))  # gère ISO + décalage horaire si présent

# --- Calcul des gaps entre mesures par station ---
# 1) Ordonner par station puis date
# 2) Calculer la date précédente et l'écart (en heures)
by_station <- data %>%
  arrange(`ID.OMM.station`, Date) %>%
  group_by(`ID.OMM.station`) %>%
  mutate(
    prev_date = lag(Date),
    gap_hours = as.numeric(difftime(Date, prev_date, units = "hours"))
  ) %>%
  ungroup()

# --- Ligne de chaque station correspondant au plus grand trou ---
# (on récupère aussi les timestamps encadrant ce trou)
max_gaps <- by_station %>%
  filter(!is.na(gap_hours)) %>%
  group_by(`ID.OMM.station`) %>%
  slice_max(order_by = gap_hours, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    ID_OMM_station = `ID.OMM.station`,
    max_gap_hours = gap_hours,
    max_gap_days  = gap_hours / 24,
    gap_start     = prev_date,  # date de la dernière mesure avant le trou
    gap_end       = Date        # date de la prochaine mesure après le trou
  )

# --- Stats complémentaires par station (utile pour diagnostiquer la qualité) ---
stats_station <- by_station %>%
  group_by(`ID.OMM.station`) %>%
  summarise(
    n_obs              = sum(!is.na(Date)),
    median_gap_hours   = median(gap_hours, na.rm = TRUE),
    mean_gap_hours     = mean(gap_hours,   na.rm = TRUE),
    p90_gap_hours      = quantile(gap_hours, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(ID_OMM_station = `ID.OMM.station`)

# --- Jointure pour un tableau final clair ---
res_station <- max_gaps %>%
  left_join(stats_station, by = "ID_OMM_station") %>%
  arrange(desc(max_gap_hours))

# Afficher les 10 stations avec le plus gros trou
print(head(res_station, 10))

# Facultatif : exporter en CSV
write.csv(res_station, "results/stations_max_gaps.csv", row.names = FALSE)

# --- (Optionnel) Visualiser les 15 plus gros trous en jours ---
top15 <- res_station %>% slice_max(order_by = max_gap_days, n = 15)
ggplot(top15, aes(x = reorder(ID_OMM_station, max_gap_days), y = max_gap_days)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Plus grandes durées entre deux mesures par station",
       x = "Station (ID OMM)", y = "Durée max entre mesures (jours)") +
  theme_minimal(base_size = 12)
ggsave("fig/max_gaps_par_station.png", width = 8, height = 6, dpi = 300)