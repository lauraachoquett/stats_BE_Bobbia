library(dplyr)
library(ggplot2)

plot_monthly_avg <- function(data, station_id, variable) {
  # Vérification que la colonne Date est bien en format datetime
  if (!inherits(data$Date, "POSIXct") && !inherits(data$Date, "Date")) {
    data$Date <- as.POSIXct(data$Date, tz = "UTC")
  }
  
  # Filtrer par station
  station_data <- data %>% 
    filter(ID.OMM.station == station_id)
  
  if (nrow(station_data) == 0) {
    message(paste("Aucune donnée trouvée pour la station", station_id))
    return(NULL)
  }
  
  # Vérifier si la variable existe
  if (!(variable %in% names(station_data))) {
    message(paste("La variable", variable, "n'existe pas dans le dataset."))
    return(NULL)
  }
  
  # Calcul des moyennes mensuelles (sans lubridate)
  monthly_avg <- station_data %>%
    mutate(Mois = as.integer(format(Date, "%m"))) %>%
    group_by(Mois) %>%
    summarise(valeur = mean(.data[[variable]], na.rm = TRUE))
  
  # Plot avec ggplot2
  ggplot(monthly_avg, aes(x = Mois, y = valeur)) +
    geom_line(color = "steelblue") +
    geom_point(size = 2, color = "darkred") +
    scale_x_continuous(breaks = 1:12) +
    labs(title = paste("Moyenne mensuelle de", variable, "- Station", station_id),
         x = "Mois",
         y = variable) +
    theme_minimal()
}
# data_cleaned <- read.csv("../data/observations_cleaned.csv", header = TRUE, sep = ",")
# plot_monthly_avg(data_cleaned, station_id = 7139, variable = "Température")
# stations_a_comparer <- c(7139, 7149, 7690)
# # Plot de la température moyenne en avril (mois = 4)
# plot_station_daily_avg_by_month(data_cleaned, mois = 4, stations = stations_a_comparer, variable = "Température")