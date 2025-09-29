library(dplyr)
library(ggplot2)

plot_station_daily_avg_by_month <- function(data, mois, stations, variable) {
  # Vérification que la colonne Date est bien en format datetime
  if (!inherits(data$Date, "POSIXct") && !inherits(data$Date, "Date")) {
    data$Date <- as.POSIXct(data$Date, tz = "UTC")
  }
  
  # Vérifier si la variable existe
  if (!(variable %in% names(data))) {
    message(paste("La variable", variable, "n'existe pas dans le dataset."))
    return(NULL)
  }
  
  # Filtrer sur le mois et les stations choisies
  subset_data <- data %>%
    filter(as.integer(format(Date, "%m")) == mois,
           ID.OMM.station %in% stations)
  
  if (nrow(subset_data) == 0) {
    message("Aucune donnée trouvée pour ce mois et ces stations.")
    return(NULL)
  }
  
  # Calcul de la moyenne journalière par station
  daily_avg <- subset_data %>%
    mutate(Jour = as.integer(format(Date, "%d"))) %>%
    group_by(ID.OMM.station, Jour) %>%
    summarise(valeur = mean(.data[[variable]], na.rm = TRUE), .groups = "drop")
  
  # Plot : une courbe par station
  ggplot(daily_avg, aes(x = Jour, y = valeur, color = factor(ID.OMM.station))) +
  geom_line() +
  geom_point() +
  labs(title = paste("Moyenne journalière de", variable, "en mois", mois),
       x = "Jour du mois",
       y = variable,
       color = "Station") +
  scale_x_continuous(breaks = 1:31) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black")
  )


}
# data_cleaned <- read.csv("../data/observations_cleaned.csv", header = TRUE, sep = ",")