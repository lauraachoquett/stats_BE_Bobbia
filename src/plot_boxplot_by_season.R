library(dplyr)
library(ggplot2)

plot_boxplot_by_season <- function(data, stations, variable) {
  # Vérification que la colonne Date est bien en format datetime
  if (!inherits(data$Date, "POSIXct") && !inherits(data$Date, "Date")) {
    data$Date <- as.POSIXct(data$Date, tz = "UTC")
  }
  
  # Vérifier si la variable existe
  if (!(variable %in% names(data))) {
    message(paste("La variable", variable, "n'existe pas dans le dataset."))
    return(NULL)
  }
  
  # Ajouter une colonne Saison
  data <- data %>%
    mutate(
      Mois = as.integer(format(Date, "%m")),
      Saison = case_when(
        Mois %in% c(12, 1, 2)  ~ "Hi",
        Mois %in% c(3, 4, 5)   ~ "Pr",
        Mois %in% c(6, 7, 8)   ~ "Ét",
        Mois %in% c(9, 10, 11) ~ "Au",
        TRUE ~ NA_character_
      )
    )
  
  # Filtrer uniquement les stations choisies
  subset_data <- data %>%
    filter(ID.OMM.station %in% stations, !is.na(Saison))
  
  if (nrow(subset_data) == 0) {
    message("Aucune donnée trouvée pour ces stations.")
    return(NULL)
  }
  
  # ✅ Boxplot saisonnier
  ggplot(subset_data, aes(x = Saison, y = .data[[variable]], fill = Saison)) +
    geom_boxplot(outlier.alpha = 0.3) +
    scale_fill_brewer(palette = "Set3") +
    labs(title = paste(variable),
         x = "Saison",
         y = variable) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none",
          axis.line = element_line(color = "black"))
}
