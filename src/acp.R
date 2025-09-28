library(dplyr)

data <- read.csv("csv/observations_cleaned.csv",
                 sep = ",", header = TRUE,
                 na.strings = c("NA","NaN",""," "))

cols_to_remove <- c("Date", "ID.OMM.station","mois_de_l_annee","Temps.présent","Type.de.tendance.barométrique","Periode.de.mesure.de.la.rafale")
observations <- data %>% select(-any_of(cols_to_remove))

obs_num <- observations %>% select(where(is.numeric))

# Retirer les lignes avec les NA
obs_num <- obs_num %>% select(where(~ !all(is.na(.x))))
obs_num <- obs_num %>% select(where(~ var(.x, na.rm = TRUE) > 0))
obs_num <- obs_num %>% filter(if_all(everything(), ~ !is.na(.)))

# Faire l'ACP sur des données centrées et réduites
acp <- prcomp(obs_num, center = TRUE, scale. = TRUE)

# 6) Plots rapides
plot(acp, type = "l", main = "Scree plot (ACP)")
biplot(acp, cex = 0.6)

cat("\nVariance expliquée (PC1..):\n")
print(summary(acp)$importance[2, ])
print(acp$sdev)


