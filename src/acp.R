library(dplyr)

data <- read.csv("csv/observations_cleaned.csv",
                 sep = ",", header = TRUE,
                 na.strings = c("NA","NaN",""," "))

cols_to_remove <- c("Date", "ID.OMM.station","mois_de_l_annee","Temps.présent")
observations <- data %>% select(-any_of(cols_to_remove))

obs_num <- observations %>% select(where(is.numeric))

obs_num <- obs_num %>% select(where(~ !all(is.na(.x))))
obs_num <- obs_num %>% select(where(~ var(.x, na.rm = TRUE) > 0))
obs_num <- obs_num %>% filter(if_all(everything(), ~ !is.na(.)))

acp <- prcomp(obs_num, center = TRUE, scale. = TRUE)

# 6) Plots rapides
plot(acp, type = "l", main = "Scree plot (ACP)")

biplot(acp, cex = 0.6)

cat("\nVariance expliquée (PC1..):\n")
print(summary(acp)$importance[2, ])
print(acp$sdev)



## Matrice de corrélation
library(ggplot2)
library(reshape2)

cor_mat <- cor(obs_num, use = "pairwise.complete.obs")

library(dplyr)

# Convertir en data.frame long
cor_melt <- reshape2::melt(cor_mat)

# Supprimer la diagonale et la moitié supérieure
cor_melt <- cor_melt %>%
  filter(as.numeric(Var1) > as.numeric(Var2))

# Plot
p <- ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Corrélation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  coord_fixed()

ggsave("heatmap_correlation.png", p, width = 8, height = 6, dpi = 300)

cm <- cor(obs_num, use = "pairwise.complete.obs")

# On enlève la diagonale et le triangle supérieur
cm[upper.tri(cm, diag = TRUE)] <- NA

# Indices des paires restantes (triangle inférieur)
idx <- which(!is.na(cm), arr.ind = TRUE)

# Tableau des paires avec NOMS + corrélation
pairs_corr <- data.frame(
  Var1 = rownames(cm)[idx[, 1]],
  Var2 = colnames(cm)[idx[, 2]],
  Corr = cm[idx]
)

# Trie décroissant (corrélation positive d'abord)
pairs_corr <- pairs_corr[order(-pairs_corr$Corr), ]

# OU si tu veux trier par |corr|
# pairs_corr <- pairs_corr[order(-abs(pairs_corr$Corr)), ]

# Afficher les 20 plus fortes
head(pairs_corr, 20)