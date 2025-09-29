#!/usr/bin/env Rscript

library(dplyr)
library(ggplot2)

analyze_correlations <- function(file_path, cor_save_path, heatmap_path = "heatmap.png") {
  # Lecture + nettoyage minimal
  data <- read.csv(
    file_path,
    sep = ",", header = TRUE,
    na.strings = c("NA","NaN",""," "),
    strip.white = TRUE
  )
  
  cols_to_remove <- c("Date","ID.OMM.station","mois_de_l_annee",
                      "Temps.présent","Type.de.tendance.barométrique",
                      "Periode.de.mesure.de.la.rafale")
  
  obs_num <- data %>%
    select(-any_of(cols_to_remove)) %>%
    mutate(across(where(is.numeric),
                  ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))) %>%
    select(where(~ var(.x, na.rm = TRUE) > 0))
  
  # Matrice de corrélation
  cor_mat <- cor(obs_num, use = "pairwise.complete.obs")
  
  # Sauvegarde de la matrice
  saveRDS(cor_mat, cor_save_path)   # ou write.csv(cor_mat, cor_save_path)
  
  # Triangle inférieur sans diagonale -> format long
  cor_long <- as.data.frame(as.table(cor_mat)) %>%
    rename(Var1 = Var1, Var2 = Var2, Corr = Freq) %>%
    filter(Var1 != Var2) %>%
    mutate(
      Var1 = factor(Var1, levels = colnames(cor_mat)),
      Var2 = factor(Var2, levels = colnames(cor_mat))
    ) %>%
    filter(as.numeric(Var1) > as.numeric(Var2))
  
  # Heatmap
  p <- ggplot(cor_long, aes(Var1, Var2, fill = Corr)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limits = c(-1, 1), name = "Corrélation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    coord_fixed() +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE)
  
  ggsave(heatmap_path, p, width = 8, height = 6, dpi = 300)
  
  # Tableau trié
  pairs_corr <- cor_long %>% arrange(desc(Corr))
  
  # Sauvegarde des paires corrélées
  write.csv(pairs_corr, file = paste0(tools::file_path_sans_ext(cor_save_path), "_pairs.csv"),
            row.names = FALSE)
}

# ------------------------
# Partie exécutable
# ------------------------

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Usage: Rscript analyze_corr.R <input_csv> <output_rds> [heatmap.png]")
}

file_path <- args[1]
cor_save_path <- args[2]
heatmap_path <- ifelse(length(args) >= 3, args[3], "heatmap.png")

analyze_correlations(file_path, cor_save_path, heatmap_path)