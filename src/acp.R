pca_with_season_plots <- function(file_path,
                                  cols_to_remove = c("Date","ID.OMM.station","mois_de_l_annee",
                                                     "Temps.présent","Type.de.tendance.barométrique",
                                                     "Periode.de.mesure.de.la.rafale","mois"),
                                  seuil_distance = 0.3,
                                  scale_factor = 3,
                                  sample_max = 5000,
                                  seed = 123,
                                  output_dir = NULL,
                                  prefix = "acp_") {
  # --- Packages ---
  stopifnot(requireNamespace("dplyr", quietly = TRUE),
            requireNamespace("ggplot2", quietly = TRUE),
            requireNamespace("lubridate", quietly = TRUE),
            requireNamespace("grid", quietly = TRUE))
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(grid) # pour unit()

  # --- Lecture & nettoyage minimal ---
  data <- read.csv(
    file_path,
    sep = ",", header = TRUE,
    na.strings = c("NA","NaN",""," "),
    strip.white = TRUE
  )

  # --- Dates & saison ---
  data <- data %>%
    mutate(Date = ymd_hms(Date)) %>%
    mutate(mois = month(Date),
           saison = case_when(
             mois %in% c(12, 1, 2) ~ "Hiver",
             mois %in% c(3, 4, 5) ~ "Printemps",
             mois %in% c(6, 7, 8) ~ "Été",
             mois %in% c(9, 10, 11) ~ "Automne",
             TRUE ~ NA_character_
           )) %>%
    filter(!is.na(saison))
data <- data %>%
    filter(nchar(as.character(ID.OMM.station)) == 4 & grepl("^[0-9]+$", ID.OMM.station))

  # --- Matrice numérique : imputation moyenne + retrait des constantes ---
  obs_num <- data %>%
    select(-dplyr::any_of(cols_to_remove)) %>%
    mutate(across(where(is.numeric),
                  ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))) %>%
    select(where(~ is.numeric(.x) && !is.na(var(.x, na.rm = TRUE)) && var(.x, na.rm = TRUE) > 0))

  stopifnot(ncol(obs_num) >= 2, nrow(obs_num) >= 3)

  # --- ACP (centrée-réduite) ---
  acp <- prcomp(obs_num, center = TRUE, scale. = TRUE)

  # --- Scree plot (ggplot) ---
  var_exp <- acp$sdev^2
  var_exp_pct <- var_exp / sum(var_exp)
  scree_df <- data.frame(
    PC = factor(paste0("PC", seq_along(var_exp)), levels = paste0("PC", seq_along(var_exp))),
    pct = as.numeric(var_exp_pct)
  )
  p_scree <- ggplot(scree_df, aes(x = PC, y = pct)) +
    geom_line(group = 1) +
    geom_point() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal() +
    labs(title = "Scree plot (ACP)", x = "Composantes principales", y = "% variance expliquée")

  # --- Loadings PC1/PC2 ---
  loadings <- acp$rotation[, 1:2, drop = FALSE]
  loadings_df <- as.data.frame(loadings)
  names(loadings_df) <- c("PC1","PC2")
  loadings_df$var <- rownames(loadings_df)

  # --- Groupes par distance euclidienne sur les loadings ---
  dist_matrix <- dist(loadings, method = "euclidean")
  hc <- hclust(dist_matrix, method = "complete")
  groupes <- cutree(hc, h = seuil_distance)
  loadings_df$groupe <- as.factor(groupes)

  centroides <- loadings_df %>%
    group_by(groupe) %>%
    summarise(
      PC1 = mean(PC1),
      PC2 = mean(PC2),
      vars = paste(var, collapse = ", "),
      n_vars = dplyr::n(),
      .groups = "drop"
    )
  plot(hc, main = "Dendrogramme des variables")
  abline(h = seuil_distance, col = "red", lty = 2)

out_file <- paste0(prefix, "groupes.txt")

capture.output({
  cat("\n=== Détail des groupes ===\n")
  for (g in unique(groupes)) {
    cat("\nGroupe", g, ":\n")
    vars_g <- loadings_df$var[loadings_df$groupe == g]
    cat(paste(vars_g, collapse = "\n"), "\n")
  }
}, file = out_file)

  # --- Scores des observations (PC1/PC2) ---
  scores <- as.data.frame(acp$x[, 1:2, drop = FALSE])
  names(scores) <- c("PC1","PC2")
  scores$saison <- data$saison

  # --- Échantillonnage pour lisibilité ---
  set.seed(seed)
  n_echantillon <- min(sample_max, nrow(scores))
  idx <- sample(nrow(scores), n_echantillon)
  scores_sample <- scores[idx, , drop = FALSE]

  # --- Palette saison ---
  pal_saison <- c(
    "Hiver" = "#3498db",
    "Printemps" = "#2ecc71",
    "Été" = "#e74c3c",
    "Automne" = "#f39c12"
  )

  # --- Plot global (toutes saisons) ---
  p_all <- ggplot() +
    geom_point(data = scores_sample,
               aes(x = PC1, y = PC2, color = saison),
               alpha = 0.3, size = 0.8) +
    geom_segment(data = centroides,
                 aes(x = 0, y = 0, xend = PC1 * scale_factor, yend = PC2 * scale_factor),
                 arrow = arrow(length = unit(0.3, "cm")),
                 linewidth = 1.5, color = "black") +
    geom_text(data = centroides,
              aes(x = PC1 * scale_factor * 1.1,
                  y = PC2 * scale_factor * 1.1,
                  label = paste0("G", groupe, " (n=", n_vars, ")")),
              size = 3.5, fontface = "bold", color = "black") +
    scale_color_manual(values = pal_saison) +
    theme_minimal() +
    labs(title = "Projection ACP : observations par saison et groupes de variables",
         subtitle = paste("Échantillon de", n_echantillon, "observations sur", nrow(scores)),
         x = paste0("PC1 (", round(100 * summary(acp)$importance[2, 1], 1), "%)"),
         y = paste0("PC2 (", round(100 * summary(acp)$importance[2, 2], 1), "%)"),
         color = "Saison") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
    theme(legend.position = "right")

    # --- Un plot par saison (avec code couleur global) ---
    saisons_ord <- c("Hiver","Printemps","Été","Automne")
    plots_saison <- lapply(saisons_ord, function(s) {
    df_s <- scores_sample %>% dplyr::filter(saison == s)
    ggplot() +
        geom_point(data = df_s,
                aes(x = PC1, y = PC2, color = saison),
                alpha = 0.35, size = 0.9) +
        geom_segment(data = centroides,
                    aes(x = 0, y = 0,
                        xend = PC1 * scale_factor,
                        yend = PC2 * scale_factor),
                    arrow = arrow(length = unit(0.25, "cm")),
                    linewidth = 1, color = "black") +
        geom_text(data = centroides,
                aes(x = PC1 * scale_factor * 1.08,
                    y = PC2 * scale_factor * 1.08,
                    label = paste0("G", groupe)),
                size = 3.2, fontface = "bold") +
        scale_color_manual(values = pal_saison, drop = FALSE) +  # <-- garde toutes les couleurs
        theme_minimal() +
        labs(title = paste0("Projection ACP – ", s),
            subtitle = paste0("Échantillon de ", nrow(df_s), " / ", nrow(scores), " observations"),
            x = paste0("PC1 (", round(100 * summary(acp)$importance[2, 1], 1), "%)"),
            y = paste0("PC2 (", round(100 * summary(acp)$importance[2, 2], 1), "%)"),
            color = "Saison") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
        theme(legend.position = "right")
    })
    names(plots_saison) <- saisons_ord

  # --- Cercle de corrélation (loadings + centroides) ---
  p_circle <- ggplot() +
    geom_segment(data = loadings_df,
                 aes(x = 0, y = 0, xend = PC1, yend = PC2, color = groupe),
                 arrow = arrow(length = unit(0.2, "cm")),
                 alpha = 0.45, linewidth = 0.5) +
    geom_segment(data = centroides,
                 aes(x = 0, y = 0, xend = PC1, yend = PC2, color = groupe),
                 arrow = arrow(length = unit(0.4, "cm")),
                 linewidth = 2) +
    geom_text(data = centroides,
              aes(x = PC1, y = PC2, label = paste0("G", groupe, " (n=", n_vars, ")")),
              hjust = -0.2, size = 4, fontface = "bold") +
    annotate("path",
             x = cos(seq(0, 2*pi, length.out = 200)),
             y = sin(seq(0, 2*pi, length.out = 200)),
             color = "grey50", linetype = "dashed") +
    xlim(-1.2, 1.2) + ylim(-1.2, 1.2) +
    coord_fixed() +
    theme_minimal() +
    labs(title = "Cercle de corrélation (PC1–PC2) avec regroupement",
         subtitle = paste("Seuil distance :", seuil_distance)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
    theme(legend.position = "none")

  # --- Sauvegardes si demandé ---
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    ggsave(file.path(output_dir, paste0(prefix, "scree.png")), p_scree, width = 8, height = 6, dpi = 150)
    ggsave(file.path(output_dir, paste0(prefix, "observations_saisons.png")), p_all, width = 12, height = 8, dpi = 150)
    ggsave(file.path(output_dir, paste0(prefix, "cercle_correlation.png")), p_circle, width = 10, height = 8, dpi = 150)
    # Un fichier par saison
    for (s in names(plots_saison)) {
      ggsave(file.path(output_dir, paste0(prefix, "projection_", s, ".png")),
             plots_saison[[s]], width = 10, height = 7, dpi = 150)
    }
    # Optionnel : écrire un petit CSV des groupes
    readr::write_csv(centroides, file.path('results/', paste0(prefix, "groupes_centroides.csv")))
  }


  # --- Retour ---
  list(
    acp = acp,
    variance_pc1_pc2 = summary(acp)$importance[2, 1:2],
    loadings = loadings_df,
    centroides = centroides,
    groupes = groupes,
    plots = list(
      scree = p_scree,
      all_seasons = p_all,
      by_season = plots_saison,
      circle = p_circle
    )
  )
}

res <- pca_with_season_plots(
  file_path = "csv/observations_cleaned.csv",
  seuil_distance = 0.2,
  output_dir = "fig",      # ou NULL pour ne pas sauvegarder
  prefix = "acp_global_"
)

# Afficher les graphiques dans R
res$plots$scree
res$plots$all_seasons
res$plots$circle
res$plots$by_season$Hiver
res$plots$by_season$Printemps
res$plots$by_season$`Été`
res$plots$by_season$Automne