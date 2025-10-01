afd_with_season_plots <- function(file_path,
                                  cols_to_remove = c("Date","ID.OMM.station","mois_de_l_annee",
                                                     "Temps.présent","Type.de.tendance.barométrique",
                                                     "Periode.de.mesure.de.la.rafale","mois"),
                                  seuil_distance = 0.3,
                                  scale_factor = 3,
                                  sample_max = 5000,
                                  seed = 123,
                                  output_dir = NULL,
                                  prefix = "afd_") {
  stopifnot(requireNamespace("dplyr", quietly = TRUE),
            requireNamespace("ggplot2", quietly = TRUE),
            requireNamespace("lubridate", quietly = TRUE),
            requireNamespace("MASS", quietly = TRUE),
            requireNamespace("grid", quietly = TRUE))
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(MASS)
  library(grid) # unit()
  has_ggdendro <- requireNamespace("ggdendro", quietly = TRUE)

  data <- read.csv(
    file_path,
    sep = ",", header = TRUE,
    na.strings = c("NA","NaN",""," "),
    strip.white = TRUE
  )

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
  tmp <- data %>%
    dplyr::select(-dplyr::any_of(cols_to_remove))

  tmp[] <- lapply(tmp, function(x) {
    if (is.numeric(x)) {
      m <- mean(x, na.rm = TRUE)
      x[is.na(x)] <- m
    }
    x
  })

  keep <- sapply(tmp, function(x) {
    is.numeric(x) && is.finite(stats::var(x, na.rm = TRUE)) && stats::var(x, na.rm = TRUE) > 0
  })
  X <- tmp[, keep, drop = FALSE]

  stopifnot(ncol(X) >= 2, nrow(X) >= 3)
  y <- factor(data$saison)
  stopifnot(nlevels(y) >= 2)


  Xs <- scale(X)
  lda_fit <- lda(x = Xs, grouping = y)

  eigvals <- lda_fit$svd^2                  
  var_exp_pct <- eigvals / sum(eigvals)
  scree_df <- data.frame(
    LD = factor(paste0("LD", seq_along(eigvals)),
                levels = paste0("LD", seq_along(eigvals))),
    pct = as.numeric(var_exp_pct)
  )
  p_scree <- ggplot(scree_df, aes(x = LD, y = pct)) +
    geom_line(group = 1) +
    geom_point() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal() +
    labs(title = "Scree plot (AFD)", x = "Axes discriminants", y = "% variance discriminante")

  loadings <- as.matrix(lda_fit$scaling)
  k_axes <- min(2, ncol(loadings))
  loadings12 <- loadings[, 1:k_axes, drop = FALSE]
  colnames(loadings12) <- paste0("LD", 1:k_axes)

  loadings_df <- as.data.frame(loadings12)
  loadings_df$var <- rownames(loadings_df)

  if (k_axes < 2) {
    warning("Il n'y a qu'un seul axe discriminant (niveaux de classe - 1). Les graphiques 2D utiliseront LD1 et un axe nul.")
    loadings_df$LD2 <- 0
    loadings12 <- as.matrix(loadings_df[, c("LD1","LD2")])
  }
  dist_matrix <- dist(loadings12, method = "euclidean")
  hc <- hclust(dist_matrix, method = "complete")
  groupes <- cutree(hc, h = seuil_distance)
  loadings_df$groupe <- as.factor(groupes)

  centroides <- loadings_df %>%
    group_by(groupe) %>%
    summarise(
      LD1 = mean(LD1),
      LD2 = mean(LD2),
      vars = paste(var, collapse = ", "),
      n_vars = dplyr::n(),
      .groups = "drop"
    )

  p_dendro <- NULL
  if (has_ggdendro) {
    dend <- ggdendro::dendro_data(hc, type = "rectangle")
    p_dendro <- ggplot() +
      geom_segment(data = dend$segments,
                   aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_text(data = dend$labels,
                aes(x = x, y = y - max(dend$segments$y)*0.05, label = label),
                angle = 90, hjust = 1, vjust = 0.5, size = 3) +
      geom_hline(yintercept = seuil_distance, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Dendrogramme des variables (AFD)",
           subtitle = paste("Seuil de coupe :", seuil_distance),
           x = NULL, y = "Hauteur")
  }

  pred <- predict(lda_fit, Xs)
  scores <- as.data.frame(pred$x[, 1:k_axes, drop = FALSE])
  if (k_axes == 1) scores$LD2 <- 0
  names(scores)[1:2] <- c("LD1","LD2")
  scores$saison <- y

  set.seed(seed)
  n_echantillon <- min(sample_max, nrow(scores))
  idx <- sample(nrow(scores), n_echantillon)
  scores_sample <- scores[idx, , drop = FALSE]

  pal_saison <- c(
    "Hiver" = "#3498db",
    "Printemps" = "#2ecc71",
    "Été" = "#e74c3c",
    "Automne" = "#f39c12"
  )

  p_all <- ggplot() +
    geom_point(data = scores_sample,
               aes(x = LD1, y = LD2, color = saison),
               alpha = 0.3, size = 0.8) +
    geom_segment(data = centroides,
                 aes(x = 0, y = 0, xend = LD1 * scale_factor, yend = LD2 * scale_factor),
                 arrow = arrow(length = unit(0.3, "cm")),
                 linewidth = 1.5, color = "black") +
    geom_text(data = centroides,
              aes(x = LD1 * scale_factor * 1.1,
                  y = LD2 * scale_factor * 1.1,
                  label = paste0("G", groupe, " (n=", n_vars, ")")),
              size = 3.5, fontface = "bold", color = "black") +
    scale_color_manual(values = pal_saison) +
    theme_minimal() +
    labs(title = "Projection AFD : observations par saison et groupes de variables",
         subtitle = paste("Échantillon de", n_echantillon, "observations sur", nrow(scores)),
         x = paste0("LD1 (", round(100 * var_exp_pct[1], 1), "%)"),
         y = paste0("LD2 (", round(100 * ifelse(length(var_exp_pct) >= 2, var_exp_pct[2], 0), 1), "%)"),
         color = "Saison") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
    theme(legend.position = "right")

  saisons_ord <- c("Hiver","Printemps","Été","Automne")
  plots_saison <- lapply(saisons_ord, function(s) {
    df_s <- scores_sample %>% dplyr::filter(saison == s)
    ggplot() +
      geom_point(data = df_s,
                 aes(x = LD1, y = LD2, color = saison),
                 alpha = 0.35, size = 0.9) +
      geom_segment(data = centroides,
                   aes(x = 0, y = 0,
                       xend = LD1 * scale_factor,
                       yend = LD2 * scale_factor),
                   arrow = arrow(length = unit(0.25, "cm")),
                   linewidth = 1, color = "black") +
      geom_text(data = centroides,
                aes(x = LD1 * scale_factor * 1.08,
                    y = LD2 * scale_factor * 1.08,
                    label = paste0("G", groupe)),
                size = 3.2, fontface = "bold") +
      scale_color_manual(values = pal_saison, drop = FALSE) +
      theme_minimal() +
      labs(title = paste0("Projection AFD – ", s),
           subtitle = paste0("Échantillon de ", nrow(df_s), " / ", nrow(scores), " observations"),
           x = paste0("LD1 (", round(100 * var_exp_pct[1], 1), "%)"),
           y = paste0("LD2 (", round(100 * ifelse(length(var_exp_pct) >= 2, var_exp_pct[2], 0), 1), "%)"),
           color = "Saison") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
      theme(legend.position = "right")
  })
  names(plots_saison) <- saisons_ord

  p_coeffs <- ggplot() +
    geom_segment(data = loadings_df,
                 aes(x = 0, y = 0, xend = LD1, yend = LD2, color = groupe),
                 arrow = arrow(length = unit(0.2, "cm")),
                 alpha = 0.45, linewidth = 0.6) +
    geom_segment(data = centroides,
                 aes(x = 0, y = 0, xend = LD1, yend = LD2, color = groupe),
                 arrow = arrow(length = unit(0.4, "cm")),
                 linewidth = 2) +
    geom_text(data = centroides,
              aes(x = LD1, y = LD2, label = paste0("G", groupe, " (n=", n_vars, ")")),
              hjust = -0.2, size = 4, fontface = "bold") +
    theme_minimal() +
    labs(title = "Coefficients des variables sur (LD1, LD2) avec regroupement",
         subtitle = paste("Seuil distance :", seuil_distance),
         x = "Coefficient sur LD1", y = "Coefficient sur LD2") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
    theme(legend.position = "none")





  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    ggsave(file.path(output_dir, paste0(prefix, "scree.png")), p_scree, width = 8, height = 6, dpi = 150)
    ggsave(file.path(output_dir, paste0(prefix, "observations_saisons.png")), p_all, width = 12, height = 8, dpi = 150)
    ggsave(file.path(output_dir, paste0(prefix, "coefficients_groupes.png")), p_coeffs, width = 10, height = 8, dpi = 150)
    if (!is.null(p_dendro)) {
      ggsave(file.path(output_dir, paste0(prefix, "dendrogramme_variables.png")), p_dendro, width = 10, height = 7, dpi = 150)
    }
    for (s in names(plots_saison)) {
      ggsave(file.path(output_dir, paste0(prefix, "projection_", s, ".png")),
             plots_saison[[s]], width = 10, height = 7, dpi = 150)
    }
    if (requireNamespace("readr", quietly = TRUE)) {
      readr::write_csv(centroides, file.path(output_dir, paste0(prefix, "groupes_centroides.csv")))
    } else {
      write.csv(centroides, file.path(output_dir, paste0(prefix, "groupes_centroides.csv")), row.names = FALSE)
    }
  }

  list(
    lda = lda_fit,
    variance_ld1_ld2 = var_exp_pct[1:min(2, length(var_exp_pct))],
    loadings = loadings_df,
    centroides = centroides,
    groupes = split(loadings_df$var, loadings_df$groupe),
    plots = list(
      scree = p_scree,
      all_seasons = p_all,
      by_season = plots_saison,
      coefficients = p_coeffs,
      dendrogram = p_dendro
    )
  )
}

id <- 'global'

# # Construire le chemin et le préfixe dynamiquement
# file_path   <- paste0("csv/observations_cleaned.csv")
# output_dir  <- paste0("fig/", id, "/")
# prefix      <- paste0("afd_", id, "_")

# # Appel de la fonction
# res_afd <- afd_with_season_plots(
#   file_path      = file_path,
#   seuil_distance = 0.2,
#   output_dir     = output_dir,      
#   prefix         = prefix
# )

# # Afficher les graphiques
# res_afd$plots$scree
# res_afd$plots$all_seasons
# res_afd$plots$coefficients
# res_afd$plots$dendrogram  # peut être NULL si ggdendro non installé
# res_afd$plots$by_season$Hiver
# res_afd$plots$by_season$Printemps
# res_afd$plots$by_season$`Été`
# res_afd$plots$by_season$Automne

# # Groupes de variables
# res_afd$groupes