library(dplyr)
library(ggplot2)

analyze_correlations <- function(file_path, heatmap_path = NULL) {
  data <- read.csv(
    file_path,
    sep = ",", header = TRUE,
    na.strings = c("NA","NaN",""," "),
    strip.white = TRUE
  )
  data <- data %>%
    filter(nchar(as.character(ID.OMM.station)) == 4 & grepl("^[0-9]+$", ID.OMM.station))

  cols_to_remove <- c("Date","ID.OMM.station","mois_de_l_annee",
                      "Temps.présent","Type.de.tendance.barométrique",
                      "Periode.de.mesure.de.la.rafale")
  
  obs_num <- data %>%
    select(-any_of(cols_to_remove)) %>%
    mutate(across(where(is.numeric),
                  ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))) %>%
    select(where(~ var(.x, na.rm = TRUE) > 0))
  
  cor_mat <- cor(obs_num, use = "pairwise.complete.obs")
  
  cor_long <- as.data.frame(as.table(cor_mat)) %>%
    rename(Var1 = Var1, Var2 = Var2, Corr = Freq) %>%
    filter(Var1 != Var2) %>%
    mutate(
      Var1 = factor(Var1, levels = colnames(cor_mat)),
      Var2 = factor(Var2, levels = colnames(cor_mat))
    ) %>%
    filter(as.numeric(Var1) > as.numeric(Var2))
  
  p <- ggplot(cor_long, aes(Var1, Var2, fill = Corr)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limits = c(-1, 1), name = "Corrélation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    coord_fixed() +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE)
  
  if (!is.null(heatmap_path)) {
    ggsave(heatmap_path, plot = p, width = 8, height = 6, dpi = 300)
  }
  
  pairs_corr <- cor_long %>% arrange(desc(Corr))
  return(list(plot = p, correlations = pairs_corr))
}



args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Usage: Rscript analyze_corr.R <input_csv> <output_rds> [heatmap.png]")
}

file_path <- args[1]
heatmap_path <- ifelse(length(args) >= 3, args[3], "heatmap.png")

analyze_correlations(file_path, heatmap_path)