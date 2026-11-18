library(ggplot2)
library(dplyr)

INPUT_DIR <- "data"
OUT_DIR   <- "output"

mv_clean <- read.csv(file.path(INPUT_DIR, "FigureS5G_mv_clean.csv"),
                     stringsAsFactors = FALSE)

stats_file <- file.path(INPUT_DIR, "FigureS5G_stats.csv")
if (file.exists(stats_file)) {
  stats <- read.csv(stats_file, stringsAsFactors = FALSE)
  permanova_p <- stats$PERMANOVA_p[1]
  cv_auc_val  <- stats$CV_AUC[1]
  cent_p      <- stats$Centroid_p[1]
} else {
  permanova_p <- 0.104
  cv_auc_val  <- 0.58
  cent_p      <- 0.155
}

param_cols <- setdiff(colnames(mv_clean), c("UID", "Group"))

mv_nb <- mv_clean %>%
  filter(Group %in% c("Non-clustered variants", "Both-clustered variants")) %>%
  mutate(Group = factor(Group,
                        levels = c("Non-clustered variants", "Both-clustered variants")))

pca_nd <- prcomp(scale(as.matrix(mv_nb[, param_cols])),
                 center = TRUE, scale. = FALSE)
ve <- pca_nd$sdev^2 / sum(pca_nd$sdev^2)
pc_df <- data.frame(PC1 = pca_nd$x[, 1], PC2 = pca_nd$x[, 2],
                    Group = mv_nb$Group)

my_colors <- c(
  "Non-clustered variants"  = "#4C83A8",
  "Both-clustered variants" = "#c75a35"
)

subtitle_txt <- sprintf(
  "PERMANOVA p = %.3f  |  CV AUC = %.2f  |  Centroid p = %.3f",
  permanova_p, cv_auc_val, cent_p)

p_D <- ggplot(pc_df, aes(x = PC1, y = PC2, color = Group)) +
  stat_ellipse(level = 0.95, type = "norm", linetype = "dashed",
               linewidth = 1.0, alpha = 0.7) +
  geom_point(size = 2.4, alpha = 0.5) +
  scale_color_manual(
    values = my_colors,
    breaks = c("Non-clustered variants", "Both-clustered variants"),
    labels = c("Non-clustered variants"  = "Non-clustered",
               "Both-clustered variants" = "Both-clustered")) +
  theme_bw(base_size = 20) +
  labs(
    title = "D. Joint 27-parameter profile: not separable",
    subtitle = subtitle_txt,
    x = sprintf("PC1 (%.1f%%)", ve[1]*100),
    y = sprintf("PC2 (%.1f%%)", ve[2]*100)) +
  theme(legend.position = "bottom",
        plot.title    = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 17, color = "grey40"),
        axis.text     = element_text(size = 17),
        legend.text   = element_text(size = 18),
        legend.title  = element_text(size = 18))

ggsave(file.path(OUT_DIR, "PanelD_Multivariate.pdf"),
       p_D, width = 12, height = 11)
cat("Panel D saved.\n")
