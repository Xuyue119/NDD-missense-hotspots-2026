library(ggplot2)
library(dplyr)

INPUT_DIR <- "data/"
OUT_DIR   <- "output"

dat <- read.csv(file.path(INPUT_DIR, "Figure5F_dimension.csv"),stringsAsFactors = FALSE)

p <- ggplot(dat,aes(x = neg_log10_q,y = Dimension,color = Significance)) +
  geom_vline(xintercept = -log10(0.05),linetype = "dashed",linewidth = 0.7,color = "grey45") +
  geom_segment(
    aes(x = 0,xend = neg_log10_q,yend = Dimension),
    linewidth = 1.0,alpha = 0.75) +
  geom_point(size = 3.8) +
  geom_text(aes(label = P_label),hjust = -0.08,size = 3.2,show.legend = FALSE,color = "black") +
  scale_color_manual(values = "#888888") +
  scale_x_continuous(
    name = expression(-log[10]("FDR-adjusted P")),
    expand = expansion(mult = c(0.02, 0.42))) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = 12) +
  theme(axis.title.y = element_blank(),
    axis.text.y = element_text(face = "bold",color = "black"),
    axis.text.x = element_text(color = "black"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold",size = 14),
    plot.subtitle = element_text(size = 10,color = "grey30"),
    plot.caption = element_text(size = 8,color = "grey35",hjust = 0),
    plot.margin = margin(10, 115, 10, 10))
ggsave(file.path(output_dir, "Figure_Seven_Dimension_Integrated_Evidence.pdf"),plot = p)
