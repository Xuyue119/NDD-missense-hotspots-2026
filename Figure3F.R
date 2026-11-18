library(RIdeogram)

####################Figure 3F####################
markers <- read.csv("data/Figure3F_markers.csv",stringsAsFactors=FALSE)

data(human_karyotype,package="RIdeogram")
data(gene_density,package="RIdeogram")

dir.create("output",showWarnings=FALSE)

ideogram(
  karyotype=human_karyotype,
  overlaid=gene_density,
  label=markers,
  label_type="marker",
  width=170,
  Lx=220,
  Ly=35,
  output="output/Figure3F.svg"
)

p <- svg2pdf("output/Figure3F.svg",file="output/Figure3F")
grid::grid.newpage()
grid::grid.draw(p)
