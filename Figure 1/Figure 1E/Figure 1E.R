library(ggalign)

data <- read.csv('Figure 1E data Larva gut.csv',header=T,row.names = 1)

p <- ggheatmap(data) +
  scale_fill_viridis_c() +
  hmanno("t") 

ggsave(filename = './Fig 4 Larva gut.pdf',plot = p,width = 8,height = 6,device = cairo_pdf)