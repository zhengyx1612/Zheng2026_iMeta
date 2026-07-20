library(ggsankey)
library(tidyverse)
library(ggplot2) 
mydata <- read.csv('Supplementary figure 5 data.csv',header=T)
mydata2 <- mydata[,c(7,5,4)]

mysankey_data <- mydata2 %>%
  make_long(Insect_Order,Insect_Species,R_or_D)

mysankey_data$node <- factor(mysankey_data$node,
                             levels = rev(c(names(table(mydata2$Insect_Order)[order(table(mydata2$Insect_Order),decreasing = T)]),
                                            names(table(mydata2$Insect_Species)[order(table(mydata2$Insect_Species),decreasing = T)]),
                                            names(table(mydata2$R_or_D)[order(table(mydata2$R_or_D),decreasing = T)]))))
mysankey_data$next_node <- factor(mysankey_data$next_node,
                                  levels = rev(c(names(table(mydata2$Insect_Species)[order(table(mydata2$Insect_Species),decreasing = T)]),
                                                 names(table(mydata2$R_or_D)[order(table(mydata2$R_or_D),decreasing = T)]))))
mysankey_data$x <- factor(mysankey_data$x,
                          levels = c('Insect_Order','Insect_Species','R_or_D'))

P1 <- ggplot(mysankey_data, 
              aes(x = x, 
                  next_x = next_x, 
                  node = node, 
                  next_node = next_node, 
                  fill = factor(node))) +
  geom_alluvial(width = 0.5, 
                smooth = 8,
                na.rm = TRUE,
                position = "identity",
                flow.alpha = 0.4,
                node.color = "transparent") +
  geom_alluvial_text(aes(label = node),
                     width = 0.2, 
                     position = "identity", 
                     size = 3, 
                     color = "black", 
                     hjust = 0.5) +
  theme_sankey(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.08),
        plot.margin = unit(c(0.1, 0, 0.1, 0), "cm"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(color = "black", size =12),
        legend.position = "none")
P44
ggsave(filename = './Supplementary figure 5.pdf',plot = P1,width = 12,height = 18,device = cairo_pdf)