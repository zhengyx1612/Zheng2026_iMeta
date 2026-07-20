library(dplyr)
library(linkET)
library(ggplot2)

species <- read.csv('Figure 1C data species.csv',header=T,row.names = 1)
env <- read.csv('Figure 1C data env.csv',header=T,row.names = 1)

mantel <- mantel_test(species,env,
                        spec_select = list(D=1,
                                           R=2),
                        method="spearman") %>%
  mutate(rd=cut(r,breaks = c(-Inf,-0.5,-0.25,0.25,0.5,Inf),
                labels = c(">=0.5","0.25-0.5","<0.25","0.25-0.5",">=0.5")),
         pd=cut(p,breaks = c(-Inf,0.01,0.05,Inf),
                labels = c("<0.01","0.01-0.05",">=0.05")))

p <- qcorrplot(correlate(env,
                         method="spearman"),
               type='upper',
               diag=FALSE)+
  geom_square()+
  geom_couple(mantel,aes(colour=pd,size=rd),curvature=0.1,
              node.colour=c("blue","blue"),
              node.fill=c("grey","grey"),
              node.size=c(4,2))+
  scale_fill_gradientn(colours = colorRampPalette(colors =c("#333399", "white", "#CC0033"),space="Lab")(10),
                       limits=c(-1,1),
                       breaks=seq(-1,1,0.5))+
  scale_size_manual(values = c(3,1.5,0.5))+
  scale_colour_manual(values = color_pal(3))+
  guides(size=guide_legend(title = "Manterl's r",
                           override.aes = list(colour='grey35'),
                           order = 2),
         colour=guide_legend(title = "Manterl's p",
                             override.aes = list(size=1.5),
                             order = 1),
         fill=guide_colorbar(title = "Spearman's r",order = 3))+
  geom_mark(size=4,
            only_mark = T,
            sig_level = c(0.05,0.01,0.001),
            sig_thres = 0.05,
            colour="black")

ggsave(filename = './Figure 1C.pdf',plot = p,width = 8,height = 6,device = cairo_pdf)
