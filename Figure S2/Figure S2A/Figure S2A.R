library(ggplot2)
WORD_SIZE=15
data <- read.csv('Figure S2A data.csv')

p <- ggplot(data,aes(x=factor(Insect_Species2,levels=c('E.miliaris',
                                                       'A.pullata',
                                                       'N.shinoharai',
                                                       'T.pseudoberyllica',
                                                       'A.AH03Ch',
                                                       'L.muscosalis',
                                                       'H.armigera',
                                                       'A.latifasciata',
                                                       'X.c-nigrum',
                                                       'A.protosema',
                                                       'L.flavalis',
                                                       'P.flavescens',
                                                       'P.bicornuta',
                                                       'C.ochreimacula',
                                                       'P.sinica',
                                                       'P.consocia',
                                                       'D.contracta',
                                                       'A.strigosa',
                                                       'K.yamadai',
                                                       'S.scintillans',
                                                       'L.nobilis',
                                                       'N.flavidorsalis')),
                     y=LarvaeLength_mm))+
  labs(x="Insect Species", y='Larval Length(mm)', fill='',colour="Host Plant Species")+   # x，y和图例标签
  ylim(0,100)+
  geom_point(aes(colour = Plant_Species2),size=5,alpha=0.7)+
  scale_color_manual(values = c('#FF0033','#CC3333','#996699','#CC99CC','#FFFF99',
                                '#009966','#FFCC33','#FFFF66','#CCCCFF','#CCCC00',
                                '#FF6666','#333333','#663300','#669933','#CC9999',
                                '#0099CC','#99CC99','#336666','#6666CC','#CCFFFF',
                                '#99CCCC','#CCCC99','#FFCC99','#FF9966','#996600',
                                '#FF9999','#9933FF','#996699','#CC6699','#003366',
                                '#339933','#993399','#99CC33','#CC9966','#999933',
                                '#333399','#333399','#003399','#99CCFF','#0066CC',
                                '#336633','#663366','#FFFFCC','#FFCCCC','#666666',
                                '#99CC66','#000000'))+
  theme_bw()+
  theme(axis.text.x = element_text(size=WORD_SIZE,angle = 45, hjust = 1),# 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=WORD_SIZE), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=WORD_SIZE),
        legend.text = element_text(size=WORD_SIZE)) 
p
ggsave(filename = './Figure S2A.pdf',plot = p,width = 19,height = 8,device = cairo_pdf)
