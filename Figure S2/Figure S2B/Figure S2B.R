data <- read.csv('Figure S2B data.csv')
data$Shape <- as.factor(data$Species)

p <- ggplot(data,aes(x=factor(Species,levels=c('E.miliaris',
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
                     y=Frequency.of.occurrence.of.OTU...))+
  # geom_point(aes(shape=Shape))+
  geom_jitter(aes(color=OTU_ID,size=Content.of.OTU...,shape=Species),alpha=0.7,height = 0.01)+
  scale_color_manual(values = c('#FF0033',
                                '#CC3333',
                                '#996699',
                                '#CC99CC',
                                '#FFFF99',
                                '#009966',
                                '#FFCC33',
                                '#FFFF66',
                                '#CCCCFF',
                                '#CCCC00',
                                '#FF6666',
                                '#333333',
                                '#663300',
                                '#669933',
                                '#CC9999',
                                '#0099CC',
                                '#99CC99',
                                '#336666',
                                '#6666CC',
                                '#CCFFFF',
                                '#99CCCC',
                                '#CCCC99',
                                '#FFCC99',
                                '#FF9966',
                                '#996600',
                                '#FF9999',
                                '#9933FF',
                                '#996699',
                                '#CC6699',
                                '#003366',
                                '#339933',
                                '#993399'))+
  scale_shape_manual(values = c(1:44))+
  labs(x="Insect Species", 
       y='Frequency of Occurrence of OTU(%)', 
       fill='',colour="OTU",
       size='Relative Content of OTU(%)')+   # x，y和图例标签
  coord_cartesian(ylim = c(70,100))+
  theme_bw()+
  theme(axis.text.x = element_text(size=WORD_SIZE,angle = 45, hjust = 1),# 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=WORD_SIZE), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=WORD_SIZE),
        legend.text = element_text(size=WORD_SIZE)) 
p
ggsave(filename = './Figure S2B.pdf',plot = p,width = 14.8,height = 7,device = cairo_pdf)