library(ggplot2) 
WORD_SIZE=15
data <- read.table('Tenthredo_pseudoberyllica.txt',sep = '\t',header = T)
data2 <- data[data['Group']=='Rel.cont.L'|
                data['Group']=='Region.P.Rel.cont'|
                data['Group']=='Region.S.Rel.cont',]
data2$Group <- as.factor(data2$Group)
data2$OTU_ID <- as.factor(data2$OTU_ID)
data2$Rel.cont <- data2$Rel.cont*100
levels(data2$OTU_ID) 

p_Tenthredo_pseudoberyllica <- ggplot(data2,aes(x=factor(OTU_ID,levels = c('OTU_4',
                                                 'OTU_27',
                                                 'OTU_100',
                                                 'OTU_130',
                                                 'OTU_156')),
                      y=Rel.cont,
                      color=OTU_ID,
                      shape=factor(Group,levels = c('Rel.cont.L',
                                                    'Region.P.Rel.cont',
                                                    'Region.S.Rel.cont'))))+
  # geom_boxplot(aes(color=OTU_ID),stat="boxplot",position="dodge2")+
  geom_jitter(aes(colour=OTU_ID),position = position_jitterdodge(0),alpha=0.7,size=5)+
  scale_color_manual(values = c('#CC3333',
                                '#FFFF66',
                                '#CCCC00',
                                '#99CC99',
                                '#FFCC99'))+
  labs(x="OTU", 
       y='Relative Content of OTU(%)', 
       fill='',colour="OTU",
       shape='Sample Type')+   # x，y和图例标签
  theme_bw()+
  theme(axis.text.x = element_text(size=WORD_SIZE,angle = 0, hjust = 0.5),# 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=WORD_SIZE), 
        axis.title.x = element_text(size=WORD_SIZE+5), 
        axis.title.y = element_text(size=WORD_SIZE+5),
        legend.title = element_text(size=WORD_SIZE),
        legend.text = element_text(size=WORD_SIZE))+
  coord_cartesian(ylim = c(0,100))
p_Tenthredo_pseudoberyllica

ggsave(filename = './p_Tenthredo_pseudoberyllica.pdf',plot = p_Tenthredo_pseudoberyllica,width = 9,height = 6,device = cairo_pdf)

