library(ggplot2)

data <- read.csv('Figure S8 data.csv')
WORD_SIZE=15

data$Source <- factor(data$Source, levels = c('Larva','Adult'))
p1 <- ggplot(data,aes(Source,BodyLength_mm))+
  geom_boxplot(aes(color=Source),size=1)+
  geom_jitter(aes(color=Source),stat = "identity",width = 0.2,alpha=0.4,size=5)+
  scale_color_manual(values = c('#0066CC','#0066CC'))+
  theme_bw()+
  labs(x="", y='Maximum body length (mm)', fill='') +  # x，y和图例标签
  coord_cartesian(ylim = c(0,40))+
  theme(axis.text.x = element_text(size=WORD_SIZE,angle = 0, hjust = 0.5),# 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=WORD_SIZE), 
        axis.title.x = element_text(size=WORD_SIZE+5), 
        axis.title.y = element_text(size=WORD_SIZE+5),
        legend.title = element_text(size=WORD_SIZE),
        legend.text = element_text(size=WORD_SIZE),
        legend.position = 'none')
p1
ggsave(filename = './p1.pdf',plot = p1,width = 8,height = 6,device = cairo_pdf)
