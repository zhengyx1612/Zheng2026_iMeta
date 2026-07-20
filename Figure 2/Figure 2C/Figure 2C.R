library(ggplot2) 
data <- read.csv('Figure 2C data.csv',header = T,fileEncoding ='GBK')
data$Group <- as.factor(data$Group)
WORD_SIZE=15
p <- ggplot(data, aes(x=Age, y=Weight,color=Group)) +
  geom_boxplot(position=position_dodge(width=0.9),)+
  geom_point(aes(color=Group),
             alpha=0.5,size=3,
             position=position_jitterdodge(jitter.width = 0.4,
                                           dodge.width = 0.9))+
  theme_bw()+
  labs(x="Larval development stage", y='Insect weight(g)', fill='') +  # x，y和图例标签
  theme(axis.text.x = element_text(size=WORD_SIZE,angle = 0, hjust = 0.5),# 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=WORD_SIZE), 
        axis.title.x = element_text(size=WORD_SIZE+5), 
        axis.title.y = element_text(size=WORD_SIZE+5),
        legend.title = element_text(size=WORD_SIZE),
        legend.text = element_text(size=WORD_SIZE))+
  ylim(c(0,0.6))
p
ggsave(filename = './Figure 2C.pdf',plot = p,width = 7,height = 6,device = cairo_pdf)
