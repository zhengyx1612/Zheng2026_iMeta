library(ggplot2)
library(scales)
library(ggsignif)

data <- read.csv('Figure S1B data.csv')
WORD_SIZE=15

x <- data[,'reads_ChloroplastExcluded']
y <- as.factor(data[,'Source'])
pairwise.wilcox.test(x, y, p.adjust.method="BH")
p1 <- ggplot(data,aes(Source,reads_ChloroplastExcluded))+
  geom_boxplot(aes(color=Source),size=1)+
  geom_jitter(aes(color=Source),stat = "identity",width = 0.2,alpha=0.4,size=5)+
  scale_color_manual(values = c('#996600','#00CC00','#999999'))+
  theme_bw()+
  labs(x="Source", y='Sample reads after chloroplast removal', fill='') +  # x，y和图例标签
  coord_cartesian(ylim = c(0,125000))+
  theme(axis.text.x = element_text(size=WORD_SIZE,angle = 0, hjust = 0.5),# 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=WORD_SIZE), 
        axis.title.x = element_text(size=WORD_SIZE+5), 
        axis.title.y = element_text(size=WORD_SIZE+5),
        legend.title = element_text(size=WORD_SIZE),
        legend.text = element_text(size=WORD_SIZE),
        legend.position = 'none')+
  geom_signif(y_position = c(120000,110000,105000),
              xmin = c(1,1,2),
              xmax = c(3,2,3),
              annotations = c('***','*','***'),tip_length = 0.01,size = 0.8,textsize = 7,
              vjust = 0.3)
ggsave(filename = './p1.pdf',plot = p1,width = 8,height = 6,device = cairo_pdf)

x <- data[,'richness']
y <- as.factor(data[,'Source'])
pairwise.wilcox.test(x, y, p.adjust.method="BH")
p2 <- ggplot(data,aes(Source,richness))+
  geom_boxplot(aes(color=Source),size=1)+
  geom_jitter(aes(color=Source),stat = "identity",width = 0.2,alpha=0.4,size=5)+
  scale_color_manual(values = c('#996600','#00CC00','#999999'))+
  theme_bw()+
  labs(x="Source", y='OTU richness after chloroplast removal', fill='') +  # x，y和图例标签
  coord_cartesian(ylim = c(0,4600))+
  theme(axis.text.x = element_text(size=WORD_SIZE,angle = 0, hjust = 0.5),# 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=WORD_SIZE), 
        axis.title.x = element_text(size=WORD_SIZE+5), 
        axis.title.y = element_text(size=WORD_SIZE+5),
        legend.title = element_text(size=WORD_SIZE),
        legend.text = element_text(size=WORD_SIZE),
        legend.position = 'none')+
  geom_signif(y_position = c(4400,4000,3850),
              xmin = c(1,1,2),
              xmax = c(3,2,3),
              annotations = c('***','***','***'),tip_length = 0.01,size = 0.8,textsize = 7,
              vjust = 0.3)
ggsave(filename = './p2.pdf',plot = p2,width = 8,height = 6,device = cairo_pdf)

x <- data[,'shannon_e']
y <- as.factor(data[,'Source'])
pairwise.wilcox.test(x, y, p.adjust.method="BH")
p3 <- ggplot(data,aes(Source,shannon_e))+
  geom_boxplot(aes(color=Source),size=1)+
  geom_jitter(aes(color=Source),stat = "identity",width = 0.2,alpha=0.4,size=5)+
  scale_color_manual(values = c('#996600','#00CC00','#999999'))+
  theme_bw()+
  labs(x="Source", y='OTU shannon_e indice after chloroplast removal', fill='') +  # x，y和图例标签
  coord_cartesian(ylim = c(0,10))+
  theme(axis.text.x = element_text(size=WORD_SIZE,angle = 0, hjust = 0.5),# 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=WORD_SIZE), 
        axis.title.x = element_text(size=WORD_SIZE+5), 
        axis.title.y = element_text(size=WORD_SIZE+5),
        legend.title = element_text(size=WORD_SIZE),
        legend.text = element_text(size=WORD_SIZE),
        legend.position = 'none')+
  geom_signif(y_position = c(9.5,8.5,8),
              xmin = c(1,1,2),
              xmax = c(3,2,3),
              annotations = c('***','***','***'),tip_length = 0.01,size = 0.8,textsize = 7,
              vjust = 0.3)
ggsave(filename = './p3.pdf',plot = p3,width = 8,height = 6,device = cairo_pdf)