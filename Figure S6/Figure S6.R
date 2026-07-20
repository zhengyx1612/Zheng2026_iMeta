library(ggplot2)
WORD_SIZE=15

data <- read.csv('Figure S6 data.csv',header = T)
p <- ggplot(data,aes(x=factor(Group,levels = c('OTU Level',
                                                'Sample Level')),
                      y=Percentage))+
  geom_bar(stat = 'identity',aes(fill=factor(Levels,levels = c('NSTI>2',
                                                               'NSTI<=2',
                                                               'weighted_NSTI>0.15',
                                                               'weighted_NSTI>=0.06 & <=0.15',
                                                               'weighted_NSTI<0.06')))) +
  scale_fill_manual(values = c('#FF0033','#00CC00','#FF0033','#FF9900','#00CC00'))+ #'#999999'灰色,'#00CC00'绿色,'#99CCFF'浅蓝,'#0099CC'深蓝
  guides(fill=guide_legend(title = ""))+
  geom_text(aes(label=round(Amount, digits=2)), color="white", size = 6,hjust='center')+
  theme_classic()+
  labs(title ='',x='',y='Percentage (%)')+
  theme(axis.text.x = element_text(size=WORD_SIZE,angle = 0, hjust = 0.5),# 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=WORD_SIZE),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=WORD_SIZE),
        legend.text = element_text(size=WORD_SIZE))
p
ggsave(filename = './Figure S6.pdf',plot = p,width = 8,height = 6,device = cairo_pdf)