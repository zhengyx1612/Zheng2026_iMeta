library(ggplot2)
data <- read.csv('Figure 1B Beijing data.csv')
WORD_SIZE=15

#Prevalence of OTU_4&OTU_94 in plant phyllosphere
p1 <- ggplot(data,aes(Occurrence_of_OTU_in_plant_samples...,
                            Log10_Average_relative_OTU_content_in_plant_samples))+
  theme_bw()+
  geom_point(alpha=0.3,size=3.5,color='#00CC00')+
  geom_vline(xintercept=data[data$OTU_ID=="OTU_4",2],color='#FFCC99')+
  geom_hline(yintercept=data[data$OTU_ID=="OTU_4",6],color='#FFCC99')+
  geom_vline(xintercept=data[data$OTU_ID=="OTU_94",2],color='#339933')+
  geom_hline(yintercept=data[data$OTU_ID=="OTU_94",6],color='#339933')+
  labs(title ='',x='Prevalency of microbes in plant (%)',
       y='Log10(Mean relative abundance of microbiomes in plant (%))')+
  theme(axis.text.x = element_text(size=WORD_SIZE,angle = 0, hjust = 0.5),# 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=WORD_SIZE),
        axis.title.x = element_text(size=WORD_SIZE+5),
        axis.title.y = element_text(size=WORD_SIZE+5),
        legend.title = element_text(size=WORD_SIZE),
        legend.text = element_text(size=WORD_SIZE))
p1
ggsave(filename = './p1.pdf',plot = p1,width = 8,height = 6,device = cairo_pdf)

#Prevalence of OTU_4&OTU_94 in soil
p2 <- ggplot(data,aes(Occurrence_of_OTU_in_soil_samples...,
                            Log10_Average_relative_OTU_content_in_soil_samples))+
  theme_bw()+
  geom_point(alpha=0.3,size=3.5,color='#999999')+
  geom_vline(xintercept=data[data$OTU_ID=="OTU_4",4],color='#FFCC99')+
  geom_hline(yintercept=data[data$OTU_ID=="OTU_4",7],color='#FFCC99')+
  geom_vline(xintercept=data[data$OTU_ID=="OTU_94",4],color='#339933')+
  geom_hline(yintercept=data[data$OTU_ID=="OTU_94",7],color='#339933')+
  labs(title ='',x='Prevalency of microbes in soil (%)',
       y='Log10(Mean relative abundance of microbiomes in soil (%))')+
  theme(axis.text.x = element_text(size=WORD_SIZE,angle = 0, hjust = 0.5),# 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=WORD_SIZE),
        axis.title.x = element_text(size=WORD_SIZE+5),
        axis.title.y = element_text(size=WORD_SIZE+5),
        legend.title = element_text(size=WORD_SIZE),
        legend.text = element_text(size=WORD_SIZE))
p2
ggsave(filename = './p2.pdf',plot = p2,width = 8,height = 6,device = cairo_pdf)