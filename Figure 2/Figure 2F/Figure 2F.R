library(ggplot2)
WORD_SIZE=15

data <- read.csv('R group data.csv',header = T)
p <- ggplot(data,aes(GeneRatio2,
                     factor(Description,levels = c('Amino sugar and nucleotide sugar metabolism',
                                                   'Steroid biosynthesis',
                                                   'Glycosaminoglycan degradation',
                                                   'Lysosome',
                                                   'Glycosaminoglycan biosynthesis - heparan sulfate / heparin',
                                                   'Hormone signaling',
                                                   'Biosynthesis of nucleotide sugars',
                                                   'Sphingolipid metabolism',
                                                   'Taurine and hypotaurine metabolism',
                                                   'Histidine metabolism',
                                                   'Carbon metabolism',
                                                   'Biosynthesis of amino acids',
                                                   'Oxidative phosphorylation',
                                                   'Pyruvate metabolism',
                                                   'Citrate cycle (TCA cycle)',
                                                   'Glycine, serine and threonine metabolism',
                                                   'Fatty acid metabolism',
                                                   'Arginine biosynthesis',
                                                   'Alanine, aspartate and glutamate metabolism',
                                                   'Glyoxylate and dicarboxylate metabolism'))))+
  geom_point(aes(size=Count,color=X..Log10.Pvalue.),shape=17)+
  scale_colour_gradient2(low="#003399",mid="#FFCC33",high="#FF0033",midpoint = 3.5)+
  scale_size(range = c(2, 6))+
  theme_bw()+
  labs(x="GeneRatio", y='', size="Count",color='-log10(Padj)', title='R的KEGG') +  # x，y和图例标签
  theme(axis.text.x = element_text(size=WORD_SIZE,angle = 0, hjust = 0.5),# 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=WORD_SIZE-5,angle = 30, hjust = 1), 
        axis.title.x = element_text(size=WORD_SIZE+5), 
        axis.title.y = element_text(size=WORD_SIZE+5),
        legend.title = element_text(size=WORD_SIZE),
        legend.text = element_text(size=WORD_SIZE))
p
ggsave(filename = './R.pdf',plot = p,width = 8,height = 7,device = cairo_pdf)

data2 <- read.csv('D group data.csv',header = T)
p2 <- ggplot(data2,aes(GeneRatio2,
                     factor(Description,levels = c('Oxidative phosphorylation',
                                                   'Ribosome',
                                                   'Spliceosome',
                                                   'Ribosome biogenesis in eukaryotes',
                                                   'Nucleotide excision repair',
                                                   'Base excision repair',
                                                   'Proteasome',
                                                   'Protein export',
                                                   'DNA replication',
                                                   'RNA polymerase',
                                                   'Biosynthesis of amino acids',
                                                   'ECM-receptor interaction',
                                                   'One carbon pool by folate',
                                                   'Glycine, serine and threonine metabolism',
                                                   'Alanine, aspartate and glutamate metabolism',
                                                   'Carbon metabolism',
                                                   'Arginine biosynthesis',
                                                   'Glyoxylate and dicarboxylate metabolism',
                                                   'Motor proteins',
                                                   'Folate transport and metabolism'))))+
  geom_point(aes(size=Count,color=X..Log10.Pvalue.),shape=17)+
  scale_colour_gradient2(low="#003399",mid="#FFCC33",high="#FF0033",midpoint = 9)+
  scale_size(range = c(2, 6))+
  theme_bw()+
  labs(x="GeneRatio", y='', size="Count",color='-log10(Padj)', title='D的KEGG') +  # x，y和图例标签
  theme(axis.text.x = element_text(size=WORD_SIZE,angle = 0, hjust = 0.5),# 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=WORD_SIZE-5,angle = 30, hjust = 1), 
        axis.title.x = element_text(size=WORD_SIZE+5), 
        axis.title.y = element_text(size=WORD_SIZE+5),
        legend.title = element_text(size=WORD_SIZE),
        legend.text = element_text(size=WORD_SIZE))
p2
ggsave(filename = './D.pdf',plot = p2,width = 7,height = 7,device = cairo_pdf)
