# 载入相关程序包
pacman::p_load(tidyverse,sf,raster,ggspatial,geoviz,
               stars,openxlsx,purrr,cowplot,beepr)

# 设置坐标系和投影
crs_84 <- st_crs("EPSG:4326")  # WGS 84 大地坐标
crs_al <- st_crs("+proj=aea +lat_1=25 +lat_2=47 +lon_0=105") ## Albers Equal Area Conic投影

China <-
  sf::st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json") %>%
  st_transform(crs_al)
# plot(China[1])

BJ <-
  sf::st_read("mapfile/BJ_out_GS(2022)1061.json") %>%
  st_transform(crs_al)
# plot(BJ[1])

SC <-
  sf::st_read("mapfile/SC_out_GS(2022)1061.json") %>%
  st_transform(crs_al)
# plot(SC[1])

ZJ <-
  sf::st_read("mapfile/ZJ_out_GS(2022)1061.json") %>%
  st_transform(crs_al)
# plot(ZJ[1])

load('mapfile/china_dem.Rdata') # 直接加载

# 作图颜色配置
colors <- c("#40A629", "#B2DF8A", "#FDBF6F", "#999999", "#E6E6E6")


# 先画地图主体部分-----------------------------------------
# 先不要加主题这些 最后统一加

P1 <- ggplot() + 
  geom_sf(data = china_dem, aes(fill = layer,  color = layer)) + #先画高程图,layer是China_dem第一列名
  geom_sf(size = .2, fill = "transparent", color = "#999999", data = China)+#国界线
  geom_sf(size = 1, fill = "transparent", color = "#060d1b", data = BJ)+#浙江省界
  geom_sf(size = 1, fill = "transparent", color = "#060d1b", data = SC)+#浙江省界
  geom_sf(size = 1, fill = "transparent", color = "#060d1b", data = ZJ)+#浙江省界
  scale_fill_gradientn(colours = colors) + #高程栅格颜色填充
  scale_color_gradientn(colours = colors) #高程栅格边沿颜色
  
# 右下角小地图-------------
# 其实就是在上面主体地图的基础上剪切出来一块
P2 <-
  P1 +
  coord_sf(crs = crs_84) + ## 将投影坐标转换为大地坐标
  scale_x_continuous(expand = c(0, 0), limits = c(107, 122), breaks = seq(70, 140, 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(2, 24), breaks = seq(10, 60, 10)) +
  guides(fill = "none", color = "none") +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(), #前三行去掉地图外围的坐标数值
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank() #后三行去除灰色背景
  )

# 组合大地图和小地图---------------
# 把主题完善一下

P3 <- P1 + coord_sf(crs = crs_al, default_crs = crs_84) + #设置坐标系
  annotate(geom = "text", x = 80, y = 17,
           label="GS(2019)6379",family = "serif", 
           vjust = 0, hjust = 0) +  #审图号
  scale_x_continuous(expand = c(0, 0),limits=c(72,142),breaks=seq(70, 140, 10)) +
  scale_y_continuous(expand = c(0, 0),limits = c(17,55.5), breaks = seq(10, 60, 10)) +
  labs(fill = "Elevant", color = "Elevant") + #高程图例
  theme_bw() +
  theme(
    legend.position = c(0.9,0.6), #设置图例放到地图内部
    legend.key.size = unit(15,'pt'),# 修该图例的大小
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), #去除灰色背景
    
    axis.text = element_text(family ="serif",color="black",size = rel(1.4)),  # 字体改为新罗马
    axis.title = element_blank(),
    plot.title = element_text(hjust = .5))+
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical(fill = c("grey40", "white"), 
                                                      line_col = "grey20"))+#指北针
  ggspatial::annotation_scale(location = "bl") + # 设置距离刻度尺
  annotation_custom(ggplotGrob(P2),
                    xmin= 122,xmax = 138,ymin=15,ymax = 29) # 最后是下图的位置


ggsave('picture/组合图.pdf',dpi=300,width = 6,height = 6)

