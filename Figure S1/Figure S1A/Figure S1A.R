# GLMM分析：幼虫体长与肠道微生物特征的关系
# 使用合适的分布族和随机效应

rm(list = ls());gc() # 清除环境变量

# 加载必要的包
library(openxlsx)   # 读取Excel数据
library(ggplot2)    # 可视化
library(car)        # 方差膨胀因子（VIF）
library(performance)# 模型诊断

# 读取绘图的结果数据
data <- read.csv("data_for_plotting.csv")

# 创建预测数据
body_length_seq <- seq(min(data$body_length), max(data$body_length), length.out = 100)

# 选择参考水平的随机效应
ref_family <- unique(data$insect_family)[1]
ref_species <- unique(data$insect_species[data$insect_family == ref_family])[1]
ref_plant <- unique(data$plant_family)[1]

newdata_simple <- data.frame(
  body_length = body_length_seq,
  insect_family = ref_family,
  insect_species = ref_species,
  plant_family = ref_plant
)

# 函数：为负二项模型添加置信区间
add_ci_nbinom <- function(model, newdata) {
  pred <- predict(model, newdata = newdata, re.form = NA, se.fit = TRUE)
  newdata$pred <- exp(pred$fit)
  newdata$lower <- exp(pred$fit - 1.96 * pred$se.fit)
  newdata$upper <- exp(pred$fit + 1.96 * pred$se.fit)
  return(newdata)
}

# 函数：为高斯模型添加置信区间
add_ci_gaussian <- function(model, newdata) {
  pred <- predict(model, newdata = newdata, re.form = NA, se.fit = TRUE)
  newdata$pred <- pred$fit
  newdata$lower <- pred$fit - 1.96 * pred$se.fit
  newdata$upper <- pred$fit + 1.96 * pred$se.fit
  return(newdata)
}

# 函数：为Beta模型添加置信区间（在logit尺度上计算，然后转换）
add_ci_beta <- function(model, newdata) {
  pred <- predict(model, newdata = newdata, re.form = NA, se.fit = TRUE)
  # 在logit尺度上计算置信区间，然后反向转换
  logit_pred <- pred$fit
  logit_lower <- logit_pred - 1.96 * pred$se.fit
  logit_upper <- logit_pred + 1.96 * pred$se.fit

  # 使用plogis反向转换（logit的逆函数）
  newdata$pred <- plogis(logit_pred)
  newdata$lower <- plogis(logit_lower)
  newdata$upper <- plogis(logit_upper)
  return(newdata)
}

# 函数：计算伪R方（用于GLMM）
calculate_pseudo_r2 <- function(model) {
  # 使用performance包计算条件R方和边际R方
  r2 <- performance::r2_nakagawa(model)
  return(list(marginal = r2$R2_marginal, conditional = r2$R2_conditional))
}

load("simplified_models.RData") # 加载之前保存的模型对象

# 为每个模型添加预测值和置信区间
newdata_richness <- add_ci_nbinom(model_richness_simple, newdata_simple)
colnames(newdata_richness)[colnames(newdata_richness) == "pred"] <- "pred_richness"
colnames(newdata_richness)[colnames(newdata_richness) == "lower"] <- "lower_richness"
colnames(newdata_richness)[colnames(newdata_richness) == "upper"] <- "upper_richness"

newdata_shannon <- add_ci_gaussian(model_shannon_simple, newdata_simple)
colnames(newdata_shannon)[colnames(newdata_shannon) == "pred"] <- "pred_shannon"
colnames(newdata_shannon)[colnames(newdata_shannon) == "lower"] <- "lower_shannon"
colnames(newdata_shannon)[colnames(newdata_shannon) == "upper"] <- "upper_shannon"

newdata_simpson <- add_ci_beta(model_simpson_simple, newdata_simple)
colnames(newdata_simpson)[colnames(newdata_simpson) == "pred"] <- "pred_simpson"
colnames(newdata_simpson)[colnames(newdata_simpson) == "lower"] <- "lower_simpson"
colnames(newdata_simpson)[colnames(newdata_simpson) == "upper"] <- "upper_simpson"

newdata_soil <- add_ci_beta(model_soil_simple, newdata_simple)
colnames(newdata_soil)[colnames(newdata_soil) == "pred"] <- "pred_soil"
colnames(newdata_soil)[colnames(newdata_soil) == "lower"] <- "lower_soil"
colnames(newdata_soil)[colnames(newdata_soil) == "upper"] <- "upper_soil"

newdata_phyllosphere <- add_ci_beta(model_phyllosphere_simple, newdata_simple)
colnames(newdata_phyllosphere)[colnames(newdata_phyllosphere) == "pred"] <- "pred_phyllosphere"
colnames(newdata_phyllosphere)[colnames(newdata_phyllosphere) == "lower"] <- "lower_phyllosphere"
colnames(newdata_phyllosphere)[colnames(newdata_phyllosphere) == "upper"] <- "upper_phyllosphere"

newdata_unknown <- add_ci_beta(model_unknown_simple, newdata_simple)
colnames(newdata_unknown)[colnames(newdata_unknown) == "pred"] <- "pred_unknown"
colnames(newdata_unknown)[colnames(newdata_unknown) == "lower"] <- "lower_unknown"
colnames(newdata_unknown)[colnames(newdata_unknown) == "upper"] <- "upper_unknown"

# 获取体长效应的显著性（从之前的结果中提取）
# 从简化模型摘要中提取p值
richness_p <- summary(model_richness_simple)$coefficients$cond[2, 4]
shannon_p <- summary(model_shannon_simple)$coefficients$cond[2, 4]
simpson_p <- summary(model_simpson_simple)$coefficients$cond[2, 4]
soil_p <- summary(model_soil_simple)$coefficients$cond[2, 4]
phyllosphere_p <- summary(model_phyllosphere_simple)$coefficients$cond[2, 4]
unknown_p <- summary(model_unknown_simple)$coefficients$cond[2, 4]

# 计算伪R方（边际R方，即固定效应的R方）
r2_richness <- calculate_pseudo_r2(model_richness_simple)$marginal
r2_shannon <- calculate_pseudo_r2(model_shannon_simple)$marginal
r2_simpson <- calculate_pseudo_r2(model_simpson_simple)$marginal
r2_soil <- calculate_pseudo_r2(model_soil_simple)$marginal
r2_phyllosphere <- calculate_pseudo_r2(model_phyllosphere_simple)$marginal
r2_unknown <- calculate_pseudo_r2(model_unknown_simple)$marginal

# 根据显著性选择线条类型（实线为显著，虚线为不显著）
richness_line <- ifelse(richness_p < 0.05, "solid", "dashed")
shannon_line <- ifelse(shannon_p < 0.05, "solid", "dashed")
simpson_line <- ifelse(simpson_p < 0.05, "solid", "dashed")
soil_line <- ifelse(soil_p < 0.05, "solid", "dashed")
phyllosphere_line <- ifelse(phyllosphere_p < 0.05, "solid", "dashed")
unknown_line <- ifelse(unknown_p < 0.05, "solid", "dashed")

# 定义昆虫科的颜色
insect_colors <- c('#CC3333','#996699','#CC99CC','#FFFF99','#009966',
                   '#FFCC33','#FFFF66','#CCCCFF','#CCCC00','#FF6666',
                   '#333333','#663300','#669933','#CC9999','#0099CC',
                   '#99CC99','#6666CC','#99CCCC','#CCCC99')

# 获取数据中实际存在的昆虫科
actual_families <- unique(data$insect_family)
# 为实际存在的科分配颜色
names(insect_colors) <- actual_families[1:length(actual_families)]

# 创建标注数据框（用于在左上角添加R方和p值）
annotation_richness <- data.frame(
  x = min(data$body_length),
  y = max(data$richness),
  label = paste0("R² = ", round(r2_richness, 3), "\np = ", format(richness_p, scientific = TRUE, digits = 3))
)

annotation_shannon <- data.frame(
  x = min(data$body_length),
  y = max(data$shannon),
  label = paste0("R² = ", round(r2_shannon, 3), "\np = ", format(shannon_p, scientific = TRUE, digits = 3))
)

annotation_simpson <- data.frame(
  x = min(data$body_length),
  y = max(data$simpson),
  label = paste0("R² = ", round(r2_simpson, 3), "\np = ", format(simpson_p, scientific = TRUE, digits = 3))
)

annotation_soil <- data.frame(
  x = min(data$body_length),
  y = max(data$soil_prop),
  label = paste0("R² = ", round(r2_soil, 3), "\np = ", format(soil_p, scientific = TRUE, digits = 3))
)

annotation_phyllosphere <- data.frame(
  x = min(data$body_length),
  y = max(data$phyllosphere_prop),
  label = paste0("R² = ", round(r2_phyllosphere, 3), "\np = ", format(phyllosphere_p, scientific = TRUE, digits = 3))
)

annotation_unknown <- data.frame(
  x = min(data$body_length),
  y = max(data$unknown_prop),
  label = paste0("R² = ", round(r2_unknown, 3), "\np = ", format(unknown_p, scientific = TRUE, digits = 3))
)

# 图1：OTU richness
p1 <- ggplot() +
  geom_point(data = data, aes(x = body_length, y = richness, color = insect_family),
             alpha = 0.5, size = 2) +
  geom_ribbon(data = newdata_richness,
              aes(x = body_length, ymin = lower_richness, ymax = upper_richness),
              alpha = 0.2, fill = "gray50") +
  geom_line(data = newdata_richness,
            aes(x = body_length, y = pred_richness),
            color = "red", size = 1.2, linetype = richness_line) +
  geom_text(data = annotation_richness,
            aes(x = x, y = y, label = label),
            hjust = 0, vjust = 1, size = 3, family = "serif") +
  labs(x = "Relative body length (%)", y = "OTU richness") +
  scale_color_manual(values = insect_colors, name = "Insect family") +
  theme_bw() +
  theme(plot.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", size = 0.5),
        axis.title = element_text(size = 10, family = "serif"),
        axis.text = element_text(size = 8, family = "serif"),
        legend.position = "none")

# 图2：OTU simpson index
p2 <- ggplot() +
  geom_point(data = data, aes(x = body_length, y = simpson, color = insect_family),
             alpha = 0.5, size = 2) +
  geom_ribbon(data = newdata_simpson,
              aes(x = body_length, ymin = lower_simpson, ymax = upper_simpson),
              alpha = 0.2, fill = "gray50") +
  geom_line(data = newdata_simpson,
            aes(x = body_length, y = pred_simpson),
            color = "red", size = 1.2, linetype = simpson_line) +
  geom_text(data = annotation_simpson,
            aes(x = x, y = y, label = label),
            hjust = 0, vjust = 1, size = 3, family = "serif") +
  labs(x = "Relative body length (%)", y = "OTU simpson index") +
  scale_color_manual(values = insect_colors, name = "Insect family") +
  theme_bw() +
  theme(plot.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", size = 0.5),
        axis.title = element_text(size = 10, family = "serif"),
        axis.text = element_text(size = 8, family = "serif"),
        legend.position = "none")

# 图3：OTU shannon index
p3 <- ggplot() +
  geom_point(data = data, aes(x = body_length, y = shannon, color = insect_family),
             alpha = 0.5, size = 2) +
  geom_ribbon(data = newdata_shannon,
              aes(x = body_length, ymin = lower_shannon, ymax = upper_shannon),
              alpha = 0.2, fill = "gray50") +
  geom_line(data = newdata_shannon,
            aes(x = body_length, y = pred_shannon),
            color = "red", size = 1.2, linetype = shannon_line) +
  geom_text(data = annotation_shannon,
            aes(x = x, y = y, label = label),
            hjust = 0, vjust = 1, size = 3, family = "serif") +
  labs(x = "Relative body length (%)", y = "OTU shannon index") +
  scale_color_manual(values = insect_colors, name = "Insect family") +
  theme_bw() +
  theme(plot.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", size = 0.5),
        axis.title = element_text(size = 10, family = "serif"),
        axis.text = element_text(size = 8, family = "serif"),
        legend.position = "none")

# 图4：Plant source score (叶际来源)
p4 <- ggplot() +
  geom_point(data = data, aes(x = body_length, y = phyllosphere_prop, color = insect_family),
             alpha = 0.5, size = 2) +
  geom_ribbon(data = newdata_phyllosphere,
              aes(x = body_length, ymin = lower_phyllosphere, ymax = upper_phyllosphere),
              alpha = 0.2, fill = "gray50") +
  geom_line(data = newdata_phyllosphere,
            aes(x = body_length, y = pred_phyllosphere),
            color = "red", size = 1.2, linetype = phyllosphere_line) +
  geom_text(data = annotation_phyllosphere,
            aes(x = x, y = y, label = label),
            hjust = 0, vjust = 1, size = 3, family = "serif") +
  labs(x = "Relative body length (%)", y = "Plant source score") +
  scale_color_manual(values = insect_colors, name = "Insect family") +
  theme_bw() +
  theme(plot.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", size = 0.5),
        axis.title = element_text(size = 10, family = "serif"),
        axis.text = element_text(size = 8, family = "serif"),
        legend.position = "none")

# 图5：Soil source score
p5 <- ggplot() +
  geom_point(data = data, aes(x = body_length, y = soil_prop, color = insect_family),
             alpha = 0.5, size = 2) +
  geom_ribbon(data = newdata_soil,
              aes(x = body_length, ymin = lower_soil, ymax = upper_soil),
              alpha = 0.2, fill = "gray50") +
  geom_line(data = newdata_soil,
            aes(x = body_length, y = pred_soil),
            color = "red", size = 1.2, linetype = soil_line) +
  geom_text(data = annotation_soil,
            aes(x = x, y = y, label = label),
            hjust = 0, vjust = 1, size = 3, family = "serif") +
  labs(x = "Relative body length (%)", y = "Soil source score") +
  scale_color_manual(values = insect_colors, name = "Insect family") +
  theme_bw() +
  theme(plot.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", size = 0.5),
        axis.title = element_text(size = 10, family = "serif"),
        axis.text = element_text(size = 8, family = "serif"),
        legend.position = "none")

# 图6：Unknown source score
p6 <- ggplot() +
  geom_point(data = data, aes(x = body_length, y = unknown_prop, color = insect_family),
             alpha = 0.5, size = 2) +
  geom_ribbon(data = newdata_unknown,
              aes(x = body_length, ymin = lower_unknown, ymax = upper_unknown),
              alpha = 0.2, fill = "gray50") +
  geom_line(data = newdata_unknown,
            aes(x = body_length, y = pred_unknown),
            color = "red", size = 1.2, linetype = unknown_line) +
  geom_text(data = annotation_unknown,
            aes(x = x, y = y, label = label),
            hjust = 0, vjust = 1, size = 3, family = "serif") +
  labs(x = "Relative body length (%)", y = "Unknown source score") +
  scale_color_manual(values = insect_colors, name = "Insect family") +
  theme_bw() +
  theme(plot.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", size = 0.5),
        axis.title = element_text(size = 10, family = "serif"),
        axis.text = element_text(size = 8, family = "serif"),
        legend.position = "none")

# 提取图例（单独创建一个带图例的图）
legend_plot <- ggplot() +
  geom_point(data = data, aes(x = body_length, y = richness, color = insect_family),
             alpha = 0.5, size = 2) +
  scale_color_manual(values = insect_colors, name = "Insect family") +
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_text(size = 10, family = "serif", face = "bold"),
        legend.text = element_text(size = 8, family = "serif"))

# 提取图例
library(patchwork)
library(cowplot)
legend <- get_legend(legend_plot)

# 合并图形（不带图例），使用水平布局，左边是图形，右边是图例
combined_plot <- (p1 + p2 + p3) / (p4 + p5 + p6)

# 将图形和图例并排放置（图形占4/5，图例占1/5）
combined_plot_with_legend <- plot_grid(combined_plot, legend,
                                       ncol = 2,
                                       rel_widths = c(4, 1))

# 显示图形
print(combined_plot_with_legend)

# 保存图形
ggsave("all_relationships_linear_combined.pdf", combined_plot_with_legend,
       width = 10, height = 5)
