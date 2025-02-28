library(ggplot2)
library(biscale)
library(dplyr)
library(readr)
library(scales)
library(patchwork)

#setwd("/Volumes/LaCie/Multiplicibility/merged_csv")
#merged_glmnet <- read_csv("merged_glmnet.csv")
#merged_knn    <- read_csv("merged_knn.csv")
#merged_ranger <- read_csv("merged_ranger.csv")
#merged_rpart  <- read_csv("merged_rpart.csv")
#merged_svm    <- read_csv("merged_svm.csv")
#merged_xgb    <- read_csv("merged_xgb.csv")

# ranger #######################################################################
# num_trees vs. min_node_size

res_normalized <- merged_ranger |>
  group_by(num_trees, min_node_size) |>
  summarise(mean_f1          = mean(f1), 
            mean_discrepancy = mean(discrepancy, na.rm = TRUE), .groups = "drop") |>
  mutate(across(c(mean_f1, mean_discrepancy), rescale))

res_biscale <- bi_class(res_normalized, 
                        x      = mean_f1, 
                        y      = mean_discrepancy, 
                        style  = "equal",
                        dim    = 3)

res_biscale <- res_biscale |>
  mutate(num_trees     = as.factor(num_trees),
         min_node_size = as.factor(min_node_size))

heatmap_plot_ranger <- ggplot(res_biscale, aes(x = num_trees, y = min_node_size, fill = bi_class)) +
  geom_tile() +
  bi_scale_fill(pal = "DkBlue2", dim = 3) +
  #bi_theme() + 
  theme_minimal() + 
  labs(title = "Random Forests") + 
  theme(legend.position = "none",
        text = element_text(size = 15)) +
  scale_x_discrete(breaks = c(1, 500, 1000, 1500, 2000)) +
  scale_y_discrete(breaks = c(1, 5000, 10000, 15000, 20000))

legend_plot <- bi_legend(pal  = "DkBlue2", 
                         dim  = 3, 
                         xlab = "F1", 
                         ylab = "discrepancy",
                         size = 10)

heatmap_plot_ranger + plot_spacer() + legend_plot + plot_layout(ncol = 3, widths = c(5, 1, 1))

# glmnet #######################################################################
# alpha vs. lambda

res_normalized <- merged_glmnet |>
  group_by(alpha, lambda) |>
  summarise(mean_f1          = mean(f1), 
            mean_discrepancy = mean(discrepancy, na.rm = TRUE), .groups = "drop") |>
  mutate(across(c(mean_f1, mean_discrepancy), rescale))

res_biscale <- bi_class(res_normalized, 
                        x      = mean_f1, 
                        y      = mean_discrepancy, 
                        style  = "equal",
                        dim    = 3)

res_biscale <- res_biscale |>
  mutate(alpha  = as.factor(alpha),
         lambda = as.factor(lambda))

heatmap_plot_glmnet <- ggplot(res_biscale, aes(x = alpha, y = lambda, fill = bi_class)) +
  geom_tile() +
  bi_scale_fill(pal = "DkBlue2", dim = 3) +
  #bi_theme() + 
  theme_minimal() + 
  labs(title = "Elastic Net") + 
  theme(legend.position = "none",
        text = element_text(size = 15)) + 
  scale_x_discrete(breaks = c(0, 0.25, 0.50, 0.75, 1)) + 
  scale_y_discrete(breaks = c(1, 32, 1024))  

legend_plot <- bi_legend(pal  = "DkBlue2", 
                         dim  = 3, 
                         xlab = "F1", 
                         ylab = "discrepancy",
                         size = 10)

heatmap_plot_glmnet + plot_spacer() + legend_plot + plot_layout(ncol = 3, widths = c(5, 1, 1))

# rpart ########################################################################
# cp vs. max_depth

res_normalized <- merged_rpart |>
  group_by(cp, maxdepth) |>
  summarise(mean_f1          = mean(f1), 
            mean_discrepancy = mean(discrepancy, na.rm = TRUE), .groups = "drop") |>
  mutate(across(c(mean_f1, mean_discrepancy), rescale))

res_biscale <- bi_class(res_normalized, 
                        x      = mean_f1, 
                        y      = mean_discrepancy, 
                        style  = "equal",
                        dim    = 3)

res_biscale <- res_biscale |>
  mutate(cp       = as.factor(cp),
         maxdepth = as.factor(maxdepth))

heatmap_plot_rpart <- ggplot(res_biscale, aes(x = cp, y = maxdepth, fill = bi_class)) +
  geom_tile() +
  bi_scale_fill(pal = "DkBlue2", dim = 3) +
  #bi_theme() + 
  theme_minimal() + 
  labs(title = "Decision Tree") + 
  theme(legend.position = "none",
        text = element_text(size = 15)) + 
  scale_x_discrete(breaks = c(0, 0.25, 0.50, 0.75, 1)) 

legend_plot <- bi_legend(pal  = "DkBlue2", 
                         dim  = 3, 
                         xlab = "F1", 
                         ylab = "discrepancy",
                         size = 10)

heatmap_plot_rpart + plot_spacer() + legend_plot + plot_layout(ncol = 3, widths = c(5, 1, 1))

# svm ##########################################################################
# gamma vs. cost

res_normalized <- merged_svm |>
  group_by(gamma, cost) |>
  summarise(mean_f1          = mean(f1), 
            mean_discrepancy = mean(discrepancy, na.rm = TRUE), .groups = "drop") |>
  mutate(across(c(mean_f1, mean_discrepancy), rescale))

res_biscale <- bi_class(res_normalized, 
                        x      = mean_f1, 
                        y      = mean_discrepancy, 
                        style  = "equal",
                        dim    = 3)

res_biscale <- res_biscale |>
  mutate(gamma = as.factor(gamma),
         cost  = as.factor(cost))


heatmap_plot_svm <- ggplot(res_biscale, aes(x = gamma, y = cost, fill = bi_class)) + 
  geom_tile() + 
  bi_scale_fill(pal = "DkBlue2", dim = 3) + 
  theme_minimal() + 
  labs(title = "Support Vector Machines") + 
  theme(legend.position = "none", 
        text = element_text(size = 15)) + 
  scale_y_discrete(breaks = c(1, 32, 1024)) + 
  scale_x_discrete(breaks = c(1, 32, 1024))

legend_plot <- bi_legend(pal  = "DkBlue2", 
                         dim  = 3, 
                         xlab = "F1", 
                         ylab = "discrepancy",
                         size = 10)

heatmap_plot_svm + 
  plot_spacer() + legend_plot + plot_layout(ncol = 2, widths = c(5, 1, 1))

# xgb ##########################################################################
# gamma vs. cost

res_normalized <- merged_xgb |>
  group_by(alpha, colsample_bytree) |>
  summarise(mean_f1          = mean(f1), 
            mean_discrepancy = mean(discrepancy, na.rm = TRUE), .groups = "drop") |>
  mutate(across(c(mean_f1, mean_discrepancy), rescale))

res_biscale <- bi_class(res_normalized, 
                        x      = mean_f1, 
                        y      = mean_discrepancy, 
                        style  = "equal",
                        dim    = 3)

res_biscale <- res_biscale |>
  mutate(alpha            = as.factor(alpha),
         colsample_bytree = as.factor(colsample_bytree))


heatmap_plot_xgb <- ggplot(res_biscale, aes(x = alpha, y = colsample_bytree, fill = bi_class)) + 
  geom_tile() + 
  bi_scale_fill(pal = "DkBlue2", dim = 3) + 
  theme_minimal() + 
  labs(title = "Extreme Gradient Boosting") + 
  theme(legend.position = "none", 
        text = element_text(size = 15)) + 
  scale_y_discrete(breaks = c(1, 32, 1024)) + 
  scale_x_discrete(breaks = c(1, 32, 1024))

legend_plot <- bi_legend(pal  = "DkBlue2", 
                         dim  = 3, 
                         xlab = "F1", 
                         ylab = "discrepancy",
                         size = 10)

heatmap_plot_xgb + 
  plot_spacer() + legend_plot + plot_layout(ncol = 2, widths = c(5, 1, 1))

################################################################################

smaller_legend <- plot_grid(legend_plot, scale = 0.5)

ggarrange(heatmap_plot_glmnet, heatmap_plot_rpart, heatmap_plot_ranger, 
          heatmap_plot_svm, heatmap_plot_xgb, smaller_legend,
          ncol  = 2, 
          nrow  = 3,
          align = "v")


