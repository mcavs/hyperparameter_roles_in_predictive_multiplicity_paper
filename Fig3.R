library(ggplot2)
library(biscale)
library(dplyr)
library(readr)
library(scales)
library(patchwork)
library(cowplot)

#setwd("/cloud/project")
#merged_glmnet <- read_csv("merged_glmnet.csv")
#merged_knn    <- read_csv("merged_knn.csv")
#merged_ranger <- read_csv("merged_ranger.csv")
#merged_rpart  <- read_csv("merged_rpart.csv")
#merged_svm    <- read_csv("merged_svm.csv")
#merged_xgb    <- read_csv("merged_xgb.csv")

# ranger #######################################################################
# num_trees vs. min_node_size

default_mtry <- merged_ranger |> 
  filter(default == TRUE) |> 
  select(dataset, mtry) |> 
  distinct()

res_normalized <- merged_ranger |>
  inner_join(default_mtry, by = "dataset") |> 
  filter(mtry.x          == mtry.y,
         sample_fraction == merged_ranger$sample_fraction[which(merged_ranger$default == TRUE)[1]]) |>
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
  labs(title = "Random Forests",
       y     = "min.node.size",
       x     = "num.trees") + 
  theme(legend.position = "none", 
        text = element_text(size   = 14),
        plot.title  = element_text(family = "LM Roman 10"),
        axis.text   = element_text(family = "LM Roman 10"),
        axis.title  = element_text(family = "Courier")) +
  scale_x_discrete(breaks = c(1, 501, 1000, 1500, 2000)) + 
  scale_y_discrete(breaks = c(1, 527, 1224, 2785, 19020))

legend_plot <- bi_legend(pal  = "DkBlue2", 
                         dim  = 3, 
                         xlab = "F1", 
                         ylab = "discrepancy",
                         size = 10) + 
  theme(axis.title  = element_text(family = "LM Roman 10"))

heatmap_plot_ranger + plot_spacer() + legend_plot + plot_layout(ncol = 3, widths = c(5, 1, 1))

# glmnet #######################################################################
# alpha vs. lambda

res_normalized <- merged_glmnet |>
  group_by(alpha, lambda) |>
  summarise(mean_f1          = mean(f1), 
            mean_discrepancy = mean(discrepancy), .groups = "drop") |>
  mutate(across(c(mean_f1, mean_discrepancy), rescale))

res_biscale <- bi_class(res_normalized, 
                        x      = mean_f1, 
                        y      = mean_discrepancy, 
                        style  = "equal",
                        dim    = 3)

res_biscale <- res_biscale |>
  mutate(alpha  = as.factor(round(alpha, 2)),
         lambda = as.factor(round(lambda, 4)))

heatmap_plot_glmnet <- ggplot(res_biscale, aes(x = alpha, y = lambda, fill = bi_class)) +
  geom_tile() +
  bi_scale_fill(pal = "DkBlue2", dim = 3) +
  #bi_theme() + 
  theme_minimal() + 
  labs(title = "Elastic Net") + 
  theme(legend.position = "none", 
        text = element_text(size   = 14),
        plot.title  = element_text(family = "LM Roman 10"),
        axis.text   = element_text(family = "LM Roman 10"),
        axis.title  = element_text(family = "Courier")) +
  scale_x_discrete(breaks = c(0, 0.25, 0.50, 0.75, 1)) + 
  scale_y_discrete(breaks = c(0.001, 0.0292, 0.9551, 31.2739, 1024))

legend_plot <- bi_legend(pal  = "DkBlue2", 
                         dim  = 3, 
                         xlab = "F1", 
                         ylab = "discrepancy",
                         size = 10) + 
  theme(axis.title  = element_text(family = "LM Roman 10"))

heatmap_plot_glmnet + plot_spacer() + legend_plot + plot_layout(ncol = 3, widths = c(5, 1, 1))

# rpart ########################################################################
# cp vs. max_depth

res_normalized <- merged_rpart |>
  filter(minsplit  == merged_rpart$minsplit[which(merged_rpart$default  == TRUE)[1]],
         minbucket == merged_rpart$minbucket[which(merged_rpart$default == TRUE)[1]]) |>
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
        text = element_text(size   = 14),
        plot.title  = element_text(family = "LM Roman 10"),
        axis.text   = element_text(family = "LM Roman 10"),
        axis.title  = element_text(family = "Courier")) + 
  scale_x_discrete(breaks = c(0, 0.25, 0.50, 0.75, 1)) +
  scale_y_discrete(breaks = c(1, 8, 16, 23, 30))

legend_plot <- bi_legend(pal  = "DkBlue2", 
                         dim  = 3, 
                         xlab = "F1", 
                         ylab = "discrepancy",
                         size = 10) + 
  theme(axis.title  = element_text(family = "LM Roman 10"))

heatmap_plot_rpart + plot_spacer() + legend_plot + plot_layout(ncol = 3, widths = c(5, 1, 1))

# svm ##########################################################################
# gamma vs. cost

res_normalized <- merged_svm |>
  filter(degree  == merged_svm$degree[which(merged_rpart$default == TRUE)[1]]) |>
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
  mutate(gamma = as.factor(round(gamma, 4)),
         cost  = as.factor(round(cost, 4)))


heatmap_plot_svm <- ggplot(res_biscale, aes(x = gamma, y = cost, fill = bi_class)) + 
  geom_tile() + 
  bi_scale_fill(pal = "DkBlue2", dim = 3) + 
  theme_minimal() + 
  labs(title = "Support Vector Machines") + 
  theme(legend.position = "none", 
        text = element_text(size   = 14),
        plot.title  = element_text(family = "LM Roman 10"),
        axis.text   = element_text(family = "LM Roman 10"),
        axis.title  = element_text(family = "Courier")) + 
  scale_x_discrete(breaks = c(0.001, 0.0312, 1, 32, 1024)) + 
  scale_y_discrete(breaks = c(0.001, 0.0312, 1, 32, 1024)) 

legend_plot <- bi_legend(pal  = "DkBlue2", 
                         dim  = 3, 
                         xlab = "F1", 
                         ylab = "discrepancy",
                         size = 10) + 
  theme(axis.title  = element_text(family = "LM Roman 10"))

heatmap_plot_svm + 
  plot_spacer() + legend_plot + plot_layout(ncol = 2, widths = c(5, 1, 1))

# xgb ##########################################################################
# alpha vs. colsample_bytree

res_normalized <- merged_xgb |>
  filter(eta               == merged_xgb$eta[which(merged_xgb$default  == TRUE)[1]],
         subsample         == merged_xgb$subsample[which(merged_xgb$default == TRUE)[1]],
         max_depth         == merged_xgb$max_depth[which(merged_xgb$default  == TRUE)[1]],
         min_child_weight  == merged_xgb$min_child_weight[which(merged_xgb$default == TRUE)[1]],
         colsample_bylevel == merged_xgb$colsample_bylevel[which(merged_xgb$default  == TRUE)[1]],
         lambda            == merged_xgb$lambda[which(merged_xgb$default == TRUE)[1]]) |>
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
  mutate(alpha            = as.factor(round(alpha, 5)),
         colsample_bytree = as.factor(colsample_bytree))


heatmap_plot_xgb <- ggplot(res_biscale, aes(x = alpha, y = colsample_bytree, fill = bi_class)) + 
  geom_tile() + 
  bi_scale_fill(pal = "DkBlue2", dim = 3) + 
  theme_minimal() + 
  labs(title = "Extreme Gradient Boosting") + 
  theme(legend.position = "none", 
        text = element_text(size   = 14),
        plot.title  = element_text(family = "LM Roman 10"),
        axis.text   = element_text(family = "LM Roman 10"),
        axis.title  = element_text(family = "Courier")) + 
  scale_y_discrete(breaks = c(0, 0.5, 1))

legend_plot <- bi_legend(pal    = "DkBlue2", 
                         dim    = 3, 
                         xlab   = "F1", 
                         ylab   = "discrepancy") + 
               theme(axis.title  = element_text(family = "LM Roman 10",
                                                size   = 15))

heatmap_plot_xgb + 
  plot_spacer() + legend_plot + plot_layout(ncol = 2, widths = c(5, 1, 1))

################################################################################

smaller_legend <- plot_grid(legend_plot, scale = 0.6)

ggarrange(heatmap_plot_glmnet, heatmap_plot_rpart, heatmap_plot_svm, 
          heatmap_plot_ranger, heatmap_plot_xgb, smaller_legend,
          ncol  = 2, 
          nrow  = 3,
          align = "v")


