library(ggplot2)
library(biscale)
library(dplyr)

res_normalized <- data_combined |>
  group_by(num.trees, sample.fraction) |>
  summarise(mean_auc         = mean(auc), 
            mean_discrepancy = mean(discrepancy)) |>
  mutate(across(c(mean_auc, mean_discrepancy), rescale))

res_biscale <- bi_class(res_normalized, 
                        x      = mean_auc, 
                        y      = mean_discrepancy, 
                        style  = "equal",
                        dim    = 3)

res_biscale <- res_biscale |>
  mutate(num.trees = as.factor(num.trees),
         sample.fraction = as.factor(sample.fraction))

heatmap_plot <- ggplot(res_biscale, aes(x = num.trees, y = sample.fraction, fill = bi_class)) +
  geom_tile() +
  bi_scale_fill(pal = "DkBlue2", dim = 3) +
  #bi_theme() + 
  theme_minimal() + 
  theme(legend.position = "none",
        text = element_text(size = 20)) 

legend_plot <- bi_legend(pal  = "DkBlue2", 
                         dim  = 3, 
                         xlab = "normalized mean AUC", 
                         ylab = "normalized mean discrepancy",
                         size = 10)

heatmap_plot + plot_spacer() + legend_plot + plot_layout(ncol = 3, widths = c(5, 1, 1))