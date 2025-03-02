# 27.02.2025
# Fig 2. The distribution of the multiplicibility of the hyperparameters of the models for the defaults
library(ggplot2)
library(readr)
library(forcats)
library(dplyr)
library(scales)
library(ggpubr)

setwd("/Volumes/LaCie/Multiplicibility/merged_csv")
merged_glmnet <- read_csv("merged_glmnet.csv")
merged_knn    <- read_csv("merged_knn.csv")
merged_ranger <- read_csv("merged_ranger.csv")
merged_rpart  <- read_csv("merged_rpart.csv")
merged_svm    <- read_csv("merged_svm.csv")
merged_xgb    <- read_csv("merged_xgb.csv")


# glmnet #######################################################################
# alpha
glmnet_alpha <- merged_glmnet |> 
  select(lambda, alpha, discrepancy, dataset, default) |>
  filter(lambda == merged_glmnet$lambda[which(merged_glmnet$default == TRUE)[1]]) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE)) |> 
  select(max_disc) |>
  mutate(parameter = "alpha",
         model     = "Elastic Net")

# lambda
glmnet_lambda <- merged_glmnet |> 
  select(lambda, alpha, discrepancy, dataset, default) |>
  filter(alpha == merged_glmnet$alpha[which(merged_glmnet$default == TRUE)[1]]) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE)) |> 
  select(max_disc) |>
  mutate(parameter = "lambda",
         model     = "Elastic Net") 
  
glmnet_disc <- rbind(glmnet_alpha, glmnet_lambda)


plot_glmnet <- ggplot(glmnet_disc, 
                      aes(x = fct_reorder(parameter, -max_disc), y = max_disc)) +
  geom_boxplot(staplewidth = 0.5) +
  #geom_swarm(aes(color = values), 
  #           size      = 3) + 
  #scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) + 
  labs(x = "", y = "") +  
  facet_grid(~ model) + 
  theme_bw() + 
  theme(legend.position = "none",
        text = element_text(size = 20,
                            family = "LM Roman 10"),
        axis.text.x = element_text(family = "Courier",
                                   size   = 10, 
                                   angle  = 45, 
                                   hjust  = 1)) 

# knn ##########################################################################
knn_k <- merged_knn |> 
  select(k, discrepancy, dataset, default) |>
  filter(default == FALSE) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE)) |> 
  select(max_disc) |>
  mutate(parameter = "k",
         model     = "k-Nearest Neighbor")

plot_knn <- ggplot(knn_k, aes(x = fct_reorder(parameter, -max_disc), y = max_disc)) +
  geom_boxplot(staplewidth = 0.5) +
  #geom_swarm(aes(color = values), 
  #           size      = 3) + 
  #scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) + 
  labs(x = "", y = "") +  
  facet_grid(~ model) + 
  theme_bw() + 
  theme(legend.position = "none",
        text = element_text(size = 20,
                            family = "LM Roman 10"),
        axis.text.x = element_text(family = "Courier",
                                   size   = 10, 
                                   angle  = 45, 
                                   hjust  = 1))

# rpart ########################################################################
# cp
rpart_cp <- merged_rpart |> 
  select(cp, maxdepth, minsplit, minbucket, discrepancy, dataset, default) |>
  filter(maxdepth  == merged_rpart$maxdepth[which(merged_rpart$default  == TRUE)[1]],
         minsplit  == merged_rpart$minsplit[which(merged_rpart$default  == TRUE)[1]],
         minbucket == merged_rpart$minbucket[which(merged_rpart$default == TRUE)[1]]) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE)) |> 
  select(max_disc) |>
  mutate(parameter = "cp",
         model     = "Decision Tree")

# maxdepth
rpart_maxdepth <- merged_rpart |> 
  select(cp, maxdepth, minsplit, minbucket, discrepancy, dataset, default) |>
  filter(cp        == merged_rpart$cp[which(merged_rpart$default  == TRUE)[1]],
         minsplit  == merged_rpart$minsplit[which(merged_rpart$default  == TRUE)[1]],
         minbucket == merged_rpart$minbucket[which(merged_rpart$default == TRUE)[1]]) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE)) |> 
  select(max_disc) |>
  mutate(parameter = "maxdepth",
         model     = "Decision Tree")

# minsplit
rpart_minsplit <- merged_rpart |> 
  select(cp, maxdepth, minsplit, minbucket, discrepancy, dataset, default) |>
  filter(cp        == merged_rpart$cp[which(merged_rpart$default  == TRUE)[1]],
         maxdepth  == merged_rpart$maxdepth[which(merged_rpart$default  == TRUE)[1]],
         minbucket == merged_rpart$minbucket[which(merged_rpart$default == TRUE)[1]]) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE)) |> 
  select(max_disc) |>
  mutate(parameter = "minsplit",
         model     = "Decision Tree")

# minbucket
rpart_minbucket <- merged_rpart |> 
  select(cp, maxdepth, minsplit, minbucket, discrepancy, dataset, default) |>
  filter(cp        == merged_rpart$cp[which(merged_rpart$default  == TRUE)[1]],
         maxdepth  == merged_rpart$maxdepth[which(merged_rpart$default  == TRUE)[1]],
         minsplit  == merged_rpart$minsplit[which(merged_rpart$default == TRUE)[1]]) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE)) |> 
  select(max_disc) |>
  mutate(parameter = "minbucket",
         model     = "Decision Tree")

rpart_disc <- rbind(rpart_cp, rpart_maxdepth, rpart_minsplit, rpart_minbucket)

plot_rpart <- ggplot(rpart_disc, aes(x = fct_reorder(parameter, -max_disc), y = max_disc)) +
  geom_boxplot(staplewidth = 0.5) +
  #geom_swarm(aes(color = values), 
  #           size      = 3) + 
  #scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) + 
  labs(x = "", y = "") +  
  facet_grid(~ model) + 
  theme_bw() + 
  theme(legend.position = "none",
        text = element_text(size = 20,
                            family = "LM Roman 10"),
        axis.text.x = element_text(family = "Courier",
                                   size   = 10, 
                                   angle  = 45, 
                                   hjust  = 1))

# xgb ##########################################################################
# eta
xgb_eta <- merged_xgb |> 
  select(eta, subsample, max_depth, min_child_weight, colsample_bytree, colsample_bylevel,
         alpha, lambda, discrepancy, dataset, default) |>
  filter(subsample         == merged_xgb$subsample[which(merged_xgb$default  == TRUE)[1]],
         max_depth         == merged_xgb$max_depth[which(merged_xgb$default  == TRUE)[1]],
         min_child_weight  == merged_xgb$min_child_weight[which(merged_xgb$default == TRUE)[1]],
         colsample_bytree  == merged_xgb$colsample_bytree[which(merged_xgb$default  == TRUE)[1]],
         colsample_bylevel == merged_xgb$colsample_bylevel[which(merged_xgb$default  == TRUE)[1]],
         alpha             == merged_xgb$alpha[which(merged_xgb$default == TRUE)[1]],
         lambda            == merged_xgb$lambda[which(merged_xgb$default == TRUE)[1]]) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE)) |> 
  select(max_disc) |>
  mutate(parameter = "eta",
         model     = "Extreme Gradient Boosting")

# subsample
xgb_subsample <- merged_xgb |> 
  select(eta, subsample, max_depth, min_child_weight, colsample_bytree, colsample_bylevel,
         alpha, lambda, discrepancy, dataset, default) |>
  filter(eta               == merged_xgb$eta[which(merged_xgb$default  == TRUE)[1]],
         max_depth         == merged_xgb$max_depth[which(merged_xgb$default  == TRUE)[1]],
         min_child_weight  == merged_xgb$min_child_weight[which(merged_xgb$default == TRUE)[1]],
         colsample_bytree  == merged_xgb$colsample_bytree[which(merged_xgb$default  == TRUE)[1]],
         colsample_bylevel == merged_xgb$colsample_bylevel[which(merged_xgb$default  == TRUE)[1]],
         alpha             == merged_xgb$alpha[which(merged_xgb$default == TRUE)[1]],
         lambda            == merged_xgb$lambda[which(merged_xgb$default == TRUE)[1]]) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE)) |> 
  select(max_disc) |>
  mutate(parameter = "subsample",
         model     = "Extreme Gradient Boosting")

# max_depth
xgb_max_depth <- merged_xgb |> 
  select(eta, subsample, max_depth, min_child_weight, colsample_bytree, colsample_bylevel,
         alpha, lambda, discrepancy, dataset, default) |>
  filter(eta               == merged_xgb$eta[which(merged_xgb$default  == TRUE)[1]],
         subsample         == merged_xgb$subsample[which(merged_xgb$default  == TRUE)[1]],
         min_child_weight  == merged_xgb$min_child_weight[which(merged_xgb$default == TRUE)[1]],
         colsample_bytree  == merged_xgb$colsample_bytree[which(merged_xgb$default  == TRUE)[1]],
         colsample_bylevel == merged_xgb$colsample_bylevel[which(merged_xgb$default  == TRUE)[1]],
         alpha             == merged_xgb$alpha[which(merged_xgb$default == TRUE)[1]],
         lambda            == merged_xgb$lambda[which(merged_xgb$default == TRUE)[1]]) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE)) |> 
  select(max_disc) |>
  mutate(parameter = "max_depth",
         model     = "Extreme Gradient Boosting")

# min_child_weight
xgb_min_child_weight <- merged_xgb |> 
  select(eta, subsample, max_depth, min_child_weight, colsample_bytree, colsample_bylevel,
         alpha, lambda, discrepancy, dataset, default) |>
  filter(eta               == merged_xgb$eta[which(merged_xgb$default  == TRUE)[1]],
         subsample         == merged_xgb$subsample[which(merged_xgb$default  == TRUE)[1]],
         max_depth         == merged_xgb$max_depth[which(merged_xgb$default == TRUE)[1]],
         colsample_bytree  == merged_xgb$colsample_bytree[which(merged_xgb$default  == TRUE)[1]],
         colsample_bylevel == merged_xgb$colsample_bylevel[which(merged_xgb$default  == TRUE)[1]],
         alpha             == merged_xgb$alpha[which(merged_xgb$default == TRUE)[1]],
         lambda            == merged_xgb$lambda[which(merged_xgb$default == TRUE)[1]]) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE)) |> 
  select(max_disc) |>
  mutate(parameter = "min_child_weight",
         model     = "Extreme Gradient Boosting")

# colsample_bytree
xgb_colsample_bytree <- merged_xgb |> 
  select(eta, subsample, max_depth, min_child_weight, colsample_bytree, colsample_bylevel,
         alpha, lambda, discrepancy, dataset, default) |>
  filter(eta               == merged_xgb$eta[which(merged_xgb$default  == TRUE)[1]],
         subsample         == merged_xgb$subsample[which(merged_xgb$default  == TRUE)[1]],
         max_depth         == merged_xgb$max_depth[which(merged_xgb$default == TRUE)[1]],
         min_child_weight  == merged_xgb$min_child_weight[which(merged_xgb$default  == TRUE)[1]],
         colsample_bylevel == merged_xgb$colsample_bylevel[which(merged_xgb$default  == TRUE)[1]],
         alpha             == merged_xgb$alpha[which(merged_xgb$default == TRUE)[1]],
         lambda            == merged_xgb$lambda[which(merged_xgb$default == TRUE)[1]]) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE)) |> 
  select(max_disc) |>
  mutate(parameter = "colsample_bytree",
         model     = "Extreme Gradient Boosting")

# colsample_bylevel
xgb_colsample_bylevel <- merged_xgb |> 
  select(eta, subsample, max_depth, min_child_weight, colsample_bytree, colsample_bylevel,
         alpha, lambda, discrepancy, dataset, default) |>
  filter(eta               == merged_xgb$eta[which(merged_xgb$default  == TRUE)[1]],
         subsample         == merged_xgb$subsample[which(merged_xgb$default  == TRUE)[1]],
         max_depth         == merged_xgb$max_depth[which(merged_xgb$default == TRUE)[1]],
         min_child_weight  == merged_xgb$min_child_weight[which(merged_xgb$default  == TRUE)[1]],
         colsample_bytree  == merged_xgb$colsample_bytree[which(merged_xgb$default  == TRUE)[1]],
         alpha             == merged_xgb$alpha[which(merged_xgb$default == TRUE)[1]],
         lambda            == merged_xgb$lambda[which(merged_xgb$default == TRUE)[1]]) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE)) |> 
  select(max_disc) |>
  mutate(parameter = "colsample_bylevel",
         model     = "Extreme Gradient Boosting")

# alpha
xgb_alpha <- merged_xgb |> 
  select(eta, subsample, max_depth, min_child_weight, colsample_bytree, colsample_bylevel,
         alpha, lambda, discrepancy, dataset, default) |>
  filter(eta               == merged_xgb$eta[which(merged_xgb$default  == TRUE)[1]],
         subsample         == merged_xgb$subsample[which(merged_xgb$default  == TRUE)[1]],
         max_depth         == merged_xgb$max_depth[which(merged_xgb$default == TRUE)[1]],
         min_child_weight  == merged_xgb$min_child_weight[which(merged_xgb$default  == TRUE)[1]],
         colsample_bytree  == merged_xgb$colsample_bytree[which(merged_xgb$default  == TRUE)[1]],
         colsample_bylevel == merged_xgb$colsample_bylevel[which(merged_xgb$default == TRUE)[1]],
         lambda            == merged_xgb$lambda[which(merged_xgb$default == TRUE)[1]]) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE)) |> 
  select(max_disc) |>
  mutate(parameter = "alpha",
         model     = "Extreme Gradient Boosting")

# lambda
xgb_lambda <- merged_xgb |> 
  select(eta, subsample, max_depth, min_child_weight, colsample_bytree, colsample_bylevel,
         alpha, lambda, discrepancy, dataset, default) |>
  filter(eta               == merged_xgb$eta[which(merged_xgb$default  == TRUE)[1]],
         subsample         == merged_xgb$subsample[which(merged_xgb$default  == TRUE)[1]],
         max_depth         == merged_xgb$max_depth[which(merged_xgb$default == TRUE)[1]],
         min_child_weight  == merged_xgb$min_child_weight[which(merged_xgb$default  == TRUE)[1]],
         colsample_bytree  == merged_xgb$colsample_bytree[which(merged_xgb$default  == TRUE)[1]],
         colsample_bylevel == merged_xgb$colsample_bylevel[which(merged_xgb$default == TRUE)[1]],
         alpha             == merged_xgb$alpha[which(merged_xgb$default == TRUE)[1]]) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE)) |> 
  select(max_disc) |>
  mutate(parameter = "lambda",
         model     = "Extreme Gradient Boosting")

xgb_disc <- rbind(xgb_eta, xgb_subsample, xgb_max_depth, xgb_min_child_weight,
                  xgb_colsample_bytree, xgb_colsample_bylevel, xgb_alpha, xgb_lambda)

plot_xgb <- ggplot(xgb_disc, aes(x = fct_reorder(parameter, -max_disc), y = max_disc)) +
  geom_boxplot(staplewidth = 0.5) +
  #geom_swarm(aes(color = values), 
  #           size      = 3) + 
  #scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) + 
  labs(x = "", y = "") +  
  facet_grid(~ model) + 
  theme_bw() + 
  theme(legend.position = "none",
        text = element_text(size = 20,
                            family = "LM Roman 10"),
        axis.text.x = element_text(family = "Courier",
                                   size   = 10, 
                                   angle  = 45, 
                                   hjust  = 1)) 

# svm ##########################################################################
# Identify the default gamma value for each dataset
default_gamma <- merged_svm |> 
  filter(default == TRUE) |> 
  select(dataset, gamma) |> 
  distinct()

# cost
svm_cost <- merged_svm |>  
  select(cost, degree, gamma, discrepancy, dataset, default) |>  
  inner_join(default_gamma, by = "dataset") |>  # Match default gamma by dataset
  filter(gamma.x == gamma.y,
         degree  == merged_svm$degree[which(merged_svm$default  == TRUE)[1]]) |>  # Keep only rows where gamma is the default
  group_by(dataset) |>  
  summarise(max_disc = max(discrepancy, na.rm = TRUE), .groups = "drop") |>  
  mutate(parameter = "cost",  
         model     = "Support Vector Machines")  

# degree
svm_degree <- merged_svm |>  
  select(cost, degree, gamma, discrepancy, dataset, default) |>  
  inner_join(default_gamma, by = "dataset") |>  # Match default gamma by dataset
  filter(gamma.x == gamma.y,
         cost    == merged_svm$cost[which(merged_svm$default  == TRUE)[1]]) |>  # Keep only rows where gamma is the default
  group_by(dataset) |>  
  summarise(max_disc = max(discrepancy, na.rm = TRUE), .groups = "drop") |>  
  mutate(parameter = "degree",  
         model     = "Support Vector Machines")  

# gamma
svm_gamma <- merged_svm |> 
  select(cost, degree, gamma, discrepancy, dataset, default) |>
  filter(cost   == merged_svm$cost[which(merged_svm$default  == TRUE)[1]],
         degree == merged_svm$degree[which(merged_svm$default  == TRUE)[1]]) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy, na.rm = TRUE), .groups = "drop") |> 
  mutate(parameter = "gamma",
         model     = "Support Vector Machines")

svm_disc <- rbind(svm_cost, svm_degree, svm_gamma)

plot_svm <- ggplot(svm_disc, aes(x = fct_reorder(parameter, -max_disc), y = max_disc)) +
  geom_boxplot(staplewidth = 0.5) +
  #geom_swarm(aes(color = values), 
  #           size      = 3) + 
  #scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) + 
  labs(x = "", y = "") +  
  facet_grid(~ model) + 
  theme_bw() + 
  theme(legend.position = "none",
        text = element_text(size = 20,
                            family = "LM Roman 10"),
        axis.text.x = element_text(family = "Courier",
                                   size   = 10, 
                                   angle  = 45, 
                                   hjust  = 1)) 

# ranger #######################################################################
# Identify the default mtry value for each dataset
default_mtry <- merged_ranger |> 
  filter(default == TRUE) |> 
  select(dataset, mtry) |> 
  distinct()

# num.trees
ranger_num.trees <- merged_ranger |>  
  select(num_trees, mtry, min_node_size, sample_fraction, discrepancy, dataset, default) |>  
  inner_join(default_mtry, by = "dataset") |>  # Match default gamma by dataset
  filter(mtry.x          == mtry.y,
         min_node_size   == merged_ranger$min_node_size[which(merged_ranger$default  == TRUE)[1]],
         sample_fraction == merged_ranger$sample_fraction[which(merged_ranger$default  == TRUE)[1]]) |> 
  group_by(dataset) |>  
  summarise(max_disc = max(discrepancy, na.rm = TRUE), .groups = "drop") |>  
  mutate(parameter = "num.trees",  
         model     = "Random Forests")  

# mtry
ranger_mtry <- merged_ranger |>  
  select(num_trees, mtry, min_node_size, sample_fraction, discrepancy, dataset, default) |>  
  filter(num_trees       == merged_ranger$num_trees[which(merged_ranger$default  == TRUE)[1]],
         min_node_size   == merged_ranger$min_node_size[which(merged_ranger$default  == TRUE)[1]],
         sample_fraction == merged_ranger$sample_fraction[which(merged_ranger$default  == TRUE)[1]]) |>  # Keep only rows where gamma is the default
  group_by(dataset) |>  
  summarise(max_disc = max(discrepancy, na.rm = TRUE), .groups = "drop") |>  
  mutate(parameter = "mtry",  
         model     = "Random Forests")  

# min.node.size
ranger_min.node.size <- merged_ranger |>  
  select(num_trees, mtry, min_node_size, sample_fraction, discrepancy, dataset, default) |>  
  inner_join(default_mtry, by = "dataset") |>  # Match default gamma by dataset
  filter(mtry.x          == mtry.y,
         num_trees       == merged_ranger$num_trees[which(merged_ranger$default  == TRUE)[1]],
         sample_fraction == merged_ranger$sample_fraction[which(merged_ranger$default  == TRUE)[1]]) |> 
  group_by(dataset) |>  
  summarise(max_disc = max(discrepancy, na.rm = TRUE), .groups = "drop") |>  
  mutate(parameter = "min.node.size",  
         model     = "Random Forests") 

# sample.fraction
ranger_sample.fraction <- merged_ranger |>  
  select(num_trees, mtry, min_node_size, sample_fraction, discrepancy, dataset, default) |>  
  inner_join(default_mtry, by = "dataset") |>  # Match default gamma by dataset
  filter(mtry.x        == mtry.y,
         min_node_size == merged_ranger$min_node_size[which(merged_ranger$default  == TRUE)[1]],
         num_trees     == merged_ranger$num_trees[which(merged_ranger$default  == TRUE)[1]]) |> 
  group_by(dataset) |>  
  summarise(max_disc = max(discrepancy, na.rm = TRUE), .groups = "drop") |>  
  mutate(parameter = "sample.fraction",  
         model     = "Random Forests") 

ranger_disc <- rbind(ranger_num.trees, ranger_mtry, ranger_min.node.size, ranger_sample.fraction)

plot_ranger <- ggplot(ranger_disc, aes(x = fct_reorder(parameter, -max_disc), y = max_disc)) +
  geom_boxplot(staplewidth = 0.5) +
  #geom_swarm(aes(color = values), 
  #           size      = 3) + 
  #scale_color_gradient(low = "blue", high = "red") +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) + 
  labs(x = "", y = "") +  
  facet_grid(~ model) + 
  theme_bw() + 
  theme(legend.position = "none",
        text = element_text(size = 20,
                            family = "LM Roman 10"),
        axis.text.x = element_text(family = "Courier",
                                   size   = 10, 
                                   angle  = 45, 
                                   hjust  = 1))

# combining plots
merged_plot <- ggarrange(plot_glmnet, plot_rpart, plot_knn, plot_svm, plot_ranger, plot_xgb,
                         ncol    = 2, 
                         nrow    = 3,
                         align   = "h")

annotate_figure(merged_plot,
                left   = text_grob("discrepancy", rot = 90, vjust = 1, size = 20, family = "LM Roman 10"))