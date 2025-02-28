# 27.02.2025
# Fig 1. The distribution of the multiplicibility of the models for the defaults
library(ggplot2)
library(readr)
library(forcats)

setwd("/Volumes/LaCie/Multiplicibility/merged_csv")

merged_glmnet <- read_csv("merged_glmnet.csv")
merged_knn    <- read_csv("merged_knn.csv")
merged_ranger <- read_csv("merged_ranger.csv")
merged_rpart  <- read_csv("merged_rpart.csv")
merged_svm    <- read_csv("merged_svm.csv")
merged_xgb    <- read_csv("merged_xgb.csv")

max_disc_glmnet <- merged_glmnet |> 
  select(model, dataset, default, discrepancy) |>
  filter(default == FALSE) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy)) |>
  mutate(model = "glmnet")

max_disc_knn <- merged_knn |> 
  select(model, dataset, default, discrepancy) |>
  filter(default == FALSE) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy)) |>
  mutate(model = "knn")

max_disc_ranger <- merged_ranger |> 
  select(model, dataset, default, discrepancy) |>
  filter(default == FALSE) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy)) |>
  mutate(model = "ranger")

max_disc_rpart <- merged_rpart |> 
  select(model, dataset, default, discrepancy) |>
  filter(default == FALSE) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy)) |>
  mutate(model = "rpart")

max_disc_svm <- merged_svm |> 
  select(dataset, default, discrepancy) |>
  filter(default == FALSE) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy)) |>
  mutate(model = "svm")

max_disc_xgb <- merged_xgb |> 
  select(model, dataset, default, discrepancy) |>
  filter(default == FALSE) |>
  group_by(dataset) |>
  summarise(max_disc = max(discrepancy)) |>
  mutate(model = "xgb")


disc <- rbind(max_disc_glmnet,
              max_disc_knn,
              max_disc_ranger,
              max_disc_rpart,
              max_disc_svm,
              max_disc_xgb)

ggplot(disc, aes(x = fct_reorder(model, max_disc),
                 y = max_disc)) + 
  geom_boxplot(staplewidth = 0.5) + 
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_x_discrete(labels = c("svm"    = "Support \nVector \nMachines ", 
                              "rpart"  = "Decision \nTree ",
                              "ranger" = "Random \nForests ",
                              "knn"    = "k-Nearest \nNeighbor ",
                              "glmnet" = "Elastic \nNet ",
                              "xgb"    = "Extreme \nGradient \nBoosting")) +  # X ekseni etiketlerini değiştir
  labs(x = "",
       y = "discrepancy") +
  coord_flip() +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.ticks.length.y = unit(0, "cm"),
        panel.grid.major.y  = element_blank(), 
        panel.grid.minor.y  = element_blank())


