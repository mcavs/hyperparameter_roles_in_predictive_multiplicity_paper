library(ggplot2)
library(readr)
library(forcats)
library(extrafont)
library(dplyr)

font_import(pattern = "cm")

# prepare data

merged_glmnet <- read_csv("merged_glmnet.csv")
merged_knn    <- read_csv("merged_knn.csv")
merged_ranger <- read_csv("merged_ranger.csv")
merged_rpart  <- read_csv("merged_rpart.csv")
merged_svm    <- read_csv("merged_svm.csv")
merged_xgb    <- read_csv("merged_xgb.csv")

merged_all <- rbind(data.frame(model = "glmnet", merged_glmnet[,c("f1", "auc", "discrepancy","dataset")]),
                    data.frame(model = "knn", merged_knn[,c("f1", "auc", "discrepancy","dataset")]),
                    data.frame(model = "ranger", merged_ranger[,c("f1", "auc", "discrepancy","dataset")]),
                    data.frame(model = "xgb", merged_xgb[,c("f1", "auc", "discrepancy","dataset")]),
                    data.frame(model = "svm", merged_svm[,c("f1", "auc", "discrepancy","dataset")]),
                    data.frame(model = "rpart", merged_rpart[,c("f1", "auc", "discrepancy","dataset")]))

# select data for presentation

merged_selected <- merged_all |> 
  filter(dataset %in% c("jm1", "MagicTelescope", "SpeedDating",
                         "ozone_level_8hr","yeast_me2", "steel_plates_fault"))

ggplot(merged_selected, aes(f1, discrepancy, color=model)) +
  geom_point(size=0.3, alpha=0.5) +
  #  geom_smooth(size=0.1, se=FALSE) +
  theme_bw() + xlab("F1 model performance") +
  facet_wrap(~dataset, ncol=3) +
  coord_cartesian(xlim = c(0.8,1), ylim = c(0,0.3))


