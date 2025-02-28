merge_csv <- function(folder_path) {
  # Define dataset names
  data_names <- c("abalone_19", "abalone", "churn", 
                  "jm1", "kc1", "MagicTelescope", 
                  "mammography", "ozone_level_8hr", "pc1", 
                  "pc3", "pc4", "phoneme", 
                  "qsar_biodeg", "spambase", "SpeedDating", 
                  "steel_plates_fault", "us_crime", "wilt", 
                  "wine_quality", "yeast_me2", "yeast_ml8")
  
  # List all CSV files in the specified folder
  files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Check if there are any CSV files in the folder
  if (length(files) == 0) {
    stop("No CSV files found in the specified folder!")
  }
  
  # Read and process each CSV file
  data_list <- lapply(seq_along(files), function(i) {
    data <- read.csv(files[i])  # Read the CSV file
    
    # Ensure index does not exceed the length of data_names
    if (i <= length(data_names)) {
      dataset_name <- data_names[i]  # Get the corresponding dataset name
    } else {
      dataset_name <- NA  # If index is out of range, assign NA
    }
    
    # Add dataset name as a new column
    data$dataset <- dataset_name
    
    return(data)
  })
  
  # Combine all data frames into one
  combined_data <- bind_rows(data_list)
  
  return(combined_data)
}

# Example usage:
# path <- "/Volumes/LaCie/Multiplicibility/csv/csv_rpart"
# merged_data <- merge_csv(path)
# head(merged_data)

setwd("/Volumes/LaCie/Multiplicibility/merged_csv")

merged_rpart <- merge_csv("/Volumes/LaCie/Multiplicibility/csv/csv_rpart")
write_csv(merged_rpart, "merged_rpart.csv")

merged_ranger <- merge_csv("/Volumes/LaCie/Multiplicibility/csv/csv_ranger")
write_csv(merged_ranger, "merged_ranger.csv")

merged_svm <- merge_csv("/Volumes/LaCie/Multiplicibility/csv/csv_svm")
write_csv(merged_svm, "merged_svm.csv")

merged_xgb <- merge_csv("/Volumes/LaCie/Multiplicibility/csv/csv_xgb")
write_csv(merged_xgb, "merged_xgb.csv")

merged_glmnet <- merge_csv("/Volumes/LaCie/Multiplicibility/csv/csv_glmnet")
write_csv(merged_glmnet, "merged_glmnet.csv")

merged_knn <- merge_csv("/Volumes/LaCie/Multiplicibility/csv/csv_knn")
write_csv(merged_knn, "merged_knn.csv")