# Load required libraries
library(tidyr)         # For data tidying
library(ggplot2)       # For data visualization
library(dplyr)         # For data manipulation
library(data.table)    # For efficient data manipulation
library(reshape2)      # For data reshaping
library(randomForest)  # For random forest modeling
library(caret)         # For data partitioning
library(gridExtra)     # For arranging multiple plots

#IMPLEMENTATION FOR SEA DATASET
# Read the sea_calib and fuels_calib datasets
sea_calib <- fread("/Users/daniellenz/Documents/TUM BWL/Bachelor Thesis 2023:2024/Data/sea_calib_v1.csv")

# Define outcome variable and features
outcome <- "OUT"
features <- c("KNOW", "GEND", "ETHN", "ORG", "TEAM", "EXP", "EXPSH", "GNRL", "GEO")

#create model formula
model_formula <- as.formula(paste(outcome, "~", paste(features, collapse = " + ")))

#Random Forest model Regression -  Full Model with all features
set.seed("101")
train_ind <- createDataPartition(sea_calib$OUT, p = 0.8, list = FALSE)
train_data <- sea_calib[train_ind, ]
test_data <- sea_calib[-train_ind, ]

rf_model <- randomForest(model_formula, data = train_data)

sea_calib$preds_rf <- predict(rf_model, newdata = sea_calib)

#Mean Absolute Error 
mae <- mean(abs(sea_calib$preds_rf - sea_calib$OUT))
print(paste("Mean Absolute Error (MAE):", mae))

# Extract feature importances
importances <- importance(rf_model)

# Create data frame to save feature importances
feature_importances <- data.frame(
  Feature = rownames(importances),
  Importance = importances[, "IncNodePurity"]
)

# Standardize importances
feature_importances$Standardized_Importance <- feature_importances$Importance / sum(feature_importances$Importance)

# Sort the data frame by standardized importance values
sorted_feature_importances <- feature_importances[order(feature_importances$Standardized_Importance), ]

# Plot original feature importances
ggplot(sorted_feature_importances, aes(x = reorder(Feature, Standardized_Importance), y = Standardized_Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(x = "Feature", y = "Decrease in Node Impurity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 10))


#IMPLEMENTATION FUELS DATASET
# Read the sea_calib and fuels_calib datasets
fuels_calib <- fread("/Users/daniellenz/Documents/TUM BWL/Bachelor Thesis 2023:2024/Data/fuels_calib_v1.csv")

# Define outcome variable and features
outcome <- "OUT"
features <- c("KNOW", "GEND", "ETHN", "ORG", "TEAM", "EXP", "EXPSH", "GNRL", "GEO")

# Create model formula
model_formula <- as.formula(paste(outcome, "~", paste(features, collapse = " + ")))

# Randomly partition the data into training and testing sets
set.seed("101")
train_ind <- createDataPartition(fuels_calib$OUT, p = 0.8, list = FALSE)
train_data <- fuels_calib[train_ind, ]
test_data <- fuels_calib[-train_ind, ]

# Train the random forest model
rf_model <- randomForest(model_formula, data = train_data)

# Make predictions on the sea_calib dataset
fuels_calib$preds_rf <- predict(rf_model, newdata = fuels_calib)

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(fuels_calib$preds_rf - fuels_calib$OUT))
print(paste("Mean Absolute Error (MAE):", mae))

# Extract feature importances from the trained random forest model
importances <- importance(rf_model)

# Create data frame to store feature importances
feature_importances <- data.frame(
  Feature = rownames(importances),
  Importance = importances[, "IncNodePurity"]
)

# Standardize importances
feature_importances$Standardized_Importance <- feature_importances$Importance / sum(feature_importances$Importance)

# Sort the data frame by standardized importance values
sorted_feature_importances <- feature_importances[order(feature_importances$Standardized_Importance), ]

# Plot original feature importances
ggplot(sorted_feature_importances, aes(x = reorder(Feature, Standardized_Importance), y = Standardized_Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(x = "Feature", y = "Decrease in Node Impurity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 10))



