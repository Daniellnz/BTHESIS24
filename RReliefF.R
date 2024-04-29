library(tidyr)
library(ggplot2)
library(dplyr)
library(data.table)
library(reshape2)
library(randomForest)
library(caret)
library(gridExtra)
library(FSelectorRcpp)

sea_calib <- fread("/Users/daniellenz/Documents/TUM BWL/Bachelor Thesis 2023:2024/Data/sea_calib_v1.csv")
fuels_calib <- fread("/Users/daniellenz/Documents/TUM BWL/Bachelor Thesis 2023:2024/Data/fuels_calib_v1.csv")

outcome <- "OUT"
features <- c("KNOW", "GEND", "ETHN", "ORG", "TEAM", 
              "EXP", "EXPSH", "GNRL", "GEO")

model_formula <- as.formula(paste(outcome, "~", paste(features, collapse = " + ")))

set.seed("10")
importances <- relief(model_formula, fuels_calib, neighboursCount = 5, sampleSize = 20)
sort(importances$importance)
importances
importances <- importances[order(importances$importance),]

importances$attributes <- factor(importances$attributes, levels = importances$attributes)

ggplot(importances, aes(x = attributes, y = importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Features", y = "Weights", title = "Feature Weights") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#MULTIPLE RUNS -------------------- 
# Define the number of runs
num_runs <- 20

# Initialize a data frame to store importances
importance_table <- data.frame(features = features)

# Perform Relief algorithm multiple times
for (run in 1:num_runs) {
  # Set seed for reproducibility
  set.seed(run)
  
  # Run Relief algorithm
  importances <- relief(model_formula, fuels_calib, neighboursCount = 5, sampleSize = 20)
  
  # Store importances in the table
  importance_table[[as.character(run)]] <- importances$importance
}

# Calculate mean importances
importance_table$Mean_Importance <- rowMeans(importance_table[, -1])

importance_table <- importance_table[order(importance_table$Mean_Importance), ]

ggplot(importance_table, aes(x = reorder(features, Mean_Importance), y = Mean_Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Feature", y = "Feature Weight") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 10))










