library(tidyr)
library(ggplot2)
library(dplyr)
library(data.table)
library(reshape2)
library(ggplot2)
library(cowplot)
library("FCBF")
library(SummarizedExperiment)

sea_calib_copy <- fread("/Users/daniellenz/Documents/TUM BWL/Bachelor Thesis 2023:2024/Data/sea_calib_v1.csv")
fuels_calib <- fread("/Users/daniellenz/Documents/TUM BWL/Bachelor Thesis 2023:2024/Data/fuels_calib_v1.csv")

features <- names(sea_calib_copy)[1:9]
outcome <- "OUT"

feature_cols <- c("KNOW", "GEND", "ETHN", "ORG", "TEAM", "EXP", "EXPSH", "GNRL", "GEO")

data_matrix <- as.matrix(sea_calib[, ..features])
data_matrix

sea_calib[, (features) := lapply(.SD, cut, breaks = 5, labels = FALSE), .SDcols = features]
sea_calib[, "KNOW" := factor(KNOW)]
sea_calib[, "GEND" := factor(GEND)]
sea_calib[, "ETHN" := factor(ETHN)]
sea_calib[, "ORG" := factor(ORG)]
sea_calib[, "TEAM" := factor(TEAM)]
sea_calib[, "EXP" := factor(EXP)]
sea_calib[, "EXPSH" := factor(EXPSH)]
sea_calib[, "GNRL" := factor(GNRL)]
sea_calib[, "GEO" := factor(GEO)]

sea_calib[, (outcome) := lapply(.SD, cut, breaks = 5, labels = FALSE), .SDcols = outcome]
sea_calib[[outcome]] <- factor(sea_calib[[outcome]])
is.factor <- is.factor(sea_calib[[outcome]])

feature_scores <- fcbf(as.matrix(sea_calib[[c("KNOW", "GEND", "ETHN", "ORG", 
                                              "TEAM", "EXP", "EXPSH", 
                                              "GNRL", "GEO")]]), sea_calib[[outcome]])

data_matrix_transposed <- t(as.matrix(sea_calib[, 1:9]))
data_matrix_transposed

fcbf_features <- fcbf(data_matrix_transposed, sea_calib[[10]], minimum_su = 0.01)

data_matrix <- as.matrix(sea_calib[, 1:9])

su_plot(sea_calib[["KNOW"]], sea_calib[[outcome]])

sea_calib[["KNOW"]]
is.factor(sea_calib[[outcome]])


fuels_calib <- fread("/Users/daniellenz/Documents/TUM BWL/Bachelor Thesis 2023:2024/Data/fuels_calib_v1.csv")

features <- c("KNOW", "GEND", "ETHN", "ORG", "TEAM", "EXP", "EXPSH", "GNRL", "GEO")

fuels_calib[, (features) := lapply(.SD, function(x) cut(x, breaks = 5, labels = FALSE)), .SDcols = features]
fuels_calib[, (features) := lapply(.SD, as.factor), .SDcols = features]

# Discretize the outcome column
fuels_calib[, OUT := cut(OUT, breaks = 5, labels = F), .SDcols = outcome]
fuels_calib[, OUT := as.factor(OUT)]

is.factor(fuels_calib$KNOW)
is.factor(fuels_calib$OUT)

fuels_calib[, 1:9]

fuels_calib[[10]] <- as.matrix(fuels_calib[[10]])
fuels_transposed<- t(as.matrix(fuels_calib[, 1:9]))
fuels_transposed

su_plot(fuels_transposed, fuels_calib[[10]])

su_values_f <- get_su_for_feature_table_and_vector(fuels_transposed, fuels_calib[[10]])
su_values
names(su_values_f)[names(su_values_f) == "sort(su_values_for_features_with_regards_to_class, decreasing = TRUE)"] <- "SU_value"
names(su_values_f)[names(su_values_f) == "gene"] <- "Feature"

su_histo_f <- ggplot(su_values_f, aes(x = SU_value)) + geom_histogram(bins = 3) +
  labs(title = "Histogram of SU Values", y = "Count of Features") +
  scale_x_continuous(breaks = seq(0, 0.3, by = 0.005)) +  # Adjust y-axis breaks
  theme(axis.text.x = element_text(hjust = 1), 
  plot.title = element_text(hjust = 0.5),
  axis.title = element_text(size = 10))

su_boxplot_f <- ggplot(su_values_f, aes(x = 1, y = SU_value)) +
  geom_boxplot() +
  labs(title = "Boxplot of SU Values") + 
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.005)) +  # Adjust y-axis breaks
  theme(axis.text.x = element_text(hjust = 1), 
  plot.title = element_text(hjust = 0.5),
  axis.title = element_text(size = 10))

combined_plot <- cowplot::plot_grid(su_histo_f, su_boxplot_f, ncol = 2)
combined_plot

fcbf_features_f <- fcbf(fuels_transposed, fuels_calib[[10]], minimum_su = 0.01)
fcbf_features_copy$index

su_plot <- ggplot(su_values_f, aes(x = reorder(Feature, SU_value), y = SU_value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Feature", y = "SU Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 10))

su_plot
combined_plot

#SEA DATASET FCBF IMPLEMENTATION ------------------------------------------------
sea_calib <- fread("/Users/daniellenz/Documents/TUM BWL/Bachelor Thesis 2023:2024/Data/sea_calib_v1.csv")

sea_calib[, (features) := lapply(.SD, function(x) cut(x, breaks = 5, labels = FALSE)), .SDcols = features]
sea_calib[, (features) := lapply(.SD, as.factor), .SDcols = features]

# Discretize the outcome column
sea_calib[, OUT := cut(OUT, breaks = 5, labels = F), .SDcols = outcome]
sea_calib[, OUT := as.factor(OUT)]

is.factor(sea_calib$KNOW)
is.factor(sea_calib$OUT)

sea_calib[, 1:9]

sea_calib[[10]] <- as.matrix(sea_calib[[10]])
sea_transposed<- t(as.matrix(sea_calib[, 1:9]))

su_plot(sea_transposed, sea_calib[[10]])

su_values_s <- get_su_for_feature_table_and_vector(sea_transposed, sea_calib[[10]])
su_values

names(su_values_s)[names(su_values_s) == "sort(su_values_for_features_with_regards_to_class, decreasing = TRUE)"] <- "SU_value"
names(su_values_s)[names(su_values_s) == "gene"] <- "Feature"

ig_values <- get_ig_for_feature_table_and_vector(sea_transposed, sea_calib[[10]])

su_histo_s <- ggplot(su_values_s, aes(x = SU_value)) + geom_histogram(bins = 3) +
  labs(title = "Histogram of SU Values", y = "Count of Features") +
  scale_x_continuous(breaks = seq(0, 0.3, by = 0.005)) +  # Adjust y-axis breaks
  theme(axis.text.x = element_text(hjust = 1), 
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 10))

su_boxplot_s <- ggplot(su_values_s, aes(x = 1, y = SU_value)) +
  geom_boxplot() +
  labs(title = "Boxplot of SU Values") + 
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.005)) +  # Adjust y-axis breaks
  theme(axis.text.x = element_text(hjust = 1), 
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 10))

combined_plot <- cowplot::plot_grid(su_histo_s, su_boxplot_s, ncol = 2)
combined_plot

fcbf_features_s <- fcbf(sea_transposed, sea_calib[[10]], minimum_su = 0.01)
fcbf_features_copy$index

su_plot <- ggplot(su_values_s, aes(x = reorder(Feature, SU_value), y = SU_value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Feature", y = "SU Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 10))



