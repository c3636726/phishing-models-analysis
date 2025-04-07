# Install required packages (if not already installed)
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("Amelia")) install.packages("Amelia")
if (!require("mice")) install.packages("mice")
if (!require("corrplot")) install.packages("corrplot")
if (!require("ggplot2")) install.packages("ggplot2")

# Load libraries
library(dplyr)
library(tidyverse)
library(Amelia)
library(mice)
library(corrplot)
library(ggplot2)

# Load the dataset
data <- read.csv("Phishing_Legitimate.csv")

# Display basic information about the dataset
str(data)
summary(data)

# Commit test
missing values section 

# Check for missing values in each column
missing_values <- colSums(is.na(data))
print("Missing values per column:")
print(missing_values)

# Calculate proportion of missing values per column
missing_prop <- missing_values / nrow(data)
print("Proportion of missing values per column:")
print(missing_prop)

# Handle missing values
average_missingness <- mean(missing_prop)
print(paste("Average missingness:", round(average_missingness * 100, 2), "%"))

if (average_missingness < 0.02) {
  data_cleaned <- na.omit(data)
  print("Removed rows with missing values.")
} else {
  data_cleaned <- mice(data, m = 1, maxit = 50, method = 'pmm', seed = 500)
  data_cleaned <- complete(data_cleaned)
  print("Imputed missing values using MICE.")
}


outliers section


# Outlier Detection and Removal (IQR Method)
outliers_iqr <- list()
for (col in names(data_cleaned)) {
  if (is.numeric(data_cleaned[[col]])) {
    Q1 <- quantile(data_cleaned[[col]], 0.25)
    Q3 <- quantile(data_cleaned[[col]], 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    outliers_iqr[[col]] <- which(data_cleaned[[col]] < lower_bound | data_cleaned[[col]] > upper_bound)
  }
}

data_cleaned <- data_cleaned[-unique(unlist(outliers_iqr)), ]
print(paste("Removed", length(unique(unlist(outliers_iqr))), "outliers based on IQR method."))



new
# Outlier Detection and Removal (IQR Method)
outliers_iqr <- list()
for (col in names(data_cleaned)) {
  if (is.numeric(data_cleaned[[col]])) {
    Q1 <- quantile(data_cleaned[[col]], 0.25)
    Q3 <- quantile(data_cleaned[[col]], 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    outliers_iqr[[col]] <- which(data_cleaned[[col]] < lower_bound | data_cleaned[[col]] > upper_bound)
  }
}

# Save the original data for comparison
data_before_removal <- data_cleaned

# Remove outliers from the dataset
data_cleaned <- data_cleaned[-unique(unlist(outliers_iqr)), ]
print(paste("Removed", length(unique(unlist(outliers_iqr))), "outliers based on IQR method."))

# Visualization: Boxplot Before and After Outlier Removal
par(mfrow = c(1, 2))  # Arrange the plots side by side

# Boxplot Before Outlier Removal
boxplot(data_before_removal, main = "Before Outlier Removal", col = "lightblue", outline = TRUE)

# Boxplot After Outlier Removal
boxplot(data_cleaned, main = "After Outlier Removal", col = "lightgreen", outline = TRUE)

# Reset the plot layout
par(mfrow = c(1, 1))  # Reset to default layout




multicorrinearlity 

# Correlation Analysis
cor_matrix <- cor(data_cleaned[sapply(data_cleaned, is.numeric)])
corrplot(cor_matrix, method = "color", tl.cex = 0.7)

# Identify highly correlated pairs (|correlation| > 0.8)
high_corr <- which(abs(cor_matrix) > 0.8 & cor_matrix != 1, arr.ind = TRUE)
print("Highly correlated variable pairs:")
print(high_corr)


investigate variable

# Low Variance Removal
variances <- apply(data_cleaned[sapply(data_cleaned, is.numeric)], 2, var)
low_variance_columns <- names(variances[variances < 0.01])
print("Low variance columns:")
print(low_variance_columns)
data_cleaned <- data_cleaned[, !(names(data_cleaned) %in% low_variance_columns)]
print("Removed low variance columns.")

# Identifier Column Removal
identifier_columns <- names(data_cleaned)[sapply(data_cleaned, function(x) length(unique(x)) == nrow(data_cleaned))]
print("Identifier columns detected:")
print(identifier_columns)
data_cleaned <- data_cleaned[, !(names(data_cleaned) %in% identifier_columns)]
print("Removed identifier columns.")

# Scaling the numerical features
scaled_data <- data_cleaned
scaled_data[sapply(scaled_data, is.numeric)] <- scale(scaled_data[sapply(scaled_data, is.numeric)])

# Summary of the scaled data
summary(scaled_data)



EDA

# Visualization: Distribution of URL Length
ggplot(data, aes(x = UrlLength)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of URL Length", x = "UrlLength", y = "Frequency")

# Visualization: Phishing vs Legitimate URL count
ggplot(data, aes(x = factor(CLASS_LABEL))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Count of Legitimate vs Phishing URLs",
       x = "CLASS_LABEL (0 = Legitimate, 1 = Phishing)",
       y = "Count")











Logistic Regression (LR)

# Logistic Regression Model

# Data Preparation
data_cleaned$CLASS_LABEL <- as.factor(data_cleaned$CLASS_LABEL)

# Data Partitioning (80% training, 20% testing)
set.seed(123)
train_index <- sample(1:nrow(data_cleaned), 0.8 * nrow(data_cleaned))
train_data <- data_cleaned[train_index, ]
test_data <- data_cleaned[-train_index, ]

# Model Training
logistic_model <- glm(CLASS_LABEL ~ ., data = train_data, family = "binomial")

# Model Testing
pred_logistic <- predict(logistic_model, test_data, type = "response")
pred_logistic_class <- ifelse(pred_logistic > 0.5, 1, 0)

# Model Evaluation
conf_matrix_logistic <- table(Predicted = pred_logistic_class, Actual = test_data$CLASS_LABEL)
print(conf_matrix_logistic)

# Model Evaluation metrics
accuracy_logistic <- sum(diag(conf_matrix_logistic)) / sum(conf_matrix_logistic)
print(paste("Accuracy:", accuracy_logistic))

# ROC Curve
library(pROC)
roc_curve <- roc(test_data$CLASS_LABEL, pred_logistic)

plot(roc_curve)


# Load necessary libraries
if (!require("class")) install.packages("class")
if (!require("ggplot2")) install.packages("ggplot2")
library(class)
library(ggplot2)

# Example Data (replace this with your actual dataset)
# For classification, you might have a categorical target (e.g., 'Class')
# For regression, you might have a continuous target (e.g., 'Value')

# Classification Example (binary classification)
set.seed(123)
classification_data <- data.frame(
  feature1 = rnorm(100),
  feature2 = rnorm(100),
  CLASS_LABEL = sample(c("Class1", "Class2"), 100, replace = TRUE)
)

# Split into training and testing data
train_data_class <- classification_data[1:80, ]
test_data_class <- classification_data[81:100, ]

# Data Preparation
train_data_knn_class <- train_data_class[, -which(names(train_data_class) == "CLASS_LABEL")]
test_data_knn_class <- test_data_class[, -which(names(test_data_class) == "CLASS_LABEL")]
train_label_knn_class <- train_data_class$CLASS_LABEL
test_label_knn_class <- test_data_class$CLASS_LABEL

# Model Training for Classification
k_value_class <- 5
knn_model_class <- knn(train = train_data_knn_class, test = test_data_knn_class, cl = train_label_knn_class, k = k_value_class)

# Model Evaluation for Classification
conf_matrix_class <- table(Predicted = knn_model_class, Actual = test_label_knn_class)
print(conf_matrix_class)

accuracy_class <- sum(diag(conf_matrix_class)) / sum(conf_matrix_class)
print(paste("Accuracy for Classification:", accuracy_class))


# Plot Predicted Classes with customized colors
ggplot(test_data_class, aes(x = feature1, y = feature2, color = predicted_class)) + 
  geom_point() + 
  labs(title = "KNN Classification Predicted Classes") +
  scale_color_manual(values = c("red", "blue", "green"))  # Replace with your desired colors



# Regression Example (continuous target)
set.seed(123)
regression_data <- data.frame(
  feature1 = rnorm(100),
  feature2 = rnorm(100),
  target_value = rnorm(100)
)

# Split into training and testing data
train_data_reg <- regression_data[1:80, ]
test_data_reg <- regression_data[81:100, ]

# Data Preparation for Regression
train_data_knn_reg <- train_data_reg[, -which(names(train_data_reg) == "target_value")]
test_data_knn_reg <- test_data_reg[, -which(names(test_data_reg) == "target_value")]
train_label_knn_reg <- train_data_reg$target_value
test_label_knn_reg <- test_data_reg$target_value

# Model Training for Regression
k_value_reg <- 5
knn_model_reg <- knn.reg(train = train_data_knn_reg, test = test_data_knn_reg, y = train_label_knn_reg, k = k_value_reg)


# Model Evaluation for Regression
predictions_reg <- knn_model_reg$pred
mse_reg <- mean((predictions_reg - test_label_knn_reg)^2)  # Mean Squared Error
print(paste("Mean Squared Error for Regression:", mse_reg))

# Plot Predicted vs Actual for Regression
ggplot(test_data_reg, aes(x = target_value, y = predictions_reg)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "KNN Regression: Predicted vs Actual", x = "Actual Values", y = "Predicted Values")




# Install and load required packages
if (!require("e1071")) install.packages("e1071")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ROCR")) install.packages("ROCR")
library(e1071)
library(ggplot2)
library(ROCR)

# Data Preparation --------------------------------------------------------
# Assuming you have pre-loaded 'train_data' and 'test_data' with CLASS_LABEL column

# Remove CLASS_LABEL from features
train_data_nb <- train_data[, -which(names(train_data) == "CLASS_LABEL")]
test_data_nb <- test_data[, -which(names(test_data) == "CLASS_LABEL")]

# Extract labels
train_label_nb <- train_data$CLASS_LABEL
test_label_nb <- test_data$CLASS_LABEL

# Model Training ----------------------------------------------------------
nb_model <- naiveBayes(train_data_nb, train_label_nb)

# Model Testing -----------------------------------------------------------
pred_nb <- predict(nb_model, test_data_nb)

# Basic Evaluation --------------------------------------------------------
# Confusion Matrix
conf_matrix_nb <- table(Predicted = pred_nb, Actual = test_label_nb)
print("Confusion Matrix:")
print(conf_matrix_nb)

# Accuracy Calculation
accuracy_nb <- sum(diag(conf_matrix_nb)) / sum(conf_matrix_nb)
print(paste("Accuracy:", round(accuracy_nb, 4)))

# Confusion Matrix Visualization with Different Theme and Color
conf_matrix_df <- as.data.frame(as.table(conf_matrix_nb))
colnames(conf_matrix_df) <- c("Predicted", "Actual", "Freq")

ggplot(conf_matrix_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "black", size = 0.5) +  # Black borders around tiles
  geom_text(aes(label = Freq), color = "white", size = 6) +  # White text for visibility
  scale_fill_gradient2(low = "yellow", high = "red", mid = "green", midpoint = mean(conf_matrix_df$Freq)) +  # Custom color gradient
  labs(title = "Naïve Bayes Confusion Matrix",
       subtitle = paste("Accuracy: ", round(accuracy_nb, 4)),
       x = "Predicted Class", y = "Actual Class") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# ROC Curve and AUC -------------------------------------------------------
# Get predicted probabilities
pred_prob_nb <- predict(nb_model, test_data_nb, type = "raw")

# Create prediction object (assuming binary classification)
pred_obj <- prediction(pred_prob_nb[,2], test_label_nb)

# Calculate performance metrics
roc_perf <- performance(pred_obj, "tpr", "fpr")
auc_perf <- performance(pred_obj, "auc")

# Plot ROC curve with enhanced styling
plot(roc_perf,
     main = "ROC Curve for Naïve Bayes Model",
     col = "#FF6347",  # Tomato color for line
     lwd = 3,          # Line width
     xlab = "False Positive Rate (FPR)",
     ylab = "True Positive Rate (TPR)",
     font.lab = 2,     # Bold labels
     font.main = 4,    # Italicized title
     cex.lab = 1.2,    # Larger label size
     cex.main = 1.5)   # Larger title size

# Add a shaded area to show AUC area
polygon(c(0, unlist(roc_perf@x.values), 1), 
        c(0, unlist(roc_perf@y.values), 1), 
        col = rgb(1, 0, 0, 0.2), border = NA)  # Shaded AUC area in red

# Add the AUC value to the plot as a legend
legend("bottomright", 
       legend = paste("AUC =", round(auc_perf@y.values[[1]], 3)),
       col = "#FF6347", 
       lty = 1, lwd = 3, 
       cex = 1.2, bg = "white")

# Print AUC value
print(paste("AUC Score:", round(auc_perf@y.values[[1]], 4)))


# Decision Tree Model

# Install and load the required packages
if (!require("rpart")) install.packages("rpart")
if (!require("rpart.plot")) install.packages("rpart.plot")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")

library(rpart)
library(rpart.plot)
library(ggplot2)
library(reshape2)

# Data Preparation (Assuming the dataset is ready)
train_data_dt <- train_data[, -which(names(train_data) == "CLASS_LABEL")]
test_data_dt <- test_data[, -which(names(test_data) == "CLASS_LABEL")]
train_label_dt <- train_data$CLASS_LABEL
test_label_dt <- test_data$CLASS_LABEL

# Model Training
dt_model <- rpart(CLASS_LABEL ~ ., data = train_data, method = "class")

# Model Testing
pred_dt <- predict(dt_model, test_data, type = "class")

# Model Evaluation
conf_matrix_dt <- table(Predicted = pred_dt, Actual = test_label_dt)
print(conf_matrix_dt)

# Model Evaluation metrics
accuracy_dt <- sum(diag(conf_matrix_dt)) / sum(conf_matrix_dt)
print(paste("Accuracy:", accuracy_dt))

# Confusion Matrix Heatmap
conf_matrix_dt_melt <- melt(conf_matrix_dt)
colnames(conf_matrix_dt_melt) <- c("Predicted", "Actual", "Frequency")

ggplot(conf_matrix_dt_melt, aes(x = Predicted, y = Actual, fill = Frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Decision Tree Confusion Matrix Heatmap", x = "Predicted", y = "Actual") +
  theme_minimal()

# Plot Decision Tree
rpart.plot(dt_model, main = "Decision Tree")










# Install and load required packages
if (!require("randomForest")) install.packages("randomForest")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ROCR")) install.packages("ROCR")
library(randomForest)
library(ggplot2)
library(ROCR)

# Data Preparation --------------------------------------------------------
# Assuming you have pre-loaded 'train_data' and 'test_data' with CLASS_LABEL column

# Prepare features and labels
train_label_rf <- train_data$CLASS_LABEL
test_label_rf <- test_data$CLASS_LABEL

# Model Training ----------------------------------------------------------
rf_model <- randomForest(CLASS_LABEL ~ ., 
                         data = train_data,
                         ntree = 100,
                         importance = TRUE)  # Enable importance calculation

# Model Testing -----------------------------------------------------------
pred_rf <- predict(rf_model, test_data)

# Basic Evaluation --------------------------------------------------------
# Confusion Matrix
conf_matrix_rf <- table(Predicted = pred_rf, Actual = test_label_rf)
print("Confusion Matrix:")
print(conf_matrix_rf)

# Accuracy Calculation
accuracy_rf <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf)
print(paste("Accuracy:", round(accuracy_rf, 4)))

# Confusion Matrix Visualization ------------------------------------------
conf_matrix_df <- as.data.frame(as.table(conf_matrix_rf))
colnames(conf_matrix_df) <- c("Predicted", "Actual", "Freq")

ggplot(conf_matrix_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 6) +
  scale_fill_gradient(low = "white", high = "#4daf4a") +
  labs(title = "Random Forest Confusion Matrix",
       x = "Predicted Class",
       y = "Actual Class") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# ROC Curve and AUC -------------------------------------------------------
# Get predicted probabilities
pred_prob_rf <- predict(rf_model, test_data, type = "prob")

# Create prediction object (assuming binary classification)
pred_obj <- prediction(pred_prob_rf[,2], test_label_rf)

# Calculate performance metrics
roc_perf <- performance(pred_obj, "tpr", "fpr")
auc_perf <- performance(pred_obj, "auc")

# Plot ROC curve
plot(roc_perf,
     main = "ROC Curve for Random Forest Model",
     col = "#4daf4a",
     lwd = 2,
     xlab = "False Positive Rate (FPR)",
     ylab = "True Positive Rate (TPR)")
abline(a = 0, b = 1, lty = 2, col = "red")
legend("bottomright", 
       legend = paste("AUC =", round(auc_perf@y.values[[1]], 3)),
       col = "#4daf4a",
       lty = 1,
       lwd = 2,
       cex = 0.8)

# Print AUC value
print(paste("AUC Score:", round(auc_perf@y.values[[1]], 4)))

# Enhanced Variable Importance Plot ---------------------------------------
importance_df <- data.frame(
  Feature = rownames(rf_model$importance),
  Importance = rf_model$importance[, "MeanDecreaseGini"]
) |> 
  arrange(desc(Importance)) |> 
  head(20)  # Show top 20 features

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "#FF6347", alpha = 0.8) +  # Changed color to Tomato Red
  coord_flip() +
  labs(title = "Random Forest - Feature Importance (Gini Index)",
       x = "Features",
       y = "Mean Decrease in Gini Index") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Combine Results

# Load the necessary libraries
library(ggplot2)

# Assuming these accuracy values are predefined
accuracy_logistic <- 0.85  # Example accuracy values
accuracy_knn <- 0.80
accuracy_nb <- 0.75
accuracy_dt <- 0.78
accuracy_rf <- 0.88

# Create data frame with models and accuracy values
results <- data.frame(
  Model = c("Logistic Regression", "KNN", "Naïve Bayes", "Decision Tree", "Random Forest"),
  Accuracy = c(accuracy_logistic, accuracy_knn, accuracy_nb, accuracy_dt, accuracy_rf)
)

# Print the results
print(results)




# Create the bar chart with accuracy percentages
ggplot(accuracy_df, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Accuracy * 100, 1), "%")), 
            vjust = -0.3, size = 5) +  # Positioning the text above the bars
  labs(title = "Model Accuracy Comparison", x = "Model", y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability




































