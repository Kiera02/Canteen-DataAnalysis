# Load required libraries
library(caret)
library(nnet)
library(dplyr)
library(ggplot2)

# Sample data provided (processed data without categories yet)
data <- read.csv("data/processed_data.csv")

# Function to cap outliers using IQR method
cap_outliers_iqr <- function(data) {
  data <- data %>%
    mutate(across(everything(), ~{
      # Calculate IQR for the column
      Q1 <- quantile(., 0.25)
      Q3 <- quantile(., 0.75)
      IQR_value <- Q3 - Q1
      
      # Define lower and upper bounds for outlier detection
      lower_bound <- Q1 - 1.5 * IQR_value
      upper_bound <- Q3 + 1.5 * IQR_value
      
      # Cap the outliers to the lower and upper bounds
      pmin(pmax(., lower_bound), upper_bound)
    }, .names = "capped_{.col}"))
  
  return(data)
}

# Cap outliers using IQR method
data_clean <- cap_outliers_iqr(data)

# Convert 'Satisfied' into categories (multi-class)
data_clean$categories <- as.factor(cut(data_clean$Satisfied, 
                                       breaks = c(0, 2, 4, 6), 
                                       labels = c("Unsatisfied", "Neutral", "Satisfied"), 
                                       include.lowest = TRUE, 
                                       right = TRUE))

# Perform oversampling to balance the categories
# Check category counts
category_counts <- table(data_clean$categories)
cat("Category counts before oversampling:\n")
print(category_counts)

# Identify the majority class and the minority classes
majority_class <- names(which.max(category_counts))
minority_classes <- setdiff(names(category_counts), majority_class)

# Oversample minority classes to match the majority class size
oversampled_data <- data_clean

for (category in minority_classes) {
  # Get the subset of data for the current minority class
  minority_data <- subset(oversampled_data, categories == category)
  
  # Find the number of rows needed to match the majority class size
  required_rows <- nrow(oversampled_data[oversampled_data$categories == majority_class, ]) - nrow(minority_data)
  
  # Randomly sample rows with replacement to achieve the required number of rows
  oversampled_minority_data <- minority_data[sample(nrow(minority_data), size = required_rows, replace = TRUE), ]
  
  # Add the oversampled data back into the dataset
  oversampled_data <- rbind(oversampled_data, oversampled_minority_data)
}

# Check the new category counts after oversampling
cat("\nCategory counts after oversampling:\n")
category_counts <- table(oversampled_data$categories)
print(category_counts)

# Define the control parameters for LOOCV (Leave-One-Out Cross-Validation)
cv_control <- trainControl(method = "LOOCV", classProbs = TRUE, summaryFunction = multiClassSummary)

# Extract independent variables (features) and dependent variable (target)
independent_vars <- oversampled_data[, c("Food.Quality", "Service.Quality", "Affordability", "Hygiene", "Food.Options", "Special.Dietary")]
dependent_var <- oversampled_data$categories

# Combine independent and dependent variables for model training
model_data <- data.frame(categories = dependent_var, independent_vars)

# Step 1: Train the logistic regression model using LOOCV
set.seed(123)

# Train the model using multinomial logistic regression
model <- train(categories ~ Food.Quality + Service.Quality + Affordability + Hygiene + Food.Options + Special.Dietary,
               data = model_data,
               method = "multinom",  # Logistic Regression (multinomial)
               trControl = cv_control,
               tuneLength = 5)

# Check the best tuning results
cat("Best Model from LOOCV:\n")
print(model$bestTune)

# Step 2: Save the best model
saveRDS(model, "results/logistic_regression_model_loocv.rds")
cat("\nModel saved to 'results/logistic_regression_model_loocv.rds'.\n")

# Step 3: Evaluate the model performance
accuracy <- mean(model$pred$pred == model$pred$obs)
cat("\nAccuracy of the model:", accuracy, "\n")

# Step 4: Predict new data (example)
new_data <- data.frame(Food.Quality = 5, Service.Quality = 6, Affordability = 5, Hygiene = 6, Food.Options = 5, Special.Dietary = 5)
predict_new_data <- predict(model, newdata = new_data)
cat("\nPrediction for new data:", predict_new_data, "\n")

# Step 5: Plot confusion matrix for LOOCV
conf_matrix <- confusionMatrix(model$pred$pred, model$pred$obs)
cat("\nConfusion Matrix:\n")
print(conf_matrix)

# Convert confusion matrix to a data frame for plotting
conf_matrix_df <- as.data.frame(as.table(conf_matrix))

names(conf_matrix_df) <- c("Predicted", "Actual", "Frequency")

# Plot the confusion matrix as a heatmap
confusion_matrix_plot <- ggplot(conf_matrix_df, aes(x = Predicted, y = Actual, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), color = "white", size = 5) +
  labs(title = "Confusion Matrix for LOOCV", x = "Predicted", y = "Actual") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal()

# Save the confusion matrix plot
ggsave("results/confusion_matrix_loocv.png", plot = confusion_matrix_plot, width = 8, height = 6)

cat("\nConfusion matrix plot saved as 'confusion_matrix_loocv.png'.\n")