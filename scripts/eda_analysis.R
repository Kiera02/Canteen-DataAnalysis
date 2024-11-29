# Load required libraries
library(ggplot2)
library(dplyr)
library(corrplot)

# Load preprocessed data
respond_clean <- read.csv("data/processed_data.csv")

# Step 1: Summary statistics
cat("Summary of the dataset:\n")
print(summary(respond_clean))

# Step 2: Distribution of target variable ('categories')
respond_clean$categories <- as.factor(cut(respond_clean$Satisfied, 
                                           breaks = c(0, 2, 4, 6), 
                                           labels = c("Unsatisfied", "Neutral", "Satisfied"), 
                                           include.lowest = TRUE, 
                                           right = TRUE))

cat("\nDistribution of 'categories':\n")
category_counts <- table(respond_clean$categories)
print(category_counts)

# Plot category distribution
category_plot <- ggplot(respond_clean, aes(x = categories, fill = categories)) +
  geom_bar() +
  labs(title = "Distribution of Satisfaction Categories", x = "Category", y = "Count") +
  theme_minimal()

# Save category distribution plot
ggsave("results/eda/category_distribution.png", plot = category_plot)

# Step 3: Boxplots for each feature by 'categories'
numerical_cols <- c("Food.Quality", "Service.Quality", "Affordability", "Hygiene", "Food.Options", "Special.Dietary")
for (col in numerical_cols) {
  p <- ggplot(respond_clean, aes_string(x = "categories", y = col, fill = "categories")) +
    geom_boxplot() +
    labs(
      title = paste("Boxplot of", col, "by Categories"),
      x = "Category",
      y = col
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Display the plot
  print(p)
  
  # Save the individual boxplot
  ggsave(paste0("results/eda/", col, "_by_category.png"), plot = p)
}

# Step 4: Correlation matrix heatmap
num_data <- respond_clean %>%
  select(all_of(numerical_cols))

corr_matrix <- cor(num_data, use = "complete.obs")

# Save Correlation Heatmap as PNG
png("results/eda/correlation_heatmap.png", width = 800, height = 800)  # Set the image size if needed
corrplot(corr_matrix, method = "color", addCoef.col = "black", title = "Correlation Heatmap")
dev.off()  # Close the png device to save the plot

# Final message
cat("EDA completed. Plots saved to 'results/eda/' directory.")
