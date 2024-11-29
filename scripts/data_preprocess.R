# Load required libraries
library(dplyr)

# Load raw data
respond <- read.csv("data/raw_data.csv")

# Step 1: Get data summary
cat("Data Summary:\n")
print(summary(respond))  # Summary statistics for numerical data
cat("\nStructure of data:\n")
print(str(respond))  # Structure of the dataset (data types, number of records)

# Step 2: Remove 'Score' and 'Timestamp' columns
respond_clean <- respond %>%
  select(-Score, -Timestamp)

# Step 3: Save the cleaned data (after removing 'Score' and 'Timestamp')
write.csv(respond_clean, "data/processed_data.csv", row.names = FALSE)

cat("\nData after removing 'Score' and 'Timestamp' saved to 'data/processed_data.csv'")
