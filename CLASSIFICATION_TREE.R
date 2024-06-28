
# Load necessary libraries
library(data.table) # For data manipulation and reading CSV files
library(tree)       # For building classification trees

# Set the working directory to where 'Airbnb_Data.csv' is located
# Update the path as per your directory structure
setwd("C:/Users/aneeb/Downloads")

# Load the Airbnb dataset
airbnb_data <- fread("Airbnb_Data.csv")

# Preprocess the data
# Categorizing 'log_price' into 'Low', 'Medium', 'High'
airbnb_data[, price_category := cut(log_price, breaks = quantile(log_price, probs = 0:3 / 3), include.lowest = TRUE, labels = c("Low", "Medium", "High"))]

# Handle missing values for numerical columns by replacing them with the median
num_cols <- c("bathrooms", "bedrooms", "beds")
airbnb_data[ , (num_cols) := lapply(.SD, function(x) fifelse(is.na(x), median(x, na.rm = TRUE), x)), .SDcols = num_cols]

# Convert categorical variables into factors
cat_cols <- c("room_type", "bed_type", "cancellation_policy")
airbnb_data[, (cat_cols) := lapply(.SD, as.factor), .SDcols = cat_cols]

# Split the data into training and testing sets
set.seed(123) # Ensure reproducibility of results
train_indices <- sample(nrow(airbnb_data), nrow(airbnb_data) * 0.8)
train_data <- airbnb_data[train_indices, ]
test_data <- airbnb_data[-train_indices, ]

# Build the classification tree model
# Adjust the formula as needed based on the variables in your dataset
tree_model <- tree(price_category ~ room_type + bed_type + cancellation_policy + bathrooms + bedrooms + beds, data = train_data)

# Plot the tree
plot(tree_model)
text(tree_model, pretty = 0)

# Predict on the test set and evaluate the model's performance
tree_predictions <- predict(tree_model, test_data, type = "class")
accuracy <- mean(tree_predictions == test_data$price_category)
confusion_matrix <- table(Predicted = tree_predictions, Actual = test_data$price_category)

# Print the accuracy and confusion matrix
print(paste("Accuracy:", accuracy))
print(confusion_matrix)

# Optional: Prune the tree if necessary, based on the model's performance
