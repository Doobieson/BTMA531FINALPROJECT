# Load necessary libraries
library(data.table) # Efficient data manipulation
library(tree)       # For building regression trees

# Set the working directory to where Airbnb_Data.csv is located and load the dataset
setwd("C:/Users/aneeb/Downloads") # Update with your actual path
airbnb_data <- fread("Airbnb_Data.csv")

# Assuming 'log_price' is the target variable and it's already in appropriate format
# Preprocess data: For simplicity, we assume minimal preprocessing is required

# Split data into training and testing sets for model evaluation
set.seed(42) # Ensure reproducibility
indexes <- sample(1:nrow(airbnb_data), size = floor(nrow(airbnb_data) * 0.8))
train_data <- airbnb_data[indexes,]
test_data <- airbnb_data[-indexes,]

# Build the regression tree model
# Here we use a simple formula, but you should adjust it based on your analysis
tree_model <- tree(log_price ~ ., data = train_data)

# Plot the tree to visualize it
plot(tree_model)
text(tree_model, pretty = 0)

# Predict on the test set
predictions <- predict(tree_model, test_data)

# Evaluate the model, for example, using Mean Squared Error (MSE)
mse <- mean((predictions - test_data$log_price)^2)
print(paste("MSE:", mse))

# Optional: Prune the tree if necessary
set.seed(123) # For reproducibility
cv_tree <- cv.tree(tree_model)
pruned_tree <- prune.tree(tree_model, best = cv_tree$size[which.min(cv_tree$dev)])
plot(pruned_tree)
text(pruned_tree, pretty = 0)

# Predict with pruned tree and evaluate again if necessary
pruned_predictions <- predict(pruned_tree, test_data)
pruned_mse <- mean((pruned_predictions - test_data$log_price)^2)
print(paste("Pruned MSE:", pruned_mse))
