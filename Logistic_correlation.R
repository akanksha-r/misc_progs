## Load the library
library(pROC)

## Load the data
data = read.csv("../akanksha/Documents/Test_data.csv")

# Perform logistic regression
model <- glm(outcome ~ sex+heart2+agedx+age_allevts, data = data, family = binomial)

# Extract p-values of the coefficients
p_values <- summary(model)$coefficients[, 4]

# Select variables with p-values less than 0.05
significant_vars <- names(p_values[p_values < 0.05])


# Print list of variables with p-values less than 0.05
cat("Variables with P-value < 0.05:\n")
cat(significant_vars, sep = ", ")
cat("\n\n")

# Update the model with only significant variables
model <- glm(outcome ~ ., data = data[, c(significant_vars, "outcome")], family = binomial)

# Make predictions on the entire dataset
predicted_probs <- predict(model, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Evaluate model performance
roc <- roc(data$outcome, predicted_probs)
cat("Receiver Operating Characteristic (ROC) Curve:\n")
plot(roc, main = "Receiver Operating Characteristic (ROC) Curve")
