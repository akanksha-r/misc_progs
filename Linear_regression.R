
## Load the data
data = read.csv("../akanksha/Documents/Test_data.csv")

# Perform linear regression
model <- lm(outcome2 ~ sex+heart2+agedx+age_allevts, data = data)

# Extract p-values of the coefficients
p_values <- summary(model)$coefficients[, "Pr(>|t|)"]

# Select variables with p-values less than 0.05
significant_vars <- names(p_values[p_values < 0.05])

# Print list of variables with p-values less than 0.05
cat("Variables with P-value < 0.05:\n")
cat(significant_vars, sep = ", ")
cat("\n\n")

# Update the model with only significant variables
model <- lm(outcome2 ~ ., data = data[, c(significant_vars, "outcome")])

# Make predictions on the entire dataset
predicted_outcome <- predict(model)

# Evaluate model performance (optional for continuous outcome)
# For linear regression, evaluation metrics are not typically used as the outcome is continuous

# Print R-squared value
cat("R-squared value:\n")
cat(summary(model)$r.squared)
cat("\n")
