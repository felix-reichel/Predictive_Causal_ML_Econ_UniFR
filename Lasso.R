load(file = "Data_Preprocessings.R")

print(sum(!ok))

library(glmnet)

set.seed(1)
cv.fit <- cv.glmnet(x = x_train, y = y_train, alpha = 1, nfolds = 5)

print(cv.fit$lambda.min)

lasso_model <- glmnet(x = x_train, y = y_train, alpha = 1, lambda = cv.fit$lambda.min)

coefficients <- coef(lasso_model)

print(coefficients)
# contact_cellphone  0.0090308493

predictions <- predict(lasso_model, newx = x_test, type = "response")

predictions[1]
predicted_outcome <- round(predictions[1], 3)
print(predicted_outcome)
predictions

binary_predictions <- ifelse(predictions >= 0.5, 1, 0)
print(binary_predictions)
num_ones <- sum(binary_predictions == 1)

print(num_ones)

total_error_rate <- mean(binary_predictions != y_test)
error_rate_outcome_1 <- mean(binary_predictions[y_test == 1] != y_test[y_test == 1])
error_rate_outcome_0 <- mean(binary_predictions[y_test == 0] != y_test[y_test == 0])

cat("Classification error rate for all observations in the test data:", round(total_error_rate, 3), "\n")
cat("Classification error rate for observations with the outcome being equal to 1:", round(error_rate_outcome_1, 3), "\n")
cat("Classification error rate for observations with the outcome being equal to 0:", round(error_rate_outcome_0, 3), "\n")

