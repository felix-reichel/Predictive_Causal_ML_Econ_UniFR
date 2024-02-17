rm(list = ls())

load("bankdata.RData")

str(bankdata)

num_missing <- sum(is.na(bankdata))

bankdata <- na.omit(bankdata)

y <- bankdata[, 1]
x <- bankdata[, -1]

print(num_missing)

library(glmnet)

set.seed(1)

train <- sample.int(n = nrow(bankdata), size = floor(0.6 * nrow(bankdata)), replace = FALSE)

train_data <- bankdata[train, ]
test_data <- bankdata[-train, ]

y_train <- as.numeric(as.character(train_data[, 1]))

x_train <- as.matrix(train_data[, -1])

cv.fit <- cv.glmnet(x = x_train, y = y_train, alpha = 1, nfolds = 5)

print(cv.fit$lambda.min)

lasso_model <- glmnet(x = x_train, y = y_train, alpha = 1, lambda = cv.fit$lambda.min)

coefficients <- coef(lasso_model)

print(coefficients)

y_test <- as.numeric(as.character(test_data[, 1]))

x_test <- as.matrix(test_data[, -1])

predictions <- predict(lasso_model, newx = x_test, s = cv.fit$lambda.min, type = "response")

binary_predictions <- ifelse(predictions >= 0.5, 1, 0)

print(binary_predictions)

predicted_outcome <- round(predictions[1], 3)

print(predicted_outcome)

num_ones <- sum(binary_predictions == 1)

print(num_ones)

total_error_rate <- mean(binary_predictions != y_test)

error_rate_outcome_1 <- mean(binary_predictions[y_test == 1] != y_test[y_test == 1])

error_rate_outcome_0 <- mean(binary_predictions[y_test == 0] != y_test[y_test == 0])

cat("Classification error rate for all observations in the test data:", round(total_error_rate, 3), "\n")
cat("Classification error rate for observations with the outcome being equal to 1:", round(error_rate_outcome_1, 3), "\n")
cat("Classification error rate for observations with the outcome being equal to 0:", round(error_rate_outcome_0, 3), "\n")
