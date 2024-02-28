load(file = "Data_Preprocessings.R")

library(rpart)

set.seed(1)
tree_model <- rpart(y_train ~ ., data = data.frame(x_train), method = "class")
rpart.plot::rpart.plot(tree_model)

test_predictions <- predict(tree_model, newdata = test_data, type = "class")

test_predictions

test_predictions_recode <- ifelse(test_predictions == 1, 1, 0)

classification_error_rate <- mean(test_predictions != test_data$outcome)

sum(test_predictions == 1)

cat("Classification Error Rate:", round(classification_error_rate, 3), "\n")

term_deposit_count <- sum(test_predictions_recode == 1)
cat("Number of individuals predicted to open a term deposit:", term_deposit_count, "\n")

library(SuperLearner)
library(randomForest)
library(glmnet)
library(e1071)

base_learners <- c("SL.randomForest", "SL.glmnet", "SL.ksvm")

cvControl<-SuperLearner.CV.control(V=5)
control <- SuperLearner.control()

set.seed(1)
superlearner_model <- SuperLearner(Y = train_data$outcome, 
                                   X = train_data[, -which(names(train_data) == "outcome")], 
                                   SL.library = base_learners, 
                                   method = "method.NNLS", family=binomial,
                                   control = control, verbose = TRUE, cvControl = cvControl)

superlearner_model

test_predictions <- predict(superlearner_model, newdata = test_data[, -which(names(test_data) == "outcome")])

test_predictions$pred[1]

predicted_prob_first_obs <- test_predictions$pred[1]

test_predictions_recode <- ifelse(test_predictions$pred > 0.5, 1, 0)

classification_error_rate <- mean(test_predictions_recode != test_data$outcome)

test_predictions
term_deposit_count <- sum(test_predictions$pred > 0.5)

cat("Predicted outcome for the first observation:", round(predicted_prob_first_obs, 3), "\n")
cat("Number of individuals predicted to open a term deposit:", term_deposit_count, "\n")
cat("Classification error rate:", round(classification_error_rate, 3), "\n")

