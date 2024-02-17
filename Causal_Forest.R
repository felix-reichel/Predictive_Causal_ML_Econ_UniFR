library(grf)

set.seed(1)

load("bankdata.RData")
bankdata <- na.omit(bankdata)
data <- bankdata

y <- as.numeric(data[, 1])
d <- as.numeric(data[, 2])
x <- as.matrix(data[, -c(1, 2)])

cf_model <- causal_forest(X = x, Y = y, W = d)

predicted_effects <- predict(cf_model, X = x[1, , drop = FALSE])

predicted_effect_first_observation <- round(predicted_effects$predictions, 4)[1]

print(predicted_effect_first_observation)

predicted_effects_all <- predict(cf_model, X = x)$predictions

hist(predicted_effects_all, main = "Distribution of Predicted Causal Effects",
     xlab = "Predicted Causal Effects", ylab = "Frequency", col = "skyblue")

statement_A_expr <- sum(predicted_effects_all > -0.002 & predicted_effects_all < 0.003) > sum(predicted_effects_all <= -0.002 | predicted_effects_all >= 0.003)
statement_B_expr <- all(predicted_effects_all >= -0.01 & predicted_effects_all <= 0.01)
statement_C_expr <- median(predicted_effects_all) > 0
statement_D_expr <- max(predicted_effects_all) > 0.002
statement_E_expr <- min(predicted_effects_all) > -0.002

ate_result <- average_treatment_effect(cf_model)

ate <- ate_result[1]
se <- ate_result[2]

ate_rounded <- round(ate, 4)

print(ate_rounded)

se_rnd <- round(se, 4)
print(se_rnd)
