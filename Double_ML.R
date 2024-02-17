library(causalweight)

load("bankdata.RData")
bankdata <- na.omit(bankdata)
data <- bankdata

set.seed(1)

y <- data[, 1]
d <- data[, 11]
x <- data[, -c(1, 11)]

complete_cases <- complete.cases(y, x, d)
y_complete <- y[complete_cases]
x_complete <- x[complete_cases, ]
d_complete <- d[complete_cases]

dml_model <- treatDML(y = y_complete, d = d_complete, x = x_complete, MLmethod = "randomforest")

estimated_ate <- dml_model$effect
estimated_ate_rounded <- round(estimated_ate, 3)

print(estimated_ate_rounded)

ate_se <- dml_model$se
ate_se_rounded <- round(ate_se, 3)

print(ate_se_rounded)
