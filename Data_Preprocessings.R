rm(list = ls())
load("bankdata.RData")
str(bankdata)

y <- bankdata[, 1]
x <- bankdata[, -1]
ok <- complete.cases(x, y)
bankdata <- bankdata[ok,]
y <- bankdata[, 1]
x <- bankdata[, -1]

set.seed(1)
train <- sample.int(n = length(y), size = floor(0.6 * length(y)), replace = FALSE)

train_data <- bankdata[train, ]
test_data <- bankdata[-train, ]

y_train <- train_data[, 1]
x_train <- as.matrix(train_data[, -1])

y_test <- test_data[, 1]
x_test <- as.matrix(test_data[, -1])