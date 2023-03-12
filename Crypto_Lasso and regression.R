my_packages = c("plyr", "plotly", "ggplot2", "psych", "tidyr", "tidyverse","dplyr","lubridate","readr","caret","caTools","glmnet","randomForest")
#install.packages(my_packages)
lapply(my_packages, require, character.only = T)

crypto_data <- read.csv("~/Documents/Northeastern University/ALY 6070/combinecoin.csv", header = T)

# drop the first and second columns
crypto_data <- crypto_data[, -(1:2)]

cat("Number of Rows before cleanup:", nrow(crypto_data), "\n")
cat("Number of Columns before cleanup:", ncol(crypto_data), "\n")
cat("Blank cells count before cleanup:", sum(!complete.cases(crypto_data)))


crypto_data$Date <- as.Date(crypto_data$Date)

headTail(crypto_data, top = 4, bottom = 4, ellipsis = F)

summary(crypto_data)
str(crypto_data)

class <- crypto_data$Class
predictors <- crypto_data[, -ncol(crypto_data)]

set.seed(123)
train <- sample(nrow(crypto_data), nrow(crypto_data) * 0.8)
crypto_train <- crypto_data[train, ]
crypto_test <- crypto_data[-train, ]

headTail(crypto_train, top = 4, bottom = 4, ellipsis = F)
headTail(crypto_test, top = 4, bottom = 4, ellipsis = F)

#install.packages("glmnet")
library(glmnet)
# Perform Lasso
x_train <- model.matrix(Close ~ ., data = crypto_train)[,-1]
y_train <- crypto_train$Close

# Perform Lasso
x_train <- model.matrix(Close ~ ., data = na.omit(crypto_train))[, -1]
y_train <- na.omit(crypto_train)$Close

x_test <- model.matrix(Close ~ ., data = na.omit(crypto_test))[, -1]
y_test <- na.omit(crypto_test)$Close

fit <- glmnet(x_train, y_train, alpha = 1)
fit

plot(fit, xvar = "lambda", label = TRUE)

cv_fit <- cv.glmnet(x_train, y_train, alpha = 1)
best_lambda <- cv_fit$lambda.min

predictions <- predict(fit, newx = x_test, s = best_lambda)

summary(predictions)

plot(y_test, predictions, pch = 20, cex = 0.7, xlab = "Actual Close Price", ylab = "Predicted Close Price")
abline(a = 0, b = 1, col = "red")

library(randomForest)
# Perform Random Forest
rf <- randomForest(Close ~ ., data = na.omit(crypto_train))


varImpPlot(rf, main = "Variable Importance Plot", type = 2, col = "coral")

## NA
