library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(dplyr)

set.seed(678)
path <- 'C:\Users\Saurabh\Box\Vedanta Resources\Sterlite Copper\Top_Nodules_CSV.csv'
topnodules <-read.csv(path)

create_train_test(df, size = 0.8, train = TRUE)
create_train_test & lt;- function(data, size = 0.8, train = TRUE) {
    n_row = nrow(data)
    total_row = size * n_row
    train_sample & lt; - 1: total_row
    if (train == TRUE) {
        return (data[train_sample, ])
    } else {
        return (data[-train_sample, ])
    }
}

rpart(formula, data=, method='')
Hold <- rpart(hold~., data = data_train, method = 'class')
rpart.plot(Hold, extra = 106)

predict(fitted_model, df, type = 'class')

predict_hold <-predict(Hold, data_test, type = 'class')

table_mat <- table(data_test$hold, predict_hold)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

print(paste('Accuracy for test', accuracy_Test))

rpart.control(minsplit = 20, minbucket = round(minsplit/3), maxdepth = 30

accuracy_tune <- function(Hold) {
    predict_hold <- predict(Hold, data_test, type = 'class')
    table_mat <- table(data_test$hold, predict_hold)
    accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
    accuracy_Test
}

control <- rpart.control(minsplit = 4,
    minbucket = round(5 / 3),
    maxdepth = 3,
    cp = 0)
tune_fit <- rpart(Hold~., data = data_train, method = 'class', control = control)
accuracy_tune(tune_fit)
