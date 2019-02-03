library(dplyr)
library(randomForest)
library(caret)
library(e1071)

data_train <- read.csv("C:\Users\Saurabh\Box\Vedanta Resources\Sterlite Copper\Roughness_CSV.csv") % > %
select(-1)
data_test <- read.csv("C:\Users\Saurabh\Box\Vedanta Resources\Sterlite Copper\Roughness_CSV.csv") % > %
select(-1)

RandomForest(formula, ntree=n, mtry=FALSE, maxnodes = NULL)
trainControl(method = "cv", number = n, search ="grid")

trControl <- trainControl(method = "cv",
    number = 10,
    search = "grid")

train(formula, df, method = "rf", metric= "Accuracy", trControl = trainControl(), tuneGrid = NULL)

set.seed(678)

Hold <- train(hold~.,
    data = data_train,
    method = "rf",
    metric = "Accuracy",
    trControl = trControl)
print(Hold)

set.seed(678)
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(hold~.,
    data = data_train,
    method = "rf",
    metric = "Accuracy",
    tuneGrid = tuneGrid,
    trControl = trControl,
    importance = TRUE,
    nodesize = 14,
    ntree = 300)
print(rf_mtry)

max(rf_mtry$results$Accuracy)
best_mtry <- rf_mtry$bestTune$mtry

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 15)) {
    set.seed(1234)
    rf_maxnode <- train(hold~.,
        data = data_train,
        method = "rf",
        metric = "Accuracy",
        tuneGrid = tuneGrid,
        trControl = trControl,
        importance = TRUE,
        nodesize = 14,
        maxnodes = maxnodes,
        ntree = 300)
    current_iteration <- toString(maxnodes)
    store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(20: 30)) {
    set.seed(1234)
    rf_maxnode <- train(hold~.,
        data = data_train,
        method = "rf",
        metric = "Accuracy",
        tuneGrid = tuneGrid,
        trControl = trControl,
        importance = TRUE,
        nodesize = 14,
        maxnodes = maxnodes,
        ntree = 300)
    key <- toString(maxnodes)
    store_maxnode[[key]] <- rf_maxnode
}
results_node <- resamples(store_maxnode)
summary(results_node)

store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
    set.seed(5678)
    rf_maxtrees <- train(hold~.,
        data = data_train,
        method = "rf",
        metric = "Accuracy",
        tuneGrid = tuneGrid,
        trControl = trControl,
        importance = TRUE,
        nodesize = 14,
        maxnodes = 24,
        ntree = ntree)
    key <- toString(ntree)
    store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

fit_rf <- train(hold~.,
    data_train,
    method = "rf",
    metric = "Accuracy",
    tuneGrid = tuneGrid,
    trControl = trControl,
    importance = TRUE,
    nodesize = 14,
    ntree = 800,
    maxnodes = 24)

predict(model, newdata= df)
prediction <-predict(fit_rf, data_test)
confusionMatrix(prediction, data_test$hold)
varImpPlot(fit_rf)
