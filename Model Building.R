rm(list=ls())
setwd('C:/Users/Arathen/Desktop/Github Projects/SF Crime/')

library(DataExplorer)
library(randomForest)
library(tidyverse)
library(caret)

train <- read.csv("CleanedTrain.csv")
test <- read.csv("CleanedTest.csv")
train <- train %>% mutate_at(vars(Category), factor)
train <- train %>% filter(!is.na(Minute))
test <- test %>% filter(!is.na(Minute))

training <- sample_n(train, 250000)

# Random Forest no cv
rf <- randomForest(formula = Category ~ .,     
             data = training)

trained1 <- train(form=Category~.,
                  data=(training),
                  method = "rf",
                  ntree=100,
                  trControl = trainControl(method = "none"),
                  importance = TRUE)

rf.preds <- predict(rf, newdata=test)
head(xgb.preds, 25)

## Fit a Boosted Gradient model
## Baseline model
grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

train_control <- caret::trainControl(
  method = "none",
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

xgb_base <- train(form=Category~.,
                  data=(train),
                  method="xgbTree",
                  trControl=train_control,
                  tuneGrid=grid_default,
                  verbose=TRUE
)

## Next, start tuning hyperparameters
nrounds <- 500
tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1),
  max_depth = c(2, 3, 4, 5),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  
  number = 3, # with n folds 
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

xgb_tune <-train(form=Category~.,
                 data=train,
                 method="xgbTree",
                 trControl=tune_control,
                 tuneGrid=tune_grid,
                 verbose=TRUE
)

# helper function for the plots
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}

tuneplot(xgb_tune)
xgb_tune$bestTune

## Next round of tuning
tune_grid2 <- expand.grid(nrounds = seq(from = 50, to = nrounds, by = 50),
                          eta = xgb_tune$bestTune$eta,
                          max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
                                             c(xgb_tune$bestTune$max_depth:4),
                                             xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
                          gamma = 0,
                          colsample_bytree = 1,
                          min_child_weight = c(1, 2, 3),
                          subsample = 1
)

xgb_tune2 <- caret::train(
  form=imdb_score~.,
  data=(train %>% select(-Set, -movie_title)),
  method="xgbTree",
  trControl=tune_control,
  tuneGrid=tune_grid2,
  verbose=TRUE
)

tuneplot(xgb_tune2)
xgb_tune2$bestTune
min(xgb_tune$results$RMSE)
min(xgb_tune2$results$RMSE)

## Next tuning round
tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)

xgb_tune3 <- caret::train(
  form=imdb_score~.,
  data=(train %>% select(-Set, -movie_title)),
  method="xgbTree",
  trControl=tune_control,
  tuneGrid=tune_grid3,
  verbose=TRUE
)

tuneplot(xgb_tune3, probs = .95)
xgb_tune3$bestTune
min(xgb_tune$results$RMSE)
min(xgb_tune2$results$RMSE)
min(xgb_tune3$results$RMSE)

## Tuning the Gamma
tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune4 <- caret::train(
  form=imdb_score~.,
  data=(train %>% select(-Set, -movie_title)),
  method="xgbTree",
  trControl=tune_control,
  tuneGrid=tune_grid4,
  verbose=TRUE
)

tuneplot(xgb_tune4)
xgb_tune4$bestTune
min(xgb_tune$results$RMSE)
min(xgb_tune2$results$RMSE)
min(xgb_tune3$results$RMSE)
min(xgb_tune4$results$RMSE)

## Reduce learning rate
tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 10000, by = 100),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = xgb_tune4$bestTune$gamma,
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune5 <- caret::train(
  form=imdb_score~.,
  data=(train %>% select(-Set, -movie_title)),
  method="xgbTree",
  trControl=tune_control,
  tuneGrid=tune_grid5,
  verbose=TRUE
)

tuneplot(xgb_tune5)
xgb_tune5$bestTune
min(xgb_tune$results$RMSE)
min(xgb_tune2$results$RMSE)
min(xgb_tune3$results$RMSE)
min(xgb_tune4$results$RMSE)
min(xgb_tune5$results$RMSE)

## Fit the model and predict
final_grid <- expand.grid(
  nrounds = xgb_tune5$bestTune$nrounds,
  eta = xgb_tune5$bestTune$eta,
  max_depth = xgb_tune5$bestTune$max_depth,
  gamma = xgb_tune5$bestTune$gamma,
  colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
  min_child_weight = xgb_tune5$bestTune$min_child_weight,
  subsample = xgb_tune5$bestTune$subsample
)

xgb_model <- caret::train(
  form=imdb_score~.,
  data=(train %>% select(-Set, -movie_title)),
  method="xgbTree",
  trControl=tune_control,
  tuneGrid=final_grid,
  verbose=TRUE
)

xgb.preds <- data.frame(Id=test$movie_title, Predicted=predict(xgb_model, newdata=test))
head(xgb.preds, 25)
write_csv(x=xgb.preds, path="./XGBPredictions.csv")
