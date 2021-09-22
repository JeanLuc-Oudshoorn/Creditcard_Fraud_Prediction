library(caret)
library(yardstick)
library(ranger)
library(gbm)
library(dplyr)


summary(cc)
sum(is.na(cc))
table(cc$Class)

cc$Class <- as.factor(cc$Class)
levels(cc$Class)[levels(cc$Class)=="0"] <- "regular"
levels(cc$Class)[levels(cc$Class)=="1"] <- "fraud"

folds <- createMultiFolds(cc$Class, k = 4, times = 3)


control <- trainControl(
  
  method = "adaptive_cv",
  adaptive = list(min = 3,
                  alpha = 0.05,
                  method = "gls",
                  complete = TRUE),
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE,
  index = folds,
  search = "random"
)


rfmodel <- train(
 
  Class ~ .,
  data = cc,
  metric = "pr_auc",
  method = "ranger",
  tuneLength = 4,
  trControl = control,
  
)

pr_curve(rfmodel)
plot(rfmodel,
     metric = "Kappa",
     plotType = "level")


##GBM model 

gbmmodel <- train(
  
  Class ~ .,
  data = cc,
  metric = "ROC",
  method = "gbm",
  tuneLength = 2,
  trControl = control,
  
)

pr_curve(gbmmodel)
plot(gbmmodel,
     metric = "Kappa",
     plotType = "level")


model_list <- list(
  rf = rfmodel,
  gbm = gbmmodel
)

resampled <- resamples(model_list)

summary(resampled)

bwplot(resampled, metric = "ROC")

densityplot(resampled, metric = "ROC")

xyplot(resampled, metric = "ROC")


