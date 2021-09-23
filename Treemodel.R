library(caret)
library(yardstick)
library(ranger)
library(gbm)
library(dplyr)
library(MLmetrics)


#checking for problems with the dataset
summary(cc)
sum(is.na(cc))
table(cc$Class)

cc$Class <- as.factor(cc$Class)
levels(cc$Class)[levels(cc$Class)=="0"] <- "regular"
levels(cc$Class)[levels(cc$Class)=="1"] <- "fraud"


#running traincontrol object
control <- trainControl(
  
  method = "adaptive_cv",
  number = 3,
  adaptive = list(min = 3,
                  alpha = 0.05,
                  method = "gls",
                  complete = FALSE),
  summaryFunction = prSummary,
  classProbs = TRUE,
  verboseIter = TRUE,
  search = "random"
)

#Random Forest Model
rfmodel <- train(
 
  Class ~ .,
  data = cc,
  metric = "PRAUC",
  method = "ranger",
  tuneLength = 20,
  trControl = control,
  
)

pr_curve(rfmodel)
plot(rfmodel,
     metric = "Kappa",
     plotType = "level")


#Gradient Boosting Machine 
gbmmodel <- train(
  
  Class ~ .,
  data = cc,
  metric = "PRAUC",
  method = "gbm",
  tuneLength = 20,
  trControl = control,
  
)

pr_curve(gbmmodel)
plot(gbmmodel,
     metric = "Kappa",
     plotType = "level")


#Final model evaluations
model_list <- list(
  rf = rfmodel,
  gbm = gbmmodel
)

resampled <- resamples(model_list)

summary(resampled)

bwplot(resampled, metric = "PRAUC")

densityplot(resampled, metric = "PRAUC")

xyplot(resampled, metric = "PRAUC")

