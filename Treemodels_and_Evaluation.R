library(readr)
library(caret)
library(ranger)
library(gbm)
library(yardstick)
library(plyr)
library(dplyr)
library(tictoc)
library(MLmetrics)


#loading and checking for problems with the dataset
cc <- read_csv("creditcard.csv")

summary(cc)
str(cc)
sum(is.na(cc))
table(cc$Class)

cc$Class <- as.factor(cc$Class)
levels(cc$Class)[levels(cc$Class)=="0"] <- "regular"
levels(cc$Class)[levels(cc$Class)=="1"] <- "fraud"


#creating traincontrol object
control <- trainControl(
  
  method = "adaptive_cv",
  number = 4,
  adaptive = list(min = 3,
                  alpha = 0.05,
                  method = "gls",
                  complete = FALSE),
  summaryFunction = prSummary,
  classProbs = TRUE,
  verboseIter = TRUE,
  search = "random"
)

tic()
#Random Forest Model
rfmodel <- train(
 
  Class ~ .,
  data = cc,
  metric = "PRAUC",
  method = "ranger",
  tuneLength = 20,
  trControl = control,
  
)
toc()


#Gradient Boosting Machine 
tic()

gbmmodel <- train(
  
  Class ~ .,
  data = cc,
  metric = "PRAUC",
  method = "gbm",
  tuneLength = 20,
  trControl = control,
  
)

toc()


#Resample model metrics
model_list <- list(
  rf = rfmodel,
  gbm = gbmmodel
)

resampled <- resamples(model_list)

summary(resampled)

bwplot(resampled, metric = "F")

densityplot(resampled, metric = "F")

xyplot(resampled, metric = "F")


#Creating prediction recall plots
resultrf <- data.frame(truth = cc$Class, Fraud = rfmodel$finalModel$predictions[,2], Regular = rfmodel$finalModel$predictions[,1],
                       predicted = predict(object = rfmodel, newdata = cc))

predgbm <- predict(object = gbmmodel, newdata = cc, type = "prob")

resultgbm <- data.frame(truth = cc$Class, Fraud = predgbm$fraud, Regular = predgbm$regular, 
                        predicted = predict(object = gbmmodel, newdata = cc))


pr_curve(resultrf, truth = truth, Regular) %>%
  ggplot(aes(x = recall, y = precision)) +
  geom_path(color = "darkred") +
  geom_path(data = pr_curve(resultgbm, truth = truth, Regular), aes(x = recall, y = precision), color = "blue") +
  ylim(0.998, 1) +
  xlim(0, 1) 

pr_curve(resultrf, truth = truth, Regular) %>%
  ggplot(aes(x = recall, y = precision)) +
  geom_path(color = "darkred") +
  geom_path(data = pr_curve(resultgbm, truth = truth, Regular), aes(x = recall, y = precision), color = "blue") +
  ylim(0.998, 1) +
  xlim(0.9998, 1) 
