library(caret)
library(mlbench)
data(Sonar)

set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]

fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 10)

modelfit <- train(Class ~., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = TRUE)

predictions <- predict(modelfit,newdata=testing)
#predprobs <- predict(modelfit,newdata=testing,type="prob")

# this is only valid for classification
#confusionMatrix(data = predictions, testing$Class)

# if you want to test a regression problem,
# uncomment the following line
postResample(pred = predictions, obs = testing$Class)
