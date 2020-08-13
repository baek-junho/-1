library(tidyverse)





getwd()
setwd(dir = './data')
list.files(pattern = 'RDA')
list.files(pattern = "RDA")
load(file = "Bank_DataSet.RDA")
parallel::detectCores()

install.packages("gbm")
library(gbm)
set.seed(seed = 1234)
fit1 <- gbm(formula = PersonalLoan ~., data = trainSet, distribution = 'multinomial', n.trees = 5000, interaction.depth = 3,
            shrinkage = 0.01, n.minobsinnode = 10, bag.fraction = 0.5, cv.folds = 5, n.cores = 7)

print(x = fit1)

fitRF <- readRDS(file = 'RFC.RDS')
library(randomForest)
pred2 <- predict(object = fitRF, newdata = testSet,type = 'response')
real <- testSet$PersonalLoan

library(caret)
confusionMatrix(data = pred2, reference = real, positive = '1')

library(MLmetrics)
F1_Score(y_true = real, y_pred = pred2, positive = '1')

prob2 <- predict(object = fitRF, newdata = testSet, type = 'vote')
prob2 <- prob2[, 2]

library(pROC)
roc(response = real, predictor = prob2) %>% 
  plot(add= TRUE, col = 'blue', lwd= 2)

auc(response = real, predictor = prob2)

grid <- expand.grid(depth = c(1,3,5), learn = c(0.01, 0.05, 0.10), min = c(5,7,10), bag = c(0.5, 0.8, 1.0), verr = NA, tree = NA)

for( i in 1:nrow(x = grid)){
 set.seed(seed = 1234)
  fit <- gbm(formula = PersonalLoan ~ ., data = trainSet, distribution = 'multinomial', n.trees = 5000, interaction.depth = grid$depth[i],
             shrinkage = grid$learn[i], n.minobsinnode = grid$min[i], bag.fraction = grid$bag[i], train.fraction = 0.75)
  grid$verr[i] <- min(x = fit$valid.error)
  grid$tree[i] <- which.min(x = fit$valid.error)
}
