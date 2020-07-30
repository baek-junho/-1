library(tidyverse)
univ <- read.csv(file = 'http://bit.ly/university_admit')
glimpse(x = univ)
summary(object = univ)
univ[,c(1,4)] <- map_df(.x = univ[, c(1,4)], .f = as.factor)

table(univ$admit) %>% prop.table()

boxplot(formula= gre ~ admit, data = univ)
abline(h = mean(x = univ$gre), col = "red", lty=2)

boxplot(formula= gpa ~ admit, data = univ)
abline(h = mean(x = univ$gpa), col = "red", lty=2)

table(univ$admit, univ$rank) %>% prop.table(margin = 2)

set.seed(seed = 1234)

n <- nrow(x = univ)
index <- sample(x = n, size = n * 0.7, replace = FALSE)
trainSet <- univ %>% slice(index)
testSet <- univ %>% slice(-index)

table(trainSet$admit) %>% prop.table()
table(testSet$admit) %>% prop.table()

fitC <- glm(formula = admit ~. ,
            data = trainSet,
            family = binomial(link = 'logit'))
summary(fitC)

pchisq(q = fitC$null.deviance - fitC$deviance,
       df = fitC$df.null - fitC$df.residual,
       lower.tail = FALSE)

result <- summary(object = fitC)
coefs <- result$coefficients

fitC$coefficients %>% exp()


library(reghelper)
beta(model = fitC)

install.packages('mccr')

probC <- predict(object = fitC, newdata = testSet, type = 'response')
predC <- ifelse(test = probC >= 0.5, yes = 1, no = 0)
predC <- as.factor(x = predC)

real <- testSet$admit

library(caret)
confusionMatrix(data = predC,reference = real, positive = '1')
library(MLmetrics)
F1_Score(y_true = real, y_pred = predC, positive = '1')
library(pROC)
roc(response = real, predictor = probC) %>% plot(main = 'ROC curve', col = 'red')
auc(response = real, predictor = probC)
