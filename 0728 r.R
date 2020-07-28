library(tidyverse)
wine <- read.csv(file = 'https://bit.ly/white_wine_quality', sep = ';')
glimpse(x = wine)

wine$quality %>% 
table() %>% 
  prop.table() %>% 
  cumsum() %>% 
  round(digits = 4L) *100

tbl <- table(wine$quality)

bp <- barplot(height = tbl,
              ylim = c(0,2400),
              # col = 'gray70',
              col = c(rep('gray70',4), rep('red',3)),
              xlab = 'Quality Score',
              main = 'White Wine Quality')

text(x = bp, y = tbl, labels = tbl, pos = 3, font = 2)


install.packages('RcolorBrewer')
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(n = 12,name = 'Paired')
myPal1 <- brewer.pal(n = 7,name = 'Paired')
myPal1
display.brewer.pal(n = 11,name = 'PRGn')
myPal3 <- brewer.pal(n = 7,name = 'Paired')

myPal2 <- gray(level = seq(from = 0.1, to = 0.8, length = 7))

myPal4 <- colorRampPalette(colors = c('red', 'purple', 'blue'))(7)


bp <- barplot(height = tbl,
              ylim = c(0,2400),
              col = myPal3,
              xlab = 'Quality Score',
              main = 'White Wine Quality')


wine$grade <- ifelse(test = wine$quality <= 6,
                     yes = 'good',
                     no = 'best')


wine$grade <- as.factor(x = wine$grade)
table(wine$grade, wine$quality)

wine$quality <- NULL 

boxplot(formula = alcohol ~ grade, data = wine)

set.seed(seed = 1234)
n <- nrow(x = wine)
index <- sample(x = n, size = n *0.7, replace = FALSE)
trainSet <- wine %>% slice(index)
testSet <- wine %>% slice(-index)
table(trainSet$grade) %>% prop.table()
table(testSet$grade) %>% prop.table()


install.packages('kknn')
library(kknn)

k <- trainSet %>% nrow() %>% sqrt() %>% ceiling()
print(x = k)

fitN <- kknn(formula = grade~.,
             train = trainSet,
             test = testSet,
             k = k,
             kernel = 'rectangular')

str(object = fitN)

predN <- fitN$fitted.values
real <- testSet$grade

install.packages('caret')
install.packages('e1071')
library(caret)
confusionMatrix(data = predN,reference = real,positive = 'best')

install.packages('MLmetrics')
library(MLmetrics)
F1_Score(y_true = real, y_pred = predN, positive = 'best')

library(pROC)
probN <- fitN$prob[,1]

roc(response = real, predictor = probN) %>% 
  plot()

roc(response = real, predictor = probN)

install.packages('DMwR')
library(DMwR)
trainBal <- SMOTE(form = grade ~.,
                  data = trainSet,
                  perc.over = 200,
                  k = 10,
                  perc.under = 150)

table(trainSet$grade) %>% prop.table()
table(trainBal$grade) %>% prop.table()

levels(x = trainSet$grade)
levels(x = trainBal$grade)

fitB <- kknn(formula = grade~.,
             train = trainBal,
             test = testSet,
             k = k,
             kernel = 'rectangular')

predB <- fitB$fitted.values

confusionMatrix(data = predB, reference = real, positive = 'best')

F1_Score(y_true = real, y_pred = predB, positive = 'best')

probB <- fitB$prob[,1]
roc(response = real, predictor = probB) %>% 
  plot()



fitW <- kknn(formula = grade~.,
             train = trainSet,
             test = testSet,
             k = k,
             kernel = 'triangular')

predW <- fitW$fitted.values
confusionMatrix(data = predW,reference = real,positive = 'best')
F1_Score(y_true = real, y_pred = predW, positive = 'best')

probW <- fitW$prob[,1]
roc(response = real, predictor = probW) %>% 
  plot(col = 'black', lwd= 2, add = TRUE)

auc(response = real, predictor = probW)


fitWB <- kknn(formula = grade~.,
             train = trainBal,
             test = testSet,
             k = k,
             kernel = 'triangular')
predWB <- fitWB$fitted.values
confusionMatrix(data = predWB,reference = real, positive = 'best')
F1_Score(y_true = real, y_pred = predWB, positive = 'best')

probWB <- fitWB$prob[,1]
roc(response = real, predictor = probWB) %>% 
  plot(col = 'orange', lwd = 3, lty = 2, add=TRUE)


auc(response = real, predictor = probWB)
