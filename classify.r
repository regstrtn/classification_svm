Sys.setenv(https_proxy="http://10.3.100.207:8080")
Sys.setenv(http_proxy="http://10.3.100.207:8080")
setwd("/Users/mohammadluqman/Downloads/sem1/clab1/a9/")
set.seed(7)
library(ggplot2)
library(dplyr)
library(e1071)
library(caret)
library(doMC)
library(pROC)

registerDoMC(cores = 2)
bank <- read.csv("~/Downloads/sem1/clab1/a9/bank-full.csv", sep=";")
bank = bank[,-c(10,11)]

f = y ~ .
f = y ~ duration + previous + pdays + housing + balance + education + job + marital + age + default + loan + campaign + poutcome + contact
f = y ~ duration + age + campaign + previous + balance + poutcome

tuneResult <- tune(svm, f,  data = bank, kernel="linear", ranges=list(cost=c(1, 2, 5, 10, 20),gamma = c(0.1,0.5,1,5,10)))


#Test train split
splitpoint = floor(0.7*nrow(bank))
tr_ind = sample(1:nrow(bank), splitpoint)
btrain = bank[tr_ind,]
btest = bank[-tr_ind,]

#Learn Model with parameters
svmmodel = svm(y~., data = btrain, kernel = "linear",cost = 10)
p = predict(svmmodel, btest)

#Radial Model - Works better than linear
svmradial = svm(f, data = btrain, kernel = "radial", cost = 1)
p = predict(svmradial, btest)

#Evaluate model
op = data.frame(act = btest$y, predicted = p)
eq = length(which(op$act == op$predicted))
noteq = length(which(op$act!=op$predicted))
eq/(noteq+eq)
table(op$act, op$predicted)

##Print correlation matrix
#No correlation observed among numerical vectors

#Get variable importance
control <- trainControl(method="repeatedcv", number=5, repeats=3)
model <- train(y~., data=bank, method="svmRadial", preProcess="scale", trControl=control)
model
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)

#Recursive feature elimination
svmProfile <- rfe(y~., data = bank,
                  sizes = c(2, 3),
                  rfeControl = rfeControl(functions = caretFuncs,
                                          number = 10),
                  ## pass options to train()
                  method = "svmLinear")
