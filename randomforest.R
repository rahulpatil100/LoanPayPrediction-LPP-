#Load Library
library(ISLR)
library(randomForest)
library(dplyr)
library(caret)

#load dtaset
loan=read.csv("loan_data.csv")
View(loan)
str(loan)


glimpse(loan)


table(loan$not.fully.paid)

#divide data train & test 
set.seed(2)
s=sample(1:nrow(loan),nrow(loan)*0.7)
loan_train=loan[s,]
loan_test=loan[-s,]


######-----random forest model--------#######
rf.loan=randomForest(as.factor(not.fully.paid)~.,data=loan_train)

#Build random forest model train data
rf.loan=randomForest(not.fully.paid ~ .,data = loan_train,ntree=300,proximity=TRUE,importance=TRUE,do.trace=300)#U CAN USE ARGUMENT do.trace=T

print(rf.loan)
#table(predict(rf.loan),loan_train$not.fully.paid)


#predecting random forest model test dataset and find error
rf.loan.pred=predict(rf.loan,newdata = loan_test,type = "response")
table(loan_test$not.fully.paid,rf.loan.pred)
accuracy <- (2413+12)/(2413+7+442+12)
print(accuracy)

#recall
recall <- 2413/(2413+442)
print(recall)

#plot variable impPlot(variables which important)
importance(rf.loan)
varImpPlot(rf.loan)



























