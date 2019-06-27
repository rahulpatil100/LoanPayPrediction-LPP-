#Load Library
library(tree)
library(ISLR)
library(randomForest)
library(dplyr)
library(caret)
library(rpart)


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

#make tree model for train data
tree.loan=tree(as.factor(not.fully.paid)~.,data=loan_train)

#plot the tree
plot(tree.loan) 
text(tree.loan,pretty=0)

#text format 
tree.loan
summary(tree.loan)

#predict for test dataset
tree.pred=predict(tree.loan,newdata = loan_test,type ="class")

#check prediction test data and calculate error
table(tree.pred,loan_test$not.fully.paid)
(1275+642)/500

#check predictin train data and calculate error
tree.pred.train=predict(tree.loan,newdata = loan_train,type ="class")
table(tree.pred.train,loan_train$not.fully.paid)
(63+13)/500

#Puring for optimal modelof tree
set.seed(2)
cv.loan=cv.tree(tree.loan,FUN = prune.misclass)
#plot the tree
plot(cv.loan$size,cv.loan$dev,type="b")



######-----random forest model--------#######
rf.loan=randomForest(as.factor(not.fully.paid)~.,data=loan_train)

#Build random forest model train data
rf.loan=randomForest(not.fully.paid ~ .,data = loan_train,ntree=300,proximity=TRUE,importance=TRUE,do.trace=300)#U CAN USE ARGUMENT do.trace=T

print(rf.loan)
#table(predict(rf.loan),loan_train$not.fully.paid)


#predecting random forest model test dataset and find error
rf.loan.pred=predict(rf.loan,newdata = loan_test,type = "response")
table(loan_test$not.fully.paid,rf.loan.pred>0.5)
accuracy <- (2380+4)/nrow(loan)*0.7
print(accuracy)


#plot variable impPlot(variables which important)
importance(rf.loan)
varImpPlot(rf.loan)



























