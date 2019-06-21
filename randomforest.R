#Load Library
library(tree)
library(ISLR)
library(randomForest)
library(dplyr)

#load dtaset
loan=read.csv("loan_data.csv")
View(loan)
str(loan)


glimpse(loan)


table(loan$not.fully.paid)

#divide data train & test 
set.seed(2)
s=sample(1:nrow(loan),500)
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
(34+1349)/1000

#check predictin train data and calculate error
tree.pred.train=predict(tree.loan,newdata = loan_train,type ="class")
table(tree.pred.train,loan_train$not.fully.paid)
(1+166)/1000

#Puring for optimal modelof tree
set.seed(2)
cv.loan=cv.tree(tree.loan,FUN = prune.misclass)
#plot the tree
plot(cv.loan$size,cv.loan$dev,type="b")



######-----random forest model--------#######

#Build random forest model train data
rf.loan=randomForest(not.fully.paid~.,data = loan_train,do.trace=T)#U CAN USE ARGUMENT do.trace=T
rf.loan

#predecting rando forest model test dataset and fin error
rf.loan.pred=predict(rf.loan,newdata = loan_test)
table(loan_test$not.fully.paid,rf.loan.pred)


#plot variable impPlot(variables which important)
importance(rf.loan)
varImpPlot(rf.loan)
