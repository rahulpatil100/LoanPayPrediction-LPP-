#load libraries
library(mice)
library(caTools)

#load dataset
loans = read.csv(file = "loan_data.csv")
str(loans)

#Check loan not paid
table(loans$not.fully.paid)
unpaid <-(8045/9578)
print(unpaid)

#summary to check missing value
summary(loans)

#impute the missing value
set.seed(144)
vars = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars]))
loans[vars] = imputed


#split the data 70% data use train the model and rest test model
sample = sample.split(loans, SplitRatio = 0.7)
loans_train = subset(loans,sample == TRUE)
loans_test = subset(loans,sample == FALSE)


#create model
model1 = glm(not.fully.paid ~ . , data=loans_train, family = "binomial")
summary(model1)
plot(not.fully.paid, data=mtcars, col="red4")
lines(not.fully.paid~, newdat, col="green4", lwd=2)

#Check Accuraccy of prediction model test data
loans_test$predicted_risk = predict(model1, newdata = loans_test)

table(loans_test$not.fully.paid, loans_test$predicted_risk > 0.5)

#Accuraccy
accuracy <- 2912/(2912+509)
print(accuracy)
