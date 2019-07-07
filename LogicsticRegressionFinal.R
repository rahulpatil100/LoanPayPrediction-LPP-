#load libraries
library(reader)
library(mice)
library(caTools)

#load dataset
loans = read.csv(file = "loans.csv")
str(loans)
summary(loans)

#Percentage of loan not fully paid
table(loans$not.fully.paid)
pln <- 1533/(1533+8045)
print(pln)


#missing value
missing <- subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) 
                  | is.na(revol.util) | is.na(inq.last.6mths) 
                  | is.na(delinq.2yrs) | is.na(pub.rec))
str(missing)
nrow(missing)
table(missing$not.fully.paid)


#summary to check missing value
summary(missing)

#impute the missing value
set.seed(144)
vars = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars]))
loans[vars] = imputed


#split the data 70% data use train the model and rest test model
sample = sample.split(loans, SplitRatio = 0.7)
loans_train = subset(loans,sample == TRUE)
loans_test = subset(loans,sample == FALSE)


#create logicstic regression models
model1 = glm(not.fully.paid ~ . , data=loans_train, family = "binomial")
summary(model1)


#Check Accuraccy of prediction model test data
pred=loans_test$predicted_risk = predict(model1, newdata = loans_test)

table(loans_test$not.fully.paid, loans_test$predicted_risk > 0.5)
plot(pred)

#Accuraccy
accuracy <- 2896/(2896+525)
print(accuracy)

#Precison function
precision <- 2893/(2893+5)
print(precision)

#recall
recall <- 2893/(2893+520)
print(recall)

