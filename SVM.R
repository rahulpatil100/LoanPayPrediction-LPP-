#Load the Library
library(ggplot2)
library(caTools)
library(reader)
library(rmarkdown)
library(e1071)

#Load the dataset
loans <- read.csv("loan_data.csv")

#summary of loan data
summary(loans)

#string data
str(loans)

#Consolidate coulumns into factor
loans$credit.policy <- as.factor(loans$credit.policy)
loans$inq.last.6mths<- as.factor(loans$inq.last.6mths)
loans$delinq.2yrs   <- as.factor(loans$delinq.2yrs)
loans$pub.rec       <- as.factor(loans$pub.rec)
loans$not.fully.paid <- as.factor(loans$not.fully.paid)

#plot the graph data affect not fully paid column
ggplot(loans,aes(fico)) + geom_histogram(aes(fill=not.fully.paid),color='black')+theme_bw()


#type of loan 
ggplot(loans,aes(factor(purpose)))+ geom_bar(aes(fill=not.fully.paid),postion='dodge') +
  theme(axis.text.x= element_text(angle = 90,hjust = 1))


#fico vs invest rate
ggplot(loans,aes(int.rate,fico)) + geom_point(aes(color=not.fully.paid),alpha=0.5)+theme_bw()


#Split data into train and test 
set.seed(101)
sample <- sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train <- subset(loans, sample == TRUE)
test <- subset(loans, sample == FALSE) 

# train the model
model <- svm(not.fully.paid~., data=train[1:14])
summary(model)


# prediction
predict.values <- predict(model,test[1:13])
table(predict.values, test$not.fully.paid)

#Tunning the cost and gamma values
tunned.svm <- svm(not.fully.paid~., data = train[1:14], kernel='radial', cost=70, gamma=0.2)
predicted.values <- predict(tunned.svm, test[1:13])
table(predicted.values, test$not.fully.paid)

# Accuracy checking
accuracy <- (2133 + 201)/(2133 + 358 + 280 + 102)
print(accuracy)


# Recall  fraction of the document that relevent and succesfully retrived
recall <- 2133/(2133+280)
print(recall)





