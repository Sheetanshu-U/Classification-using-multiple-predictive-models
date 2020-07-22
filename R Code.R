

#Objective - To build different classification models (bagging, boosting, KNN, Logistic Regression and Naive Bayes to classify the employess into two groups one who use car to commute to office while another group which used other modes)


data.cars = read.csv(file.choose(), header = TRUE)

View(data.cars)

############checking complete cases

sum(complete.cases(data.cars))

colSums(is.na(data.cars))

############as there is one missing value, removing that from the dataset

data.cars2 = data.cars[complete.cases(data.cars), ]

View(data.cars2)


########summary of the dataset

summary(data.cars2)

#########converting categorical variables into factors

data.cars2$Engineer = as.factor(data.cars2$Engineer)
data.cars2$MBA = as.factor(data.cars2$MBA)
data.cars2$license = as.factor(data.cars2$license)

data.cars3 = data.cars2#########creating a duplicate copy

View(data.cars3)

######################Exploratory analysis

library(ggplot2)

geom_boxplot()

ggplot(data.cars2, aes(x = data.cars2$Transport, y = data.cars2$Age))+geom_boxplot()
ggplot(data.cars2, aes(x = data.cars2$Transport, y = data.cars2$Salary))+geom_boxplot()



########checking whether the target variable(Transport) is balanced or not 

prop.table(table(data.cars3$Transport))

########checking the correlation between predictor variables (multicolliniarity check)

cor(data.cars3[, c(1, 5, 6, 7)])###########variables with strong correlation are work experience, age and salary



#############converting the three class target variable into two class

data.cars3$Transport = as.character(data.cars3$Transport)

data.cars3$Transport[data.cars3$Transport=="Public Transport"] = "0"
data.cars3$Transport[data.cars3$Transport=="2Wheeler"] = "0"
data.cars3$Transport[data.cars3$Transport=="Car"] = "1"


data.cars3$Transport = as.factor(data.cars3$Transport)



##################splitting the data into train and test

library(caret)

?createDataPartition

part = createDataPartition(data.cars3$Transport, p = 0.7, list = FALSE)

train.cars3 = data.cars3[part, ]
test.cars3 = data.cars3[-part, ]

###################################logistic regression model


logit = glm(Transport~., data = train.cars34, family=binomial)

logit

##checking overall validity of the model

library(lmtest)

lrtest(logit)

######Since obtained p value <0.05, the model is valid and at least one of the coefficients of "X"is non-zero

summary(logit)

library(car)

vif(logit)

##########removing insignificant variables


train.cars31 = train.cars3[, -3]

train.cars32 = train.cars31[, -c(2, 3)]

train.cars33 = train.cars32[, -5]

train.cars34 = train.cars33[, -2]

View(train.cars34)

View(test.cars3)

test.cars34 = test.cars3[, c(1, 6, 7, 9)]

##checking pseudo R square

library(pscl)

pR2(logit)


##checking p values of variables

summary(logit)

##################predicting using the model

predicted.logit = predict(logit, train.cars34, type = "response")

predicted.logit = floor(predicted.logit+0.5)

predicted.logitest = predict(logit, test.cars34, type = "response")

predicted.logitest = floor(predicted.logitest+0.5)

#########################confusion matrix

predicted.logit = as.factor(predicted.logit)

predicted.logitest = as.factor(predicted.logitest)


confusionMatrix(predicted.logit, train.cars34$Transport, positive = "1")

confusionMatrix(predicted.logitest, test.cars3$Transport, positive = "1")


##########################Naive Bayes Model


library(e1071)

nb=naiveBayes(Transport~., data=train.cars34)

nb

## Predicting on the training and test data

nb_train_pred = predict(nb,train.cars34)

nb_test_pred = predict(nb,test.cars34)

### Evaluate the Naïve Bayes model on the test data.

confusionMatrix(nb_train_pred,train.cars34$Transport, positive = "1")

confusionMatrix(nb_test_pred,test.cars34$Transport, positive = "1")


##########################KNN Model

View(data.cars3)

train.cars3$Gender = as.character(train.cars3$Gender)

train.cars3$Gender[train.cars3$Gender=="Male"]  = 1

train.cars3$Gender[train.cars3$Gender=="Female"]  = 0

test.cars3$Gender = as.character(test.cars3$Gender)

test.cars3$Gender[test.cars3$Gender=="Male"]  = 1

test.cars3$Gender[test.cars3$Gender=="Female"]  = 0


View(train.cars3)
View(test.cars3)

train.cars3[, c(1, 5, 6, 7)] = scale(train.cars3[, c(1, 5, 6, 7)])

test.cars3[, c(1, 5, 6, 7)] = scale(test.cars3[, c(1, 5, 6, 7)])


?kNN

library(class)

KNN = kNN(Transport~., train.cars3, test.cars3[, -9], norm = FALSE, k = 4)

KNN2 = kNN(Transport~., train.cars3, train.cars3[, -9], norm = FALSE, k = 4)


##############confusion matrix using KNN

confusionMatrix(KNN, test.cars3$Transport, positive = "1")

confusionMatrix(KNN2, train.cars3$Transport, positive = "1")



#################################Bagging Model


library(caret) 

?trainControl

train.c = trainControl(method = "repeatedcv", number = 3)
               
modelrf = train(Transport ~., data = train.cars3, method = "rf", 
              trControl = train.c, tuneLength = 5)
              

predrf1 = predict(modelrf, train.cars3)
predrf2 = predict(modelrf, test.cars3)

##################confusion matrix

confusionMatrix(predrf1, train.cars3$Transport, positive = "1")
confusionMatrix(predrf2, test.cars3$Transport, positive = "1")

    

#################################Boosting Model

train.x = trainControl(method = "repeatedcv", number = 3)


model.x = train(Transport ~., data = train.cars3, method = "xgbTree", 
              trControl = train.x, tuneLength = 5)


predrx1 = predict(model.x, train.cars3)
predrx2 = predict(model.x, test.cars3)

##################confusion matrix

confusionMatrix(predrx1, train.cars3$Transport, positive = "1")
confusionMatrix(predrx2, test.cars3$Transport, positive = "1")


