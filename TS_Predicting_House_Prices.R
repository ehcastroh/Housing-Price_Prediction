###----------------------------------------------------------------------------###
# title: "IEOR 142 Group Project -- EDA of Macroecomic Data and Ames Response"
# author: "Elias Castro Hernandez, Darren Fang, Matthew Clagett, and Zhi Ji"
# date: "November 2017"
# purpose: Perform Exploratory Data Analysis (EDA) on Macroecomic/Ames
###----------------------------------------------------------------------------###

#### Install library dependencies ####
library(dplyr)
library(ggplot2)
library(GGally)
library(caTools)
library(ROCR)
library(MASS)
library(rpart) # CART
library(rpart.plot) # CART Plotting
library(caret) # Cross Validation
library(randomForest) # Random Forrests
library(gbm) # Boosting
library(tidyverse) # Data Cleanup and Manipulation
library(stringr) # String Manipulation
library(glmnet) # Regularization
library(pls) # Least Square Regression
library(leaps) # Subset Prediction for Best Response (Regression)

# Reading and splitting data
ames_response <- read.csv("Letters142.csv")
macro_predict <- read.csv("Macroeconomic Indicators.csv")
#variable.names(l142_raw)
#View(l142_raw)


#### START #### Global Functions ####

# Out of Sample R^2
OSR2 <- function(predictions, test, train, title) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  print(title)
  print("ORS2: ")
  return(r2)
}

# ROC
ROC <- function (pred.model, data.test, model.type) {
  pred <- prediction(pred.model, data.test)
  isB.performance <- performance(pred, "tpr", "fpr")
  return(isB.performance)
}

# AUC
AUC <- function (pred.model, data.test, model.type) {
  pred <- prediction(pred.model, data.test)
  ?print(model.type)
  as.numeric(performance(pred, "auc")@y.values)
}
  
# Confussion Matrix and Evaluative Statistics given a classification threshold
eval <- function(data.test, pred.model, train.data.test, p){
  x <-table(data.test, pred.model > p)
  print("Confusion Matrix")
  print(x)
  print("Accuracy: ")
  print((x[1,1] + x[2,2]) / nrow(train.data.test))   # Accuracy
  print("TPR: ")
  print(x[2,2] / (x[2,1] + x[2,2]))                  # TPR     
  print("FPR: ")
  print(x[1,2] / (x[1,1] + x[1,2]))                  # FPR
}

# Confussion Matrix and Accuracy Statistics for a multiclassification threshold
multiclass_eval <- function(data.test, pred.model, train.data.test){
  x <-table(data.test, pred.model)
  print("Multiclassification Confusion Matrix")
  print(x)
  print("Accuracy: ")
  print((x[1,1] + x[2,2] +x[3,3] + x[4,4]) / nrow(train.data.test))          # Accuracy
}
####  END  #### ---------------- ####



##############################################
###  Exploratory Data Analysis -- Pb 3. a  ###
##############################################
# numerical examination
summary(l142_raw)
# viewing data types of 
for (i in l142_raw){
  print(typeof(i))
}

a_l142 <- l142_raw

# Exploring categorical response variables
a_l142$letter <- as.factor(a_l142$letter)
barplot(table(a_l142$letter), ylab = "Letter Count", beside = TRUE, legend = TRUE)


# exploring variable height and width
letters.height=table(a_l142$height)
letters.width=table(a_l142$width)

# ploting height and width, and height vs width
barplot(letters.width,ylab="Letter Width",col="black") 
barplot(letters.height,ylab="Letter Height",col="red")
plot(a_l142$height, a_l142$width, xlab = "Width of Smallest Box", ylab ="Height of Smallest Box", main = "Box Dimension Plot")

# scatter plot of pairwise relationship
pairs(a_l142)

# Scatter plot and Correlation matrix
ggscatmat(a_l142) # notice strong correlation between xbox, ybox, and width, height, and xbar and ybar



##############################################
###  Predicting if letter = 'B' - Pb 3. b  ###
##############################################
# Reproducibility seed ensures data is split so as to be verifyable  
set.seed(127)

Letters142 <- l142_raw
Letters142$isB = as.factor(Letters142$letter == "B")
# Inspect changes to dataframe
Letters142 # note addition of 'isB' boolean variable

# split and train data
train.ids = sample(nrow(Letters142), 0.65*nrow(Letters142))
Letters142.train = Letters142[train.ids,]
Letters142.test = Letters142[-train.ids,]


#### START 3.b.i ## Examining Baseline model ####
# baseline model confusion matrix for testing set
table(Letters142.test$letter)
table(Letters142.test$isB)

# baseline accuracy for most frequent outcome "not B"
base.accuracy = table(Letters142.test$isB)["FALSE"] / sum(table(Letters142.test$letter))
base.accuracy
####  END  3.b.i ## ------------------------ ####

#### START 3.b.ii ## Logistic Regression Classification on "B" ####
# let Y = 1 if "is B"
# cleaning up data to remove dependent variable
b_l142.train <- subset(Letters142.train, select = -letter)
b_l142.test <- subset(Letters142.test, select = -letter)

# logistic regression model to predict whether or not the letter is a B, using modified training set
isB.ii.lr <- glm(isB ~., data=b_l142.train, family="binomial")
summary(isB.ii.lr)

# Accuracy of logistic regression model on the test set, using a threshold of p = 0.5?
pred.isB.ii <- predict(isB.ii.lr, newdata = b_l142.test, type = "response") 
summary(pred.isB.ii)

# Confusion Matrix, Accuracy, TPR, and FPR, for baseline P>.5
p =.5
print("Evaluative Statistics for Logistic Regression")
eval (b_l142.test$isB, pred.isB.ii, b_l142.test, p)
####  END  3.b.ii ## ----------------------------------------- ####

#### START 3.b.iii ## logistic regression ROC and AUC ####
# ROC 
model.type = " Linear Regression ROC "
# AUC
model.subtype = "with AUC: "
auc <- toString(AUC(pred.isB.ii, b_l142.test$isB, model.type.auc))
title <- paste(model.type, model.subtype, auc, sep=" ")

isB.ii.performance <- ROC(pred.isB.ii, b_l142.test$isB, model.type)
plot(isB.ii.performance, main=title, colorize = TRUE)
abline(0, 1)
####  END  3.b.iii ## ------------------------------- ####

#### START 3.b.iv  ## CART Tree ####
# Selection of tunning parameter CP (average cost complexiting pruning) by leave-k-out cross validation 
b.cpVals = data.frame(cp = seq(0, .1, by=.002))
# CART training  
isB.iii_l142 <- train(isB ~. , data = b_l142.train, 
                      method = "rpart", 
                      tuneGrid = b.cpVals, 
                      trControl = trainControl(method = "cv", number=10), 
                      metric = "Accuracy")

# plot the results of accuracy as a function of cp
ggplot(isB.iii_l142$results, aes(x=cp, y=Accuracy)) + geom_point(size=3) +
  xlab("Complexity Parameter (cp)") + geom_line()

# Extract the best model and make predictions
isB.iii_l142$bestTune
pred.isB.cart = isB.iii_l142$finalModel

# plot CART Tree
prp(pred.isB.cart, digits=3)

# Extract "Model Matrix":
isB.test = as.data.frame(model.matrix(isB~.,+0, data=b_l142.test))
pred.isB.iv = predict(pred.isB.cart, newdata=isB.test, type="class")

# Confusion Matrix, Accuracy, TPR, and FPR, for CART
print("Evaluative Statistics for CART")
nonbench_eval (b_l142.test$isB, pred.isB.iv, b_l142.test)
####  END  3.b.iv   ## --------- ####

#### START 3.b.v ## Ramdom Forest ####
# Simple Random Forest (rf) model
isB.v.rf <- randomForest(isB ~ ., data = b_l142.train)
pred.isB.v <- predict(isB.v.rf, newdata = b_l142.test) 

# RF related plots
plot(isB.v.rf)
varImpPlot(isB.v.rf)

# Confusion Matrix and Accuracy for Random Forest
print("Evaluative Statistics for Random Forest")
nonbench_eval (b_l142.test$isB, pred.isB.v, b_l142.test)
####  END  3.b.v ## -------------- ####

#### START 3.b.vi ## Multimodel Accuracy Comparison #### 
# presented in report
####  END  3.b.vi ## ------------------------------ ####




##############################################
### Predicting Variable 'letter' - Pb 3. c ###
##############################################

#### START 3.c.i ## Multiclass Baseline model ####
c_l142 <- l142_raw
c_l142$letter = as.factor(Letters142$letter)

# Reproducibility
set.seed(127)

# split and train data
train.ids = sample(nrow(c_l142), 0.65*nrow(c_l142))
c_l142.train = c_l142[train.ids,]
c_l142.test = c_l142[-train.ids,]

# baseline model confusion matrix for testing set
table(c_l142.test$letter)
# letter P is most frequent class
table(c_l142.test$letter == "P")

# baseline accuracy for most frequent outcome "P"
c.accuracy <- table(c_l142.test$letter=="P")["TRUE"] / sum(table(c_l142.test$letter))
c.accuracy
####  END  3.c.i ## ------------------------- ####

#### START 3.c.ii ## Multiclass Cross Validation CART Model ####
# Selection of tunning parameter CP (average cost complexiting pruning ) by leave-k-out cross validation 
c.cpVals = data.frame(cp = seq(0, .025, by=.0001))

# CART trainint with Cross Validation
ii.c_l142.cvCart <- train(letter~.,
                    data = c_l142.train,
                    method = "rpart",
                    tuneGrid = c.cpVals,
                    trControl = trainControl(method = "cv", number=10),
                    metric = "Accuracy")

# plot the results of accuracy as a function of cp
ggplot(ii.c_l142.cvCart$results, aes(x=cp, y=Accuracy)) + geom_point(size=3) +
  xlab("Complexity Parameter (cp)") + geom_line()

# Extract the best model and make predictions
ii.c_l142.cvCart$bestTune
c.pred.cart = ii.c_l142.cvCart$finalModel
prp(c.pred.cart, digits=3)

# Extract "Model Matrix":
c.ii.test = as.data.frame(model.matrix(letter~.,+0, data=c_l142.test))
pred.c.ii.cart = predict(c.pred.cart, newdata=c.ii.test, type="class")

# Confusion Matrix, Accuracy, TPR, and FPR, for CART
print("Evaluative Statistics for CART")
multiclass_eval(c_l142.test$letter, pred.c.ii.cart, c_l142.test)
####  END  3.c.ii ## -------------------------------------- ####

#### START 3.c.iii ## Multiclass Random Forest ####
# Simple Random Forest (rf) model
iii.c_l142.rf <- randomForest(letter ~ ., data = c_l142.train)
pred.c.iii.rf <- predict(iii.c_l142.rf, newdata = c_l142.test) 

# Importance by GINI
importance(iii.c_l142.rf)

# RF related plots
plot(margin(iii.c_l142.rf))

# Confusion Matrix, Accuracy, TPR, and FPR, for Random Forest
print("Evaluative Statistics for Random Forest")
multiclass_eval(c_l142.test$letter, pred.c.iii.rf, c_l142.test)
####  END  3.c.iii ## -------------------- ####

#### START 3.c.iv ## Multiclass Cross Validation Random Forest  ####
# train nodel by CV based on 16 predictors 
c.mtryVals = data.frame(mtry = 1:16)
iv.c_l142.cvRF <- train(letter ~ .,
                        data = c_l142.train,
                        tuneGrid = c.mtryVals)

# Explore results of RF
iv.c_l142.cvRF$results
iv.c_l142.cvRF
best.iv.c_l142.cvRF <- iv.c_l142.cvRF$finalModel
pred.best.iv.c_l142.cvRF <- predict(best.iv.c_l142.cvRF, newdata = c.ii.test)

ggplot(iv.c_l142.cvRF$results, aes(x = mtry, y = Accuracy)) + geom_point(size = 3) + 
  ylab("CV Accuracy") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

# Confusion Matrix, and Accuracy, for Cross Validation Random Forest
print("Evaluative Statistics for CV Random Forest")
multiclass_eval(c_l142.test$letter, pred.best.iv.c_l142.cvRF, c_l142.test)
####  END  3.c.iv ## -----------------------------------------  ####

### START 3.c.v ## Multiclass Boosting Regression Trees ###
# Boosting model
v.c_l142.boost <- gbm(letter ~ .,
                      data = c_l142.train,
                      n.trees = 22400,
                      interaction.depth = 10)

# Boosting prediction on 22,400 trees
pred.v.c_l142.boost <- predict(v.c_l142.boost, newdata = c_l142.test, type="response", n.trees=22400)

# Variable importance
summary(pred.v.c_l142.boost)

# Converting predictive boost model to same length and data type as testing data
best.pred.v.c_l142.boost <- apply(pred.v.c_l142.boost, 1, which.max)
best.pred.v.c_l142.boost <- factor(best.pred.v.c_l142.boost, levels = c(1,2,3,4), labels = c("A", "B", "P", "R"))

# Confusion Matrix, and Accurary for Boosting
print("Evaluative Statistics for Boosting Regression Trees")
multiclass_eval(c_l142.test$letter, best.pred.v.c_l142.boost, c_l142.test)
###  END  3.c.v ## ------------------------------------ ###

### START 3.c.vi ## Model Accuracy Comparison for Multiclass Formulations ###
# Presented on report
###  END  3.c.vi ## ----------------------------------------------------- ###

### START 3.c.vii ## Multiclass Logistic Regression ###
# See text page 318 and lecture on the topic
# Pesented on report
###  END  3.c.vii ## ------------------------------ ###

########################################################################
########################################################################
