### Install necessary packages and libraries
install.packages("tidyverse")
install.packages("stringr")
install.packages("glmnet")
install.packages("pls")
install.packages("randomForest")
install.packages("caret")
install.packages("leaps")
install.packages("lme4")
install.packages("h2o")
library(tidyverse)
library(stringr)
library(glmnet)
library(pls)
library(randomForest)
library(caret)
library(leaps)
library(lme4)
library(h2o)
## Load the data
getwd()
setwd("/Users/Darren/Desktop")

ames <- read.csv("Ames.csv")
amesdata <- read.csv("Ames_Macro_Monthly_Predictors.csv")

## Appending Avg Sale Price to Amesdata

amesnew <- ames[ -c(-76,-77,-80)]
amesnew1 <- amesnew[amesnew$MoSold == 1,]
amesnew12006 <- amesnew1[amesnew1$YrSold == 2006,]
avg12006 <- sum(amesnew12006$SalePrice)/18

amesnew2 <- amesnew[amesnew$MoSold == 2,]
amesnew22006 <- amesnew2[amesnew2$YrSold == 2006,]
avg22006 <- sum(amesnew22006$SalePrice)/24

amesnew3 <- amesnew[amesnew$MoSold == 3,]
amesnew32006 <- amesnew3[amesnew3$YrSold == 2006,]
avg32006 <- sum(amesnew32006$SalePrice)/51

amesnew4 <- amesnew[amesnew$MoSold == 4,]
amesnew42006 <- amesnew4[amesnew4$YrSold == 2006,]
avg42006 <- sum(amesnew42006$SalePrice)/48

amesnew5 <- amesnew[amesnew$MoSold == 5,]
amesnew52006 <- amesnew5[amesnew5$YrSold == 2006,]
avg52006 <- sum(amesnew52006$SalePrice)/75

amesnew6 <- amesnew[amesnew$MoSold == 6,]
amesnew62006 <- amesnew6[amesnew6$YrSold == 2006,]
avg62006 <- sum(amesnew62006$SalePrice)/97

amesnew7<- amesnew[amesnew$MoSold == 7,]
amesnew72006 <- amesnew7[amesnew7$YrSold == 2006,]
avg72006 <- sum(amesnew72006$SalePrice)/122

amesnew8 <- amesnew[amesnew$MoSold == 8,]
amesnew82006 <- amesnew8[amesnew8$YrSold == 2006,]
avg82006 <- sum(amesnew82006$SalePrice)/45

amesnew9 <- amesnew[amesnew$MoSold == 9,]
amesnew92006 <- amesnew9[amesnew9$YrSold == 2006,]
avg92006 <- sum(amesnew92006$SalePrice)/41

amesnew10 <- amesnew[amesnew$MoSold == 10,]
amesnew102006 <- amesnew10[amesnew10$YrSold == 2006,]
avg102006 <- sum(amesnew102006$SalePrice)/49

amesnew11 <- amesnew[amesnew$MoSold == 11,]
amesnew112006 <- amesnew11[amesnew11$YrSold == 2006,]
avg112006 <- sum(amesnew112006$SalePrice)/31

amesnew12 <- amesnew[amesnew$MoSold == 12,]
amesnew122006 <- amesnew12[amesnew12$YrSold == 2006,]
avg122006 <- sum(amesnew122006$SalePrice)/24

amesnew12007 <- amesnew1[amesnew1$YrSold == 2007,]
avg12007 <- sum(amesnew12007$SalePrice)/31

amesnew22007 <- amesnew2[amesnew2$YrSold == 2007,]
avg22007 <- sum(amesnew22007$SalePrice)/28

amesnew32007 <- amesnew3[amesnew3$YrSold == 2007,]
avg32007 <- sum(amesnew32007$SalePrice)/53

amesnew42007 <- amesnew4[amesnew4$YrSold == 2007,]
avg42007 <- sum(amesnew42007$SalePrice)/50

amesnew52007 <- amesnew5[amesnew5$YrSold == 2007,]
avg52007 <- sum(amesnew52007$SalePrice)/89

amesnew62007 <- amesnew6[amesnew6$YrSold == 2007,]
avg62007 <- sum(amesnew62007$SalePrice)/106

amesnew72007 <- amesnew7[amesnew7$YrSold == 2007,]
avg72007 <- sum(amesnew72007$SalePrice)/108

amesnew82007 <- amesnew8[amesnew8$YrSold == 2007,]
avg82007 <- sum(amesnew82007$SalePrice)/75

amesnew92007 <- amesnew9[amesnew9$YrSold == 2007,]
avg92007 <- sum(amesnew92007$SalePrice)/40

amesnew102007 <- amesnew10[amesnew10$YrSold == 2007,]
avg102007 <- sum(amesnew102007$SalePrice)/41

amesnew112007 <- amesnew11[amesnew11$YrSold == 2007,]
avg112007 <- sum(amesnew112007$SalePrice)/41

amesnew122007 <- amesnew12[amesnew12$YrSold == 2007,]
avg122007 <- sum(amesnew122007$SalePrice)/32

amesnew12008 <- amesnew1[amesnew1$YrSold == 2008,]
avg12008 <- sum(amesnew12008$SalePrice)/29

amesnew22008 <- amesnew2[amesnew2$YrSold == 2008,]
avg22008 <- sum(amesnew22008$SalePrice)/28

amesnew32008 <- amesnew3[amesnew3$YrSold == 2008,]
avg32008 <- sum(amesnew32008$SalePrice)/36

amesnew42008 <- amesnew4[amesnew4$YrSold == 2008,]
avg42008 <- sum(amesnew42008$SalePrice)/61

amesnew52008 <- amesnew5[amesnew5$YrSold == 2008,]
avg52008 <- sum(amesnew52008$SalePrice)/83

amesnew62008 <- amesnew6[amesnew6$YrSold == 2008,]
avg62008 <- sum(amesnew62008$SalePrice)/108

amesnew72008 <- amesnew7[amesnew7$YrSold == 2008,]
avg72008 <- sum(amesnew72008$SalePrice)/101

amesnew82008 <- amesnew8[amesnew8$YrSold == 2008,]
avg82008 <- sum(amesnew82008$SalePrice)/51

amesnew92008 <- amesnew9[amesnew9$YrSold == 2008,]
avg92008 <- sum(amesnew92008$SalePrice)/35

amesnew102008 <- amesnew10[amesnew10$YrSold == 2008,]
avg102008 <- sum(amesnew102008$SalePrice)/30

amesnew112008 <- amesnew11[amesnew11$YrSold == 2008,]
avg112008 <- sum(amesnew112008$SalePrice)/34

amesnew122008 <- amesnew12[amesnew12$YrSold == 2008,]
avg122008 <- sum(amesnew122008$SalePrice)/27

amesnew12009 <- amesnew1[amesnew1$YrSold == 2009,]
avg12009 <- sum(amesnew12009$SalePrice)/19

amesnew22009 <- amesnew2[amesnew2$YrSold == 2009,]
avg22009 <- sum(amesnew22009$SalePrice)/27

amesnew32009 <- amesnew3[amesnew3$YrSold == 2009,]
avg32009 <- sum(amesnew32009$SalePrice)/45

amesnew42009 <- amesnew4[amesnew4$YrSold == 2009,]
avg42009 <- sum(amesnew42009$SalePrice)/48

amesnew52009 <- amesnew5[amesnew5$YrSold == 2009,]
avg52009 <- sum(amesnew52009$SalePrice)/69

amesnew62009 <- amesnew6[amesnew6$YrSold == 2009,]
avg62009 <- sum(amesnew62009$SalePrice)/112

amesnew72009 <- amesnew7[amesnew7$YrSold == 2009,]
avg72009 <- sum(amesnew72009$SalePrice)/110

amesnew82009 <- amesnew8[amesnew8$YrSold == 2009,]
avg82009 <- sum(amesnew82009$SalePrice)/62

amesnew92009 <- amesnew9[amesnew9$YrSold == 2009,]
avg92009 <- sum(amesnew92009$SalePrice)/45

amesnew102009 <- amesnew10[amesnew10$YrSold == 2009,]
avg102009 <- sum(amesnew102009$SalePrice)/53

amesnew112009 <- amesnew11[amesnew11$YrSold == 2009,]
avg112009 <- sum(amesnew112009$SalePrice)/37

amesnew122009 <- amesnew12[amesnew12$YrSold == 2009,]
avg122009 <- sum(amesnew122009$SalePrice)/21

amesnew12010 <- amesnew1[amesnew1$YrSold == 2010,]
avg12010 <- sum(amesnew12010$SalePrice)/26

amesnew22010 <- amesnew2[amesnew2$YrSold == 2010,]
avg22010 <- sum(amesnew22010$SalePrice)/26

amesnew32010 <- amesnew3[amesnew3$YrSold == 2010,]
avg32010 <- sum(amesnew32010$SalePrice)/48

amesnew42010 <- amesnew4[amesnew4$YrSold == 2010,]
avg42010 <- sum(amesnew12010$SalePrice)/72

amesnew52010 <- amesnew5[amesnew5$YrSold == 2010,]
avg52010 <- sum(amesnew52010$SalePrice)/79

amesnew62010 <- amesnew6[amesnew6$YrSold == 2010,]
avg62010 <- sum(amesnew62010$SalePrice)/82

amesnew72010 <- amesnew7[amesnew7$YrSold == 2010,]
avg72010 <- sum(amesnew72010$SalePrice)/8

amesnew82010 <- amesnew8[amesnew8$YrSold == 2010,]
avg82010 <- 199754.563

df <- data.frame(matrix(ncol = 1, nrow = 56))
df[1,1] <- avg12006
df[2,1] <- avg22006
df[3,1] <- avg32006
df[4,1] <- avg42006
df[5,1] <- avg52006
df[6,1] <- avg62006
df[7,1] <- avg72006
df[8,1] <- avg82006
df[9,1] <- avg92006
df[10,1] <- avg102006
df[11,1] <- avg112006
df[12,1] <- avg122006
df[13,1] <- avg12007
df[14,1] <- avg22007
df[15,1] <- avg32007
df[16,1] <- avg42007
df[17,1] <- avg52007
df[18,1] <- avg62007
df[19,1] <- avg72007
df[20,1] <- avg82007
df[21,1] <- avg92007
df[22,1] <- avg102007
df[23,1] <- avg112007
df[24,1] <- avg122007
df[25,1] <- avg12008
df[26,1] <- avg22008
df[27,1] <- avg32008
df[28,1] <- avg42008
df[29,1] <- avg52008
df[30,1] <- avg62008
df[31,1] <- avg72008
df[32,1] <- avg82008
df[33,1] <- avg92008
df[34,1] <- avg102008
df[35,1] <- avg112008
df[36,1] <- avg122008
df[37,1] <- avg12009
df[38,1] <- avg22009
df[39,1] <- avg32009
df[40,1] <- avg42009
df[41,1] <- avg52009
df[42,1] <- avg62009
df[43,1] <- avg72009
df[44,1] <- avg82009
df[45,1] <- avg92009
df[46,1] <- avg102009
df[47,1] <- avg112009
df[48,1] <- avg122009
df[49,1] <- avg12010
df[50,1] <- avg22010
df[51,1] <- avg32010
df[52,1] <- avg42010
df[53,1] <- avg52010
df[54,1] <- avg62010
df[55,1] <- avg72010
df[56,1] <- avg82010

colnames(df)[1] <- "SalePrice"

amesdata <- cbind(amesdata,df)

##Log house prices to be more fair comparing low vs high price homes
amesdata <- amesdata %>% mutate(LogSalePrice = log(SalePrice))
amesdata <- amesdata %>% select(LogSalePrice, everything())
amesdata$SalePrice <- NULL

summary(amesdata$LogSalePrice)
amesdata %>% ggplot(aes(x = LogSalePrice)) + geom_density()


## Analysis 

## Helper Functions
OSR2 <- function(predictions, train, test) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

printMetricsHelp <- function(train, test, pred.train, pred.test, doExp) {
  if(doExp) {
    train <- exp(train)
    test <- exp(test)
    pred.train <- exp(pred.train)
    pred.test <- exp(pred.test)
  }
  
  trainRsq <- OSR2(pred.train, train, train)
  testRsq <- OSR2(pred.test, train, test)
  trainMAE <- mean(abs(train - pred.train))
  testMAE <- mean(abs(test - pred.test))
  trainRMSE <- sqrt(mean((train - pred.train)^2))
  testRMSE <- sqrt(mean((test - pred.test)^2))
  
  print(str_c("Training set R^2: ", trainRsq))
  print(str_c("Training set MAE: ", trainMAE))
  print(str_c("Training set RMSE: ", trainRMSE))
  print(str_c("Test set R^2: ", testRsq))
  print(str_c("Test set MAE: ", testMAE))
  print(str_c("Test set RMSE: ", testRMSE))
}

printMetrics <- function(train, test, pred.train, pred.test) {
  print("Metrics for Log(Sale Price):")
  printMetricsHelp(train, test, pred.train, pred.test, FALSE)
  print("")
  print("Metrics for Sale Price:")
  printMetricsHelp(train, test, pred.train, pred.test, TRUE)
}


## Train/Test Split


train <- amesdata %>% filter(Date %in% c("1/1/2006","2/1/2006","3/1/2006",
"4/1/2006","5/1/2006","6/1/2006","7/1/2006","8/1/2006","9/1/2006","10/1/2006",
"11/1/2006","12/1/2006","1/1/2007","2/1/2007","3/1/2007","4/1/2007",
"5/1/2007","6/1/2007","7/1/2007","8/1/2007","9/1/2007","10/1/2007",
"11/1/2007","12/1/2007","1/1/2008","2/1/2008","3/1/2008","4/1/2008",
"5/1/2008","6/1/2008","7/1/2008","8/1/2008","9/1/2008","10/1/2008",
"11/1/2008","12/1/2008"))
test <- amesdata %>% filter(Date %in% c("1/1/2009","2/1/2009","3/1/2009",
"4/1/2009","5/1/2009","6/1/2009","7/1/2009","8/1/2009","9/1/2009","10/1/2009",
"11/1/2009","12/1/2009","1/1/2010","2/1/2010","3/1/2010","4/1/2010","5/1/2010",
"6/1/2010","7/1/2010","8/1/2010"))

train$Date <- NULL
test$Date <- NULL


trainY <- train$LogSalePrice
testY <- test$LogSalePrice

poly_form <- (LogSalePrice ~ poly(AEGoodProducing,1) + poly(AEGovernmentInAmes,1) + poly(AEPrivateServiceProviding,1)+
poly(AEState.Government.in.Ames,1) + poly(All_Employees,1) + poly(All_Employees_Federal_Gov,1)
+ poly(All_Employees_Local_Gov,1) + poly(All_Employees_Service_Providing,1) + poly(All_Employees_Total_Private,1) + poly(AQTW,1)
+ poly(AQWWEPE,1) + poly(ATHPI,1) + poly(AverageHourlyEarning,1) + poly(Avg_Weekly_Earnings,1)
+ poly(AWWEFGE,1) + poly(LaborForce,1) + poly(NPAISC,1) + poly(Unemployment,1)-1 +.)

trainX <- model.matrix(poly_form, data = train)
testX <- model.matrix(poly_form, data = test)

## Naive Linear Regression
train.df <- as.data.frame(cbind(trainY, trainX))
mod.naive <- lm(trainY ~ ., data = train.df)
summary(mod.naive)


##"Common Sense" Linear Regression
mod.commonsense <- lm(LogSalePrice ~ ., data = train)
summary(mod.commonsense)


## Predictions for Common Sense Linear Regression
pred.commonsense.train <- predict(mod.commonsense)
pred.commonsense.test <- predict(mod.commonsense, newdata = test)
printMetrics(trainY, testY, pred.commonsense.train, pred.commonsense.test)

## Ridge Regression
set.seed(1112)
mod.ridge <- glmnet(x = trainX, y = trainY, alpha = 0)
mod.ridge$lambda
coefs.ridge <- coef(mod.ridge)
plot(mod.ridge, xvar = "lambda")

set.seed(3134)
cv.ridge <- cv.glmnet(x = trainX, y = trainY, alpha = 0)
plot(cv.ridge)


print(str_c("Chosen lambda: ", cv.ridge$lambda.1se))

pred.ridge.train <- predict(cv.ridge, newx = trainX)
pred.ridge.test <- predict(cv.ridge, newx = testX)
printMetrics(trainY, testY, pred.ridge.train, pred.ridge.test)

## LASSO
set.seed(3439)
mod.lasso <- glmnet(x = trainX, y = trainY, alpha = 1)

mod.lasso$lambda
coefs.lasso <- coef(mod.lasso)
plot(mod.lasso, xvar = "lambda")

set.seed(821)
cv.lasso <- cv.glmnet(x = trainX, y = trainY, alpha = 1)
cv.lasso$lambda.min
cv.lasso$lambda.1se
plot(cv.lasso)

pred.lasso.train <- predict(cv.lasso, newx = trainX)
pred.lasso.test <- predict(cv.lasso, newx = testX)

nzero.lasso <- predict(cv.lasso, type = "nonzero")
printMetrics(trainY, testY, pred.lasso.train, pred.lasso.test)

##Forward Stepwise
mod.initial <- lm(trainY ~ 1, data = train.df)
forward.big <- formula(lm(trainY ~ ., data = train.df))
mod.forward <- step(mod.initial, steps = 25, direction = "forward", scope = forward.big)

test.df <- as.data.frame(cbind(testY, testX))
pred.forward.train <- predict(mod.forward, newdata = train.df)
pred.forward.test <- predict(mod.forward, newdata = test.df)
printMetrics(trainY, testY, pred.forward.train, pred.forward.test)

##Random Forest
set.seed(95)
mod.rf <- randomForest(x = trainX, y = trainY, do.trace = FALSE)
pred.rf.train <- predict(mod.rf, newdata = trainX)
pred.rf.test <- predict(mod.rf, newdata = testX)

printMetrics(trainY, testY, pred.rf.train, pred.rf.test)

##Deep Learning
h2o.init()
amestrain.hex <- as.h2o(train, destination_frame="amestrain.hex")
amestest.hex <- as.h2o(test, destination_frame = "amestest.hex")
dl_fit <- h2o.deeplearning(x = 2:26,
                           y = 1,
                           training_frame = amestrain.hex,
                           model_id = "dl_fit",
                           epochs = 20,
                           hidden= c(10,10),
                           stopping_rounds = 0,  
                           seed = 1)

dl_perf <- h2o.performance(model = dl_fit, newdata = amestest.hex)
print(dl_perf)



