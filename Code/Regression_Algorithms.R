#####################################################
########################           REGRESSION PROBLEM
#####################################################

### SAMPLE EXAMPLE
setwd("/Users/subasishdas1/Desktop/TAMU_ITE_WKSHP1_Feb17/Data")
mydata1 <- read.csv("data01.csv")
head(mydata1)
dim(mydata1)
str(mydata1)

### install.packages()
instll <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("party", "glmnet", "pls", "RColorBrewer", "scales", "grid", "ggplot2")
instll(packages)

### Linear Regression
library(party)
fit <- lm(freq ~., mydata1)
summary(fit)
predictions <- predict(fit, mydata1)
rmse <- mean((mydata1$freq - predictions)^2)
print(rmse)


#### Principal Component Regression
library(pls)
fit <- pcr(freq~., data=mydata1, validation="CV")
summary(fit)
predictions <- predict(fit, mydata1)
rmse <- mean((mydata1$freq - predictions)^2)
print(rmse)

### Ridge Regression
library(glmnet)
x <- as.matrix(mydata1[,1:5])
y <- as.matrix(mydata1[,6])
fit <- glmnet(x, y, family="gaussian", alpha=0, lambda=0.001)
summary(fit)
predictions <- predict(fit, x, type="link")
rmse <- mean((y - predictions)^2)
print(rmse)


### Least Absolute Shrinkage and Selection Operator
library(lars)
x <- as.matrix(mydata1[,1:5])
y <- as.matrix(mydata1[,6])
fit <- lars(x, y, type="lasso")
summary(fit)
predictions <- predict(fit, x, type="link")
rmse <- mean((y - predictions)^2)
print(rmse)

### Support Vector Regression
library(kernlab)
fit <- ksvm(freq~., data=mydata1)
summary(fit)
predictions <- predict(fit, mydata1)
rmse <- mean((mydata1$freq - predictions)^2)
print(rmse)

### k-Nearest Neighbor
library(caret)
fit <- knnreg(mydata1[,1:5], mydata1[,6], k=3)
summary(fit)
predictions <- predict(fit, mydata1[,1:5])
rmse <- mean((mydata1$freq - predictions)^2)
print(rmse)

### Neural Network
library(nnet)
data(longley)
x <- mydata1[,1:6]
y <- mydata1[,7]
# fit model
fit <- nnet(freq~., mydata1, size=12, maxit=500, linout=T, decay=0.01)
summary(fit)
# make predictions
predictions <- predict(fit, x, type="raw")
rmse <- mean((y - predictions)^2)
print(rmse)

### Decision Tree
library(rpart)
# fit model
fit <- rpart(freq~., data=mydata1, control=rpart.control(minsplit=5))
# summarize the fit
summary(fit)
library(rpart.plot)
rpart.plot(fit, main="Decision Tree")

