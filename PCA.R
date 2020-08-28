library(reshape2)
library(caret)
library(MASS)
library(readxl)
require(caret)
require(factoextra)
require(rpart)
require(mgcv)
require(earth)
require(randomForest)
# Setting working directory
setwd("C:/Users/17346/Documents/IOE 691 - Predictive Analytics and Machine Learning")

# Reading the file
data1 = data.frame(read_excel('BigData_1.xlsx'))

k <- 20
folds <- cut(seq(1, nrow(data1)),breaks = k, labels = FALSE)
head(folds)
set.seed(256)
a3 <- data.frame(matrix(ncol = 28,nrow = 0,dimnames = list(NULL, c(colnames(data1)))))
a4 <- data.frame(matrix(ncol = 28,nrow = 0,dimnames = list(NULL, c(colnames(data1)))))
ResultMatrix <- data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("Model","Fold","MAE","MSE"))))
ResultMatrix1 <- data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("Model","Fold","MAE","MSE"))))
for (i in seq(1,k,1))
{
  testid <- which(folds == i, arr.ind = TRUE)
  test1 <- data1[testid, ]
  train1 <- data1[-testid, ]
  a1 <- scale(train1,center = TRUE, scale = TRUE)
  trainmean <- apply(train1,2,mean)
  trainsd <- apply(train1,2,sd)
  trans_train <- sweep(sweep(train1,2L,trainmean),2,trainsd,"/" )
  trans_train1  <- trans_train[-1]
  mypc <- prcomp(trans_train1)
  z1 <- mypc$rotation
  z2 <- get_eigenvalue(mypc)
  pc1 <- which(z2$cumulative.variance.percent > 90)[1]
  pc2 <- pc1-1
  a5 <- mypc$x
  x1 <- cor(trans_train1)
  x2 <- findCorrelation(x1,cutoff = 0.85)
  x3  <- matrix(x2,nrow = 1,ncol = length(x2))
  x4 <- trans_train1[,-x3]
  train_final <- cbind(g=trans_train$g,a5[,c(1:pc2)])
  train_final1 <- cbind(g=trans_train$g,x4)
  a6 <- data.frame(matrix(ncol = c(pc2+1),nrow = 0,dimnames = list(NULL, c(colnames(train_final)))))
  a61 <- data.frame(matrix(ncol = c(length(x4)+1),nrow = 0,dimnames = list(NULL, c(colnames(train_final1)))))
  a6 <- rbind(a6,train_final)
  a61 <- rbind(a61,train_final1)
  m4 <- train(a6[-1],a6[,1],method = "glm")
  m5 <- train(a6[-1],a6[,1],method = "gam")
  m6 <- train(a6[-1],a6[,1],method = "rpart")
  m7 <- train(a6[-1],a6[,1],method = "earth")
  m8 <- randomForest(a6[-1],a6[,1])
  n4 <- train(a61[-1],a61[,1],method = "glm")
  n5 <- train(a61[-1],a61[,1],method = "gam")
  n6 <- train(a61[-1],a61[,1],method = "rpart")
  n7 <- train(a61[-1],a61[,1],method = "earth")
  n8 <- randomForest(a61[-1],a61[,1])
  trans_test <- sweep(sweep(test1,2L,trainmean),2,trainsd,"/" )
  trans_test1 <- trans_test[-1]
  o1 <- c(z1)
  test_temp <- as.matrix(trans_test1) %*% mypc$rotation
  test_temp1 <- cbind(g=trans_test$g,test_temp[,c(1:pc2)])
  test_temp2 <- cbind(g=trans_test$g,trans_test1[,-x3])
  a7 <- data.frame(matrix(ncol = c(pc2+1),nrow = 0,dimnames = list(NULL, c(colnames(test_temp1)))))
  a7 <- rbind(a7,test_temp1)
  a71 <- data.frame(matrix(ncol = c(length(x4)+1),nrow = 0,dimnames = list(NULL, c(colnames(test_temp2)))))
  a71 <- rbind(a71,test_temp2)
  models <- list(m4,m5,m6,m7,m8)
  models2 <- list(n4,n5,n6,n7,n8)
  models1 <- c("GLM","GAM","CART","MARS","RANDOM FOREST")
  
  for(z in seq(1,length(models),1))
  {
  test_forecast <- predict(models[[z]], newdata = a7)
  test_forecast1 <- predict(models2[[z]], newdata = a71)
  u1 <- test_forecast
  u2 <- test_forecast1
  w1 = a7$g
  w2 = a71$g
  error1 <-  w1- u1
  error2 <- w2 - u2
  mae1 <- mean(abs(error1))
  mae2 <- mean(abs(error2))
  mse1 <- mean(error1^2)
  mse2 <- mean(error2^2)
  ResultMatrix[nrow(ResultMatrix)+1,1:4] = c(models1[z],i,mae1,mse1)
  ResultMatrix1[nrow(ResultMatrix1)+1,1:4] = c(models1[z],i,mae2,mse2)
  }
}
print(ResultMatrix)
print(ResultMatrix1)