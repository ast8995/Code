require(neuralnet)
require(dbarts)
require(BayesTree)
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
require(stats)

setwd("C:/Users/17346/Documents/IOE 691 - Predictive Analytics and Machine Learning")

data1 = data.frame(read_excel('train.xlsx'))
data2 <- data1[(1:20000),]
ResultMatrix <- data.frame(matrix(ncol=6,nrow=0, dimnames=list(NULL, c("Model","Fold","MAE","MSE","RMSE","R-squared"))))
set.seed(256)

hs<-data.frame(replicate(30,runif(20000,0,1)))
col_name <- colnames(hs)

for(i in seq(1,30,1))
{  
  test <- data.frame(matrix(ncol=82,nrow=0, dimnames=list(NULL, colnames(data2))))
  train <- data.frame(matrix(ncol=82,nrow=0, dimnames=list(NULL, colnames(data2))))
  for (j in seq(1,20000,1))
  {
    if(hs[j,col_name[i]] >0.8){
      test <- rbind(test,data2[j,])
    }
    else{
      train <- rbind(train,data2[j,])
    }
  }
  mean0 <- apply(train,2,mean)
  sd0 <- apply(train,2,sd)
  train1 <- sweep(sweep(train,2L,mean0),2,sd0,"/" )
  t1 <-train1[-1]
  d1 <- cor(t1)
  r2 <- findCorrelation(d1,cutoff = 0.9)
  r3 <- matrix(r2,nrow = 1, ncol = length(r2))
  r4 <- t1[,-r3]
  data3 <- train1$critical_temp
  data4 <- cbind(data3,r4)
  m1 <- randomForest(train1[-1],train1[,1],mtry = 6)
  test1 <- sweep(sweep(test,2L,mean0),2,sd0,"/" )
  t2 <- test1[-1]
  t3 <- data.frame(t2[,-r3])
  p1 <- predict(m1, newdata = t2)
  p1 <- p1*sd0[1]+mean0[1]
  d2 <- test1[,1]
  d2 <- d2*sd0[1]+mean0[1]
  e1 <- d2-p1
  mae1 <- mean(abs(e1))
  mse1 <- mean(e1^2)
  rmse1 <- mse1^0.5
  rsquared <- 1 - (sum((d2-p1)^2)/sum((d2-mean(d2))^2))
  ResultMatrix[nrow(ResultMatrix)+1,1:6] = c("RF",i,mae1,mse1,rmse1,rsquared)
  p3 <- data.frame(p1)
  d3 <- data.frame(d2)
  print(i)
}
