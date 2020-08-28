library(readxl)
library(reshape2)

setwd("C:/Users/17346/Documents/IOE 691 - Predictive Analytics and Machine Learning")

data1 = data.frame(read_excel('error1.xlsx'))
MAE_matrix1 <- data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c("GLM","GAM","CART","Random Forest","Neural Network"))))
MSE_matrix1 <- data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c("GLM","GAM","CART","Random Forest","Neural Network"))))
MAE_matrix2 <- data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c("GLM","GAM","CART","MARS","Random Forest"))))
MSE_matrix2 <- data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c("GLM","GAM","CART","MARS","Random Forest"))))

for(i in seq(1,20,1))
{
  c1 = c()
  c2 = c()
  for(j in seq(1,6,1))
  {
    c1[j] <- data1[i+(j*20)-20,3]
    c2[j] <- data1[i+(j*20)-20,4]
  }
  MAE_matrix1[nrow(MAE_matrix1)+1,1:5] <- c1
  MSE_matrix1[nrow(MSE_matrix1)+1,1:5] <- c2
}
print(MAE_matrix1)
print(MSE_matrix1)


melt_mae1 = melt(MAE_matrix1)
melt_mse1 = melt(MSE_matrix1)

test1 <-  pairwise.t.test(melt_mae1$value, melt_mae1$variable, p.adjust = "none")[["p.value"]]
test2 <-  pairwise.t.test(melt_mse1$value, melt_mse1$variable, p.adjust = "none")[["p.value"]]

for(k in seq(1,nrow(data2),5))
{
  c3 = c()
  c4 = c()
  for(l in seq(1,5,1))
  {
    c3[l] <- data2[k+l-1,3]
    c4[l] <- data2[k+l-1,4]
  }
  MAE_matrix2[nrow(MAE_matrix2)+1,1:5] <- c3
  MSE_matrix2[nrow(MSE_matrix2)+1,1:5] <- c4
}

melt_mae2 = melt(MAE_matrix2)
melt_mse2 = melt(MSE_matrix2)

test3 <-  pairwise.t.test(melt_mae2$value, melt_mae2$variable, p.adjust = "none")[["p.value"]]
test4 <-  pairwise.t.test(melt_mse2$value, melt_mse2$variable, p.adjust = "none")[["p.value"]]
