setwd("C:\\Users\\it24102464\\Desktop\\IT24102464")
branch_data<-read.table("Exercise.txt", header = TRUE, sep=",")
str(branch_data)
boxplot(branch_data$Sales_X1, main = "Boxplot of Sales", ylab = "Sales") 
summary(branch_data$Advertising_X2)
quantile(branch_data$Advertising_X2)
IQR(branch_data$Advertising_X2) 
find_outliers <- function(x) { 
  Q1 <- quantile(x, 0.25) 
  
  Q3 <- quantile(x, 0.75) 
  
  IQR <- Q3 - Q1 
  
  lower_bound <- Q1 - 1.5*IQR 
  
  upper_bound <- Q3 + 1.5*IQR 
  
  outliers <- x[x < lower_bound | x > upper_bound] 
  
  return(outliers) 
  
} 
find_outliers(branch_data$Years_X3) 