library(tidyverse)

df <- read.csv("D:\\R_Project\\Mycode\\dataset.csv", header=TRUE)
head(df)
class(df)
nrow(df)
ncol(df)

#整理資訊
age_table <- data.frame(table(df$年齡層))
age_cont <- count(df$年齡層)
head(age_table)
#繪圖
hist(x = age_table[1], y = age_table[2])

