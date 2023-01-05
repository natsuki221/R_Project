setwd("/Users/lintzujeng/Documents/GitHub/R_Project/Mycode")
library(tidyverse)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(ipred)

gene.2016 <- read.csv("2016_geneSequencing.csv", header = T)
gene.2017 <- read.csv("2017_geneSequencing.csv", header = T)
gene.all <- rbind(gene.2016, gene.2017)
gene.flu <- filter(gene.all, 病毒總類 == "Influenza")

gene.flu <- gene.flu %>%
  separate(發病日期, c("year", "month", "day"), "/")

for (i in 8:10) {
  gene.flu[,i] <- as.numeric(gene.flu[,i])
}

flu.factor <- subset(gene.flu, select = c(年齡, 性別, 區域, year, month, day))

flu.factor$性別 <- ifelse(flu.factor$性別 == "男", 1, 0)
flu.factor$區域[flu.factor$區域 == "北區"] <- 1
flu.factor$區域[flu.factor$區域 == "中區"] <- 2
flu.factor$區域[flu.factor$區域 == "南區"] <- 3
flu.factor$區域[flu.factor$區域 == "東區"] <- 4
df.names <- names(flu.factor)
flu.factor[, df.names] <- lapply(flu.factor[,df.names], factor)

n <- nrow(flu.factor)
set.seed(1111)
new.flu.factor <- flu.factor[sample(n),]

t.idx <- sample(seq_len(n), round(0.7*n))

traindata <- new.flu.factor[t.idx, ]
testdata <- new.flu.factor[-t.idx, ]

dTree <- rpart(formula = 年齡 ~ ., data = traindata, method = "anova", control = rpart.control(cp = 0, xval = 10))
dTree
plotcp(dTree)


fancyRpartPlot(dTree)


result <- predict(dTree, newdata = testdata, type = "vector")
result

cm <- data.frame(table(testdata$年齡, result, dnn = c("實際", "預測")))
cm


dTree.bagged <- bagging(formula = 年齡 ~ ., data = traindata, method = "treebag", trainControl = T, importance = T)
dTree.bagged


