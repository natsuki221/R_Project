setwd("/Users/lintzujeng/Documents/GitHub/R_Project/Mycode")
library(tidyverse)

cases1.all <- read.csv("dataset.csv", header = T)
cases2.all <- read.csv("dataset2.csv", header = T)
gene.2016 <- read.csv("2016_geneSequencing.csv", header = T)
gene.2017 <- read.csv("2017_geneSequencing.csv", header = T)

for (i in 1:nrow(cases1.all)) {
  cases1.all$發病週別[i] = ceiling(cases1.all$發病週別[i] / 4.416667)
}
names(cases1.all)[3] = "發病月份"
cases2.all$發病月份 = as.numeric(cases2.all$發病月份)

cases.all <- rbind(cases1.all, cases2.all)
cases.all <- cases.all[-1]
gene.all <- rbind(gene.2016, gene.2017)

cases.all$性別 = as.factor(cases.all$性別)
cases.all$是否為境外移入 = as.factor(cases.all$是否為境外移入)

gene.flu <- filter(gene.all, 病毒總類 == "Influenza")
cases.domestic <- filter(cases.all, 是否為境外移入 == "否")
cases.foreign <- filter(cases.all, 是否為境外移入 == "是")
cases.male <- filter(cases.all, 性別 == "M")
cases.female <- filter(cases.all, 性別 == "F")

gene.flu <- gene.flu %>%
  separate(發病日期, c("year", "month", "day"), "/")

for (i in 8:10) {
  gene.flu[,i] <- as.numeric(gene.flu[,i])
}
