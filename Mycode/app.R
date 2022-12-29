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
cases.all <- filter(cases.all, 發病年份 >= 2016)
gene.all <- rbind(gene.2016, gene.2017)

cases.all$性別 = as.factor(cases.all$性別)
cases.all$是否為境外移入 = as.factor(cases.all$是否為境外移入)
cases.all$年齡層 = as.factor(cases.all$年齡層)

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

table.month <- table(cases.all$發病月份)
table.sex <- table(cases.all$性別)
lbls.sex <- c("Male", "Female")
slices <- c(round(table.sex[2] / length(cases.all$性別) * 100, 2), round(table.sex[1] / length(cases.all$性別) * 100, 2))
chart.sex <- pie(slices, labels = paste0(lbls.sex, "\n", slices, "%"), col = rainbow(length(lbls.sex)), main = "Pie Chart of Sex", cex=1.5)
  

chart.month <- ggplot(data = cases.all, aes(x = 發病月份)) +
  geom_histogram(bins = 12, fill = "white", colour = 4) +
  theme(text = element_text(family = "PingFang TC Regular")) +
  labs(title="台灣流感重症病發症月分佈圖(2003-2022)", x ="月份", y = "確診數") +
  scale_x_continuous(breaks=1:12)
chart.month
