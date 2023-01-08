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
cases.all$年齡層 = as.factor(cases.all$年齡層)

###################################function area################################

age.range <- c("0-4","5-9","10-14","15-19","20-24","25-29", "30-34","35-39",
               "40-44","45-49","50-54","55-59","60-64","65-69","70+")
age.counter <- function(m) {
  age.cnt <- as.numeric(rep(0, times = 15))
  for (i in m) {
    if (i == "0" | i == "1" | i == "2" | i == "3" | i == "4") {
      age.cnt[1] = age.cnt[1] + 1
    } else if (i == "5-9") {
      age.cnt[2] = age.cnt[2] + 1
    } else if (i == "10-14") {
      age.cnt[3] = age.cnt[3] + 1
    } else if (i == "15-19") {
      age.cnt[4] = age.cnt[4] + 1
    } else if (i == "20-24") {
      age.cnt[5] = age.cnt[5] + 1
    } else if (i == "25-29") {
      age.cnt[6] = age.cnt[6] + 1
    } else if (i == "30-34") {
      age.cnt[7] = age.cnt[7] + 1
    } else if (i == "35-39") {
      age.cnt[8] = age.cnt[8] + 1
    } else if (i == "40-44") {
      age.cnt[9] = age.cnt[9] + 1
    } else if (i == "45-49") {
      age.cnt[10] = age.cnt[10] + 1
    } else if (i == "50-54") {
      age.cnt[11] = age.cnt[11] + 1
    } else if (i == "55-59") {
      age.cnt[12] = age.cnt[12] + 1
    } else if (i == "60-64") {
      age.cnt[13] = age.cnt[13] + 1
    } else if (i == "65-69") {
      age.cnt[14] = age.cnt[14] + 1
    } else if (i == "70+") {
      age.cnt[15] = age.cnt[15] + 1
    }
  }
  
  return(age.cnt)
}

#建立一個function<sampler>做sampling
sampler <- function(m, t1, t2){
  n = t1 #sample 次數
  sample.mean <- rep(NA, times = n)#建立一個list存所有sampling的mean
  for (i in 1:n) {#use for loop to sampling
    sample.mean[i] <- mean(sample(m, t2))#把sample出來的mean存入sample.mean
  }
  excepted <- mean(sample.mean)#get expected value
  return(c(excepted, sample.mean))#return expected value
}

#建立一個function<sampler>做sampling
sampler.str <- function(scol, t1, t2){
  n = t1 #sample 次數
  sample.mode <- rep(NA, times = n)#建立一個list存所有sampling的mode
  dataset <- data.frame()
  for (j in 1:nrow(scol)) {
    tempdf <- data.frame(var = rep(scol$range[j], times = scol$age[j]))
    dataset <- rbind(dataset, tempdf)
    tempdf <- tempdf[,-1]
  }
  for (i in 1:n) {#use for loop to sampling
    data <-sample(dataset$var, t2)
    sample.mode[i] <- names(table(data))[which.max(table(data))]
    #把sample出來的mode存入sample.mode
  }
  mode.df <- data.frame(sample.mode = age.range)
  table.mode.df <- data.frame(table(sample.mode))
  mode.df <- inner_join(mode.df, table.mode.df, by = "sample.mode")
  
  return(mode.df)#return df
}

age.male <- filter(cases.all, 性別 == "M")
age.male.cnt <- age.counter(age.male$年齡層)
age.female <- filter(cases.all, 性別 == "F")
age.female.cnt <- age.counter(age.female$年齡層)
age.count.df <- data.frame(group = as.numeric(1:length(age.range)), 
                           range = age.range, 
                           male = age.male.cnt * (-1), 
                           female = age.female.cnt)

###################################age pyamid###################################

library(grid)
vplayout <- function(x, y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}
v.max <- 6000
dig.temp <- nchar(as.character(v.max))
lim.1 <- c(-v.max, 0)
lim.2 <- c(0, v.max)
by <- round(v.max / (5 * 10 ^ (dig.temp - 2))) * 10 ^ (dig.temp - 2)
bre.1 <- seq(from = 0, to = -v.max, by = -by)
bre.2 <- seq(from = 0, to = v.max, by = by)
lab.1 <- seq(from = 0, to = v.max, by = by)
lab.2 <- seq(from = 0, to = v.max, by = by)
mg.1 <- unit(c(0, 0.0, 0.3, 0.5), "lines")  # 上右下左
mg.2 <- unit(c(0, 0.5, 0.3, 0.0), "lines")
mg.3 <- unit(c(0, 0.0, 2.3, 0.0), "lines")
mg.l <- margin(0, 0, 0, 0, 'lines')
text.x <- rep(2, nrow(age.count.df))
text.y <- 1:nrow(age.count.df)
title.x <- 2
title.y <- 2
title.lab <- ('2003-2022流感併發重症男女年齡層金字塔圖')

# 圖形
p.1 <- ggplot(age.count.df) +
  geom_bar(aes(group, male), fill = 'skyblue', stat="identity", position="dodge") +
  scale_y_continuous(limits = lim.1, breaks = bre.1, labels = lab.1) +
  scale_x_continuous(limits = c(0, (nrow(age.count.df) + 1)), 
                     breaks = 1:nrow(age.count.df), labels = NULL, 
                     expand = expansion(), position = 'top') +
  theme(plot.margin = mg.1, axis.text = element_text(margin = mg.l)) +
  theme(text = element_text(family = "PingFang TC Regular")) +
  xlab(NULL) +
  ylab('男') +
  coord_flip() + 
  guides(fill = none)
p.1

p.2 <- ggplot(age.count.df) +
  geom_bar(aes(group, female), fill = 'firebrick1', stat="identity", 
           position="dodge") +
  scale_y_continuous(limits = lim.2, breaks = bre.2, labels = lab.2) +
  scale_x_continuous(limits = c(0, (nrow(age.count.df) + 1)), 
                     breaks = 1:nrow(age.count.df), labels = NULL, 
                     expand = expansion()) +
  theme(plot.margin = mg.2, axis.text = element_text(margin = mg.l)) +
  theme(text = element_text(family = "PingFang TC Regular")) +
  xlab(NULL) +
  ylab('女') +
  coord_flip() +
  guides(fill = none)
p.2

p.3 <- ggplot() + 
  geom_text(aes(x = text.x, y= text.y, label = age.count.df$range), size = 3.6) +
  scale_x_continuous(limits = c(0, 4), breaks = NULL, expand = expansion()) +
  scale_y_continuous(limits = c(0, (nrow(age.count.df) + 1)), breaks = NULL, 
                     expand = expansion()) +
  labs(x = NULL, y = NULL) + 
  theme(text = element_text(family = "PingFang TC Regular")) +
  theme(plot.margin = mg.3,
        axis.text = element_text(margin = mg.l),
        panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
p.3

p.4 <- ggplot() + 
  geom_text(aes(x = title.x, y= title.y, label = title.lab), size = 6, 
            family = "PingFang TC Regular") +
  scale_x_continuous(limits = c(0, 4), breaks = NULL, expand = expansion()) +
  scale_y_continuous(limits = c(0, 4), breaks = NULL, expand = expansion()) +
  labs(x = NULL, y = NULL) +
  #theme(text = element_text(family = "PingFang TC Regular")) +
  theme(plot.margin = mg.l,
        axis.text = element_text(margin = mg.l),
        panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
p.4

# 分面畫圖
grid.newpage()  ##新建頁面
pushViewport(viewport(layout = grid.layout(12, 11))) 
print(p.1, vp = vplayout(2:12, 1:5))
print(p.2, vp = vplayout(2:12, 7:11))
print(p.3, vp = vplayout(2:12, 6))
print(p.4, vp = vplayout(1, 1:11))

#################################1617age.hist###################################

gene.all <- rbind(gene.2016, gene.2017)
gene.flu <- filter(gene.all, 病毒總類 == "Influenza")
plot.hist.1617age <- ggplot(data = gene.flu, aes(x = 年齡)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  geom_vline(aes(xintercept = mean(年齡)), color = "lightblue4", linewidth = 1.25) +
  ggtitle("2016-2017 確診年齡分佈 Histogram") +
  theme(text = element_text(family = "PingFang TC Regular"))
plot.hist.1617age

################################1622age.CLThist#################################

age.all <- data.frame(id = 1:15, range = age.range, age = age.counter(cases.all$年齡層))
age.all.sum <- sum(age.all$age)
for (i in 1:length(age.all$age)) {
  age.all$weight[i] <-round(age.all$age[i] / age.all.sum, 2)
}
age.all.sampling <- sampler.str(age.all, 1000, 5)
plot.hist.0322age <- ggplot(data = age.all.sampling, aes(x = sample.mode, y = Freq)) +
  geom_bar(fill="steelblue1", alpha=0.9, stat = "identity") +
  ggtitle("2003-2022 流感併發重症年齡Histogram use CLT") +
  #geom_density(data = age.all.sampling, aes() color = "white", fill = "cyan", 
  #alpha = 0.8, stat = "identity") +
  labs(y = "modes") +
  theme(text = element_text(family = "PingFang TC Regular"))
plot.hist.0322age

################################1617age.CLThist#################################

age.sampling <- sampler(gene.flu$年齡, 5000, 250)
age.sampling <- data.frame(年齡 = age.sampling[2:length(age.sampling)])
age.sampling.sum <- data.frame(summary(age.sampling))
age.sampling.sum <- age.sampling.sum %>%
  separate(Freq, c("varValue", "value"), ":")
age.sampling.sum = age.sampling.sum[,3:4]

plot.hist.sample <- ggplot(data = age.sampling, aes(x = 年齡)) +
  geom_histogram(aes(y = ..density..), fill="#69b3a2", color="#e9ecef") +
  geom_density(color = "white", fill = "cyan", alpha = 0.4) +
  ggtitle("2016-2017 確診年齡mean分佈 Histogram use CLT") +
  labs(x = "age mean", y = "Frequency") +
  theme(text = element_text(family = "PingFang TC Regular"))
plot.hist.sample

#################################1622age.hist###################################

plot.hist.1622age <- ggplot(data = age.all, aes(x = range, y = age)) +
  geom_bar(fill="purple1", alpha=0.9, stat = "identity") +
  labs(x = "年齡", y = "Frequency") +
  theme(text = element_text(family = "PingFang TC Regular"))
plot.hist.1622age


