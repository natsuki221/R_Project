setwd("/Users/lintzujeng/Documents/GitHub/R_Project/Mycode")
par(family="STKaitiTC")
library(tidyverse)
library(stringr)
library(data.table)
library(maptools)
library(knitr)
library(kableExtra)
library(janitor)

cases1.all <- read.csv("dataset.csv", header = T)
cases2.all <- read.csv("dataset2.csv", header = T)
for (i in 1:nrow(cases1.all)) {
  cases1.all$發病週別[i] = ceiling(cases1.all$發病週別[i] / 4.416667)
}
names(cases1.all)[3] = "發病月份"
cases2.all$發病月份 = as.numeric(cases2.all$發病月份)

cases.all <- rbind(cases1.all, cases2.all)
cases.all <- cases.all[-1]
cases.all <- filter(cases.all, 發病年份 >= 2016)
cases.all$性別 = as.factor(cases.all$性別)
cases.all$是否為境外移入 = as.factor(cases.all$是否為境外移入)
cases.all$年齡層 = as.factor(cases.all$年齡層)
cases.all.ka <- kable(cases.all)
cases.all

table.countary = data.frame(table(cases.all$縣市))
names(table.countary)[1] <- "name.zh"
names(table.countary)[2] <- "confirmed"

tw.shp <- readShapeSpatial("twmap/gadm41_TWN_2.shp")
tw.shp.old <- readShapeSpatial("twmap/gadm36_TWN_2.shp")
tw.map.old <- fortify(tw.shp.old)
tw.map <- fortify(tw.shp)
plot.tw.map <- ggplot(tw.map, aes(x = long, y = lat, group=group)) +
  geom_path() +
  coord_map()
plot.tw.map
print(as.character(tw.shp$NAME_2))
chinese.name <- c("金門縣", "連江縣", "新竹市", "高雄市",
                  "新北市", "臺中市", "臺南市", "臺北市",
                  "桃園市", "彰化縣", "嘉義市", "嘉義縣",
                  "新竹縣", "花蓮縣", "基隆市", "苗栗縣",
                  "南投縣", "澎湖縣", "屏東縣", "臺東縣",
                  "宜蘭縣", "雲林縣")

own.df <- data.frame(name.en = tw.shp$NAME_2, name.zh = as.character(tw.shp$NL_NAME_2), id = 0:21)
own.df[5,2] <- "台中市"
own.df[6,2] <- "台南市"
own.df[2,2] <- "連江縣"
own.df$name.zh <- as.factor(own.df$name.zh)


merged.df <- full_join(own.df, table.countary, by = "name.zh")

final.plot <- merge(tw.map, merged.df, by='id', all.x=T)
head(final.plot)


library(RColorBrewer)
library(mapproj)
twcmap <- ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, group = group, 
                   fill = confirmed), 
               color = "black", size = 0.25) + 
  coord_map()+#維持地圖比例
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"), name = "確診人口")+
  theme(text = element_text(family = "PingFang TC Regular"))+
  labs(title="台灣流感重症病發症人口分佈圖(2003~2022)", x ="經度", y = "緯度")

twcmap
