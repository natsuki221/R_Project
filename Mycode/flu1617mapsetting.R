setwd("/Users/lintzujeng/Documents/GitHub/R_Project/Mycode")
par(family="STKaitiTC")
library(tidyverse)
library(stringr)
library(data.table)
library(maptools)
library(knitr)
library(kableExtra)
library(janitor)

gene.2016 <- read.csv("2016_geneSequencing.csv", header = T)
gene.2017 <- read.csv("2017_geneSequencing.csv", header = T)
gene.all <- rbind(gene.2016, gene.2017)
gene.flu <- filter(gene.all, 病毒總類 == "Influenza")
gene.flu <- gene.flu %>%
  separate(發病日期, c("year", "month", "day"), "/")
for (i in 8:10) {
  gene.flu[,i] <- as.numeric(gene.flu[,i])
}

table.countary = data.frame(table(gene.flu$縣市))
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
  labs(title="台灣流感人口分佈圖(2016-2017)", x ="經度", y = "緯度")

twcmap
