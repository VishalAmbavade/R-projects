library(dplyr)
library(tidyverse)
library(ggplot2)
library(cluster)
library(factoextra)

domestict20Bowling <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\domestict20careerbowlingrating_mod.csv")

head(domestict20Bowling)

summary(domestict20Bowling)

set.seed(20)

domesticBowlCluster <- kmeans(domestict20Bowling[, 2:13], 5)

domesticBowlCluster$cluster <- as.factor(domesticBowlCluster$cluster)

ggplot(domestict20Bowling, aes(Economy, Wickets, color = domesticBowlCluster$cluster)) +
  geom_point(size = 2) +
  scale_color_hue(labels = c("Bad players", "Good players", "Best Players")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Domestic T20 Bowling Average vs Wickets")

library(plotly)

p <- plot_ly(domestict20Bowling, x = ~Economy, y = ~Wickets, type = 'scatter', 
             mode = 'markers', color = domesticBowlCluster$cluster, 
             text = ~paste('Name: ', Name)) %>%
  layout(title = "Cluster of Averages (batsmen)")

p

