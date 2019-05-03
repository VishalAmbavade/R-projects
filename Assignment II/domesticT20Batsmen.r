library(dplyr)
library(tidyverse)
library(ggplot2)
library(cluster)
library(factoextra)

domestict20batting <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\domestict20careerbattingrating_mod.csv")

head(domestict20batting)

summary(domestict20batting)

set.seed(20)

domesticBatCluster <- kmeans(domestict20batting[, 2:15], 3)

domesticBatCluster$cluster <- as.factor(domesticBatCluster$cluster)
#domesticBatCluster$cluster

ggplot(domestict20batting, aes(Innings, Runs/Innings, color = domesticBatCluster$cluster)) +
  geom_point(size = 2) +
  scale_color_hue(labels = c("Best players", "Good players", "Bad Players")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Domestic T20 Batting Average")

library(plotly)

p <- plot_ly(domestict20batting, x = ~Innings, y = ~Runs/Innings, type = 'scatter', 
             mode = 'markers', color = domesticBatCluster$cluster, 
             text = ~paste('Name: ', Name)) %>%
  layout(title = "Cluster of Averages (batsmen)")

p

head(domestict20batting)


df <- select(domestict20batting, Average, Strike_Rate, Runs.Innings)
domesticBatCluster1 <- kmeans(df, 3)
domesticBatCluster1$cluster <- as.factor(domesticBatCluster1$cluster)

q <- plot_ly(domestict20batting, x = ~Matches, y = ~Strike_Rate, type = 'scatter',
             mode = 'markers', color = domesticBatCluster1$cluster,
             text = ~paste('Name: ', Name)) %>%
  layout(title = "Cluster of matches vs Strike Rate")
q

str(domestict20batting)
any(is.na(domestict20batting))

domestict20batting_label <- domestict20batting$Stumpings
domestict20batting$Stumpings <- NULL
str(domestict20batting)

domestict20batting_label <- domestict20batting$Name
domestict20batting$Name <- NULL
str(domestict20batting)

domestict20batting_sc <- as.data.frame(scale(domestict20batting))
summary(domestict20batting_sc)

dist_mat <- dist(domestict20batting_sc, method = 'euclidean')

hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)

cut_avg <- cutree(hclust_avg, k = 6)

plot(hclust_avg)
rect.hclust(hclust_avg , k = 6, border = 2:6)
abline(h = 3, col = 'red')
library(dendextend)

avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)

domestict20batting_cl <- mutate(domestict20batting, cluster = cut_avg)
count(domestict20batting_cl, cluster)

ggplot(domestict20batting_cl, aes(x=Matches, y = Runs, color = factor(cluster))) + geom_point()
