library(ggplot2)
library(plotly)

odiBowling <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Statistics\\odicareerbowling.csv")

df <- odiBowling[, 1:10]
head(df)

df <- na.omit(df)
head(df)

summary(df)

set.seed(20)

BWE <- df %>%
  select(2, 5, 10)

df2 <- df %>%
  select(3, 4, 8)

BWECluster <- kmeans(BWE, 5)

BWECluster$cluster <- as.factor(BWECluster$cluster)

plot_ly(BWE, x = ~Wickets, y = ~Economy, type = 'scatter',
        mode = 'markers', color = BWECluster$cluster,
        text = ~paste('Name: ', df$Name)) %>%
  layout(title = "Cluster of wickets & economy")
