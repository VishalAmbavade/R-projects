library(ggplot2)
library(plotly)

odiBattingFielding <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Statistics\\odicareerbattingandfielding.csv")

df <- odiBattingFielding[, 1:11]
head(df)

df <- na.omit(df)
head(df)

summary(df)

set.seed(20)

df1 <- df %>%
  select(3, 10)

df2 <- df %>%
  select(3, 4, 8)

OBFCluster <- kmeans(df1, 5)

OBFCluster$cluster <- as.factor(OBFCluster$cluster)

ggplot(df1, aes(Innings, Strike_Rate, color = OBFCluster$cluster)) +
  geom_point(size = 2) +
  scale_color_hue(labels = c("Best players",  "Bad Players", "Good players", "Useless", "Worst Players")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("ODI Career Matches vs Strike Rate")

p <- plot_ly(df1, x = ~Innings, y = ~Strike_Rate, type = 'scatter', 
             mode = 'markers', color = OBFCluster$cluster, 
             text = ~paste('Name: ', df$Name)) %>%
  layout(title = "Cluster of Averages (batsmen)")

p

OBFCluster2 <- kmeans(df2, 5)
OBFCluster2$cluster <- as.factor(OBFCluster2$cluster)

ggplot(df, aes(Innings, No_Of_100, color = OBFCluster2$cluster)) +
  geom_point(size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Players with more notouts and centuries")

q <- plot_ly(df2, x = ~Innings, y = ~No_Of_100, type = 'scatter',
             mode = 'markers', color = OBFCluster2$cluster,
             text = ~paste('Name: ', df$Name)) %>%
  layout(title = "Cluster of notouts and centuries")
q
