library(ggplot2)
library(plotly)

testBattingFielding <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Statistics\\testcareerbattingandfielding.csv")

df <- testBattingFielding[, 1:12]
head(df)

df <- na.omit(df)
head(df)

summary(df)

set.seed(20)

testStumping <- testBattingFielding %>%
  filter(Stumpings != 'NA') %>%
  select(2, 5, 12)

testStumpingCluster <- kmeans(testStumping, 3)

testStumpingCluster$cluster <- as.factor(testStumpingCluster$cluster)

plot_ly(testStumping, x = ~Matches, y = ~Stumpings, type = 'scatter',
        mode = 'markers', color = testStumpingCluster$cluster,
        text = ~paste('Name: ', df$Name)) %>%
  layout(title = "Cluster of wickets & economy")
