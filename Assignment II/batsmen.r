odibatting2007 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2007odibattingrating.csv")
odibatting2008 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2008odibattingrating.csv")
odibatting2009 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2009odibattingrating.csv")
odibatting2010 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2010odibattingrating.csv")


library(dplyr)
dataOdiBatting <- bind_rows(odibatting2007, odibatting2008, odibatting2009, odibatting2010)

summary(dataOdiBatting)

library(VIM)
aggr(dataOdiBatting)

set.seed(20)

clusters <- kmeans(dataOdiBatting[,2:3], 6)

dataOdiBatting$new <- as.factor(clusters$cluster)

str(clusters)

library(DT)

dataOdiBatting_new <- count_(dataOdiBatting, vars = c('Rating', 'new'), sort = TRUE) %>%
    arrange(Rating, new)
datatable(dataOdiBatting_new)

library(ggplot2)
batsmen_growth <- dataOdiBatting_new %>%
  ggplot(aes(n, Rating, color = new)) + 
  geom_point()

batsmen_growth

