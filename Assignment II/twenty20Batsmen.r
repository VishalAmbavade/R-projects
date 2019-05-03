twenty20batting2007 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2007twenty20battingrating.csv")
twenty20batting2008 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2008twenty20battingrating.csv")
twenty20batting2009 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2009twenty20battingrating.csv")
twenty20batting2010 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2010twenty20battingrating.csv")
twenty20batting2011 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2011twenty20battingrating.csv")
twenty20batting2012 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2012twenty20battingrating.csv")
twenty20batting2013 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2013twenty20battingrating.csv")
twenty20batting2014 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2014twenty20battingrating.csv")
twenty20batting2015 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2015twenty20battingrating.csv")
twenty20batting2016 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2016twenty20battingrating.csv")

library(dplyr)
dataTwenty20Batting <- bind_rows(twenty20batting2007, twenty20batting2008, twenty20batting2009, twenty20batting2010,
                             twenty20batting2011, twenty20batting2012, twenty20batting2013, twenty20batting2014,
                             twenty20batting2015, twenty20batting2016)

summary(dataTwenty20Batting)

library(VIM)
aggr(dataTwenty20Batting)

dataTwenty20Batting <- dataTwenty20Batting %>%
  group_by(Name) %>%
  summarise(avg = mean(Rating))

set.seed(20)

batcluster <- kmeans(dataTwenty20Batting[, 2], 5)

batcluster$cluster <- as.factor(batcluster$cluster)
library(ggplot2)

library(DT)

ggplot(dataTwenty20Batting, aes(dataTwenty20Batting$Name, avg, color = batcluster$cluster)) +
  geom_point(size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Twenty20 Batting Ratings(2007-2016)")

dat <- arrange(dataTwenty20Batting, desc(avg)) %>%
  mutate(rank = 1:nrow(dataTwenty20Batting))

dataTwenty20Batting <- merge(dataTwenty20Batting, dat, by  = "Name") 
dataTwenty20Batting
library(plotly)

p <- plot_ly(dataTwenty20Batting, x = ~Name, y = ~avg.x, type = 'scatter', 
             mode = 'markers', color = batcluster$cluster, 
             text = ~paste('Rank: ', rank))

p
