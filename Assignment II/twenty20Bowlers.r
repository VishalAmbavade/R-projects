twenty20bowling2007 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2007twenty20bowlingrating.csv")
twenty20bowling2008 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2008twenty20bowlingrating.csv")
twenty20bowling2009 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2009twenty20bowlingrating.csv")
twenty20bowling2010 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2010twenty20bowlingrating.csv")
twenty20bowling2011 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2011twenty20bowlingrating.csv")
twenty20bowling2012 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2012twenty20bowlingrating.csv")
twenty20bowling2013 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2013twenty20bowlingrating.csv")
twenty20bowling2014 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2014twenty20bowlingrating.csv")
twenty20bowling2015 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2015twenty20bowlingrating.csv")
twenty20bowling2016 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2016twenty20bowlingrating.csv")

library(dplyr)
dataTwenty20Bowling <- bind_rows(twenty20bowling2007, twenty20bowling2008, twenty20bowling2009, twenty20bowling2010, twenty20bowling2011,
                             twenty20bowling2012, twenty20bowling2013, twenty20bowling2014, twenty20bowling2015, twenty20bowling2016)

summary(dataTwenty20Bowling)

library(ggplot2)

dataTwenty20Bowling <- dataTwenty20Bowling %>%
  group_by(Name) %>%
  summarise(avg = mean(Rating))

set.seed(20)

ballcluster <- kmeans(dataTwenty20Bowling[, 2], 5)

ballcluster$cluster <- as.factor(ballcluster$cluster)

ggplot(dataTwenty20Bowling, aes(dataTwenty20Bowling$Name, avg, color = ballcluster$cluster)) +
  geom_point(size = 2) +
  scale_color_manual(labels = c("Good", "Best", "Useless", "Better", "Average"), values = c("blue", "red", "green", "cyan", "black")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Twenty20 Bowling Ratings(2007-2016)")

dat <- arrange(dataTwenty20Bowling, desc(avg)) %>%
  mutate(rank = 1:nrow(dataTwenty20Bowling))

dataTwenty20Bowling <- merge(dataTwenty20Bowling, dat, by  = "Name") 

library(plotly)

p <- plot_ly(dataTwenty20Bowling, x = ~Name, y = ~avg.x, type = 'scatter', 
             mode = 'markers', color = ballcluster$cluster, 
             text = ~paste('Rank: ', rank))

p
