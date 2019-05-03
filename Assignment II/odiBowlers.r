odibowling2007 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2007odibowlingrating.csv")
odibowling2008 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2008odibowlingrating.csv")
odibowling2009 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2009odibowlingrating.csv")
odibowling2010 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2010odibowlingrating.csv")
odibowling2011 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2011odibowlingrating.csv")
odibowling2012 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2012odibowlingrating.csv")
odibowling2013 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2013odibowlingrating.csv")
odibowling2014 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2014odibowlingrating.csv")
odibowling2015 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2015odibowlingrating.csv")
odibowling2016 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2016odibowlingrating.csv")

library(dplyr)
dataOdiBowling <- bind_rows(odibowling2007, odibowling2008, odibowling2009, odibowling2010, odibowling2011,
                            odibowling2012, odibowling2013, odibowling2014, odibowling2015, odibowling2016)

summary(dataOdiBowling)

library(ggplot2)

dataOdiBowling <- dataOdiBowling %>%
  group_by(Name) %>%
  summarise(avg = mean(Rating))

set.seed(20)

ballcluster <- kmeans(dataOdiBowling[, 2], 5)

ballcluster$cluster <- as.factor(ballcluster$cluster)

ggplot(dataOdiBowling, aes(dataOdiBowling$Name, avg, color = ballcluster$cluster)) +
  geom_point(size = 2) +
  scale_color_manual(labels = c("Good", "Best", "Useless", "Better", "Average"), values = c("blue", "red", "green", "cyan", "black")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("ODI Bowling Ratings(2007-2016)")

dat <- arrange(dataOdiBowling, desc(avg)) %>%
  mutate(rank = 1:nrow(dataOdiBowling))

dataOdiBowling <- merge(dataOdiBowling, dat, by  = "Name") 

library(plotly)

p <- plot_ly(dataOdiBowling, x = ~Name, y = ~avg.x, type = 'scatter', 
             mode = 'markers', color = ballcluster$cluster, 
             text = ~paste('Rank: ', rank))

p

dat
