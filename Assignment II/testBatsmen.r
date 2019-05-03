testbatting2007 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2007testbattingrating.csv")
testbatting2008 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2008testbattingrating.csv")
testbatting2009 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2009testbattingrating.csv")
testbatting2010 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2010testbattingrating.csv")
testbatting2011 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2011testbattingrating.csv")
testbatting2012 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2012testbattingrating.csv")
testbatting2013 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2013testbattingrating.csv")
testbatting2014 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2014testbattingrating.csv")
testbatting2015 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2015testbattingrating.csv")
testbatting2016 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2016testbattingrating.csv")

library(dplyr)
dataTestBatting <- bind_rows(testbatting2007, testbatting2008, testbatting2009, testbatting2010,
                            testbatting2011, testbatting2012, testbatting2013, testbatting2014,
                            testbatting2015, testbatting2016)

summary(dataTestBatting)

library(VIM)
aggr(dataTestBatting)

dataTestBatting <- dataTestBatting %>%
  group_by(Name) %>%
  summarise(avg = mean(Rating))

set.seed(20)

batcluster <- kmeans(dataTestBatting[, 2], 5)

batcluster$cluster <- as.factor(batcluster$cluster)


library(DT)
library(ggplot2)

ggplot(dataTestBatting, aes(dataTestBatting$Name, avg, color = batcluster$cluster)) +
  geom_point(size = 2) +
  scale_color_manual(labels = c("Good", "Best", "Useless", "Better", "Average"), values = c("blue", "red", "green", "magenta", "black")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Test Batting Ratings(2007-2016)")

dat <- arrange(dataTestBatting, desc(avg)) %>%
  mutate(rank = 1:nrow(dataTestBatting))

dataTestBatting <- merge(dataTestBatting, dat, by  = "Name") 
dataTestBatting
library(plotly)

p <- plot_ly(dataTestBatting, x = ~Name, y = ~avg.x, type = 'scatter', 
             mode = 'markers', color = batcluster$cluster, 
             text = ~paste('Rank: ', rank))

p
