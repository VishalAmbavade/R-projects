odibatting2007 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2007odibattingrating.csv")
odibatting2008 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2008odibattingrating.csv")
odibatting2009 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2009odibattingrating.csv")
odibatting2010 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2010odibattingrating.csv")
odibatting2011 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2011odibattingrating.csv")
odibatting2012 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2012odibattingrating.csv")
odibatting2013 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2013odibattingrating.csv")
odibatting2014 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2014odibattingrating.csv")
odibatting2015 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2015odibattingrating.csv")
odibatting2016 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2016odibattingrating.csv")

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
library(ggplot2)
library(plotly)
allData <- bind_rows(odibatting2007, odibatting2008, odibatting2009, odibatting2010,
                            odibatting2011, odibatting2012, odibatting2013, odibatting2014,
                            odibatting2015, odibatting2016,
                            testbatting2007, testbatting2008, testbatting2009, testbatting2010,
                            testbatting2011, testbatting2012, testbatting2013, testbatting2014,
                            testbatting2015, testbatting2016,
                            twenty20batting2007, twenty20batting2008, twenty20batting2009,
                            twenty20batting2010, twenty20batting2011, twenty20batting2012,
                            twenty20batting2013, twenty20batting2014, twenty20batting2015,
                            twenty20batting2016)

allData <- na.omit(allData)
library(VIM)
aggr(allData)

allData <- allData %>%
  group_by(Name) %>%
  summarise(avg = mean(Rating))

set.seed(20)

allDataCluster <- kmeans(allData[, 2], 5)

allDataCluster$cluster <- as.factor(allDataCluster$cluster)

str(allDataCluster)


ggplot(allData, aes(Name, avg, color = allDataCluster$cluster)) +
  geom_point(size = 2) +
  scale_color_hue(labels = c("Good", "Best", "Useless", "Better", "Average")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("ODI Batting Ratings(2007-2016)")

dat <- arrange(allData, desc(avg)) %>%
  mutate(rank = 1:nrow(allData))

allData <- merge(allData, dat, by  = "Name") 

p <- plot_ly(allData, x = ~Name, y = ~avg.x, type = 'scatter', 
             mode = 'markers', color = allDataCluster$cluster, 
             text = ~paste('Rank: ', rank))

p
