testbowling2007 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2007testbowlingrating.csv")
testbowling2008 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2008testbowlingrating.csv")
testbowling2009 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2009testbowlingrating.csv")
testbowling2010 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2010testbowlingrating.csv")
testbowling2011 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2011testbowlingrating.csv")
testbowling2012 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2012testbowlingrating.csv")
testbowling2013 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2013testbowlingrating.csv")
testbowling2014 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2014testbowlingrating.csv")
testbowling2015 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2015testbowlingrating.csv")
testbowling2016 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment II\\Player Ratings\\2016testbowlingrating.csv")

library(dplyr)
dataTestBowling <- bind_rows(testbowling2007, testbowling2008, testbowling2009, testbowling2010, testbowling2011,
                            testbowling2012, testbowling2013, testbowling2014, testbowling2015, testbowling2016)

summary(dataTestBowling)

library(ggplot2)

dataTestBowling <- dataTestBowling %>%
  group_by(Name) %>%
  summarise(avg = mean(Rating))

set.seed(20)

ballcluster <- kmeans(dataTestBowling[, 2], 5)

ballcluster$cluster <- as.factor(ballcluster$cluster)


ggplot(dataTestBowling, aes(dataTestBowling$Name, avg, color = ballcluster$cluster)) +
  geom_point(size = 2) +
  scale_color_manual(labels = c("Good", "Best", "Useless", "Better", "Average"), values = c("blue", "red", "green", "cyan", "black")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Test Bowling Ratings(2007-2016)")

dat <- arrange(dataTestBowling, desc(avg)) %>%
  mutate(rank = 1:nrow(dataTestBowling))

dataTestBowling <- merge(dataTestBowling, dat, by  = "Name") 

library(plotly)

p <- plot_ly(dataTestBowling, x = ~Name, y = ~avg.x, type = 'scatter', 
             mode = 'markers', color = ballcluster$cluster, 
             text = ~paste('Rank: ', rank))

p


dat <- dataTestBowling[, -1]

norm <- function(x) {((x - min(x))/ (max(x) - min(x)))}
dat_norm <- as.data.frame(lapply(dat, norm))
dat_norm


dat_train <- dat[1:183, ]
dat_test <- dat[184:262, ]

dat_norm

library(class)
dat_pred <- knn(dat_train, dat_test, dataTestBowling[1:183, 2], k = 3)
table(dat_pred, dat[184:262, 2])


plot(dat_pred)
