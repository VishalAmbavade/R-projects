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

library(dplyr)
dataOdiBatting <- bind_rows(odibatting2007, odibatting2008, odibatting2009, odibatting2010,
                            odibatting2011, odibatting2012, odibatting2013, odibatting2014,
                            odibatting2015, odibatting2016)


summary(dataOdiBatting)

library(VIM)
aggr(dataOdiBatting)

dataOdiBatting <- dataOdiBatting %>%
  group_by(Name) %>%
  summarise(avg = mean(Rating))

set.seed(20)

batcluster <- kmeans(dataOdiBatting[, 2], 5)

batcluster$cluster <- as.factor(batcluster$cluster)

str(batcluster)

library(ggplot2)

ggplot(dataOdiBatting, aes(dataOdiBatting$Name, avg, color = batcluster$cluster)) +
  geom_point(size = 2) +
  scale_color_hue(labels = c("Good", "Best", "Useless", "Better", "Average")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("ODI Batting Ratings(2007-2016)")


dat <- arrange(dataOdiBatting, desc(avg)) %>%
  mutate(rank = 1:nrow(dataOdiBatting))

dataOdiBatting <- merge(dataOdiBatting, dat, by  = "Name") 
dataOdiBatting
library(plotly)

p <- plot_ly(dataOdiBatting, x = ~Name, y = ~avg.x, type = 'scatter', 
             mode = 'markers', color = batcluster$cluster, 
             text = ~paste('Rank: ', rank))

p

p <- plot_ly(dataOdiBatting, x = ~Name, y = ~avg.x, type = 'scatter', mode = 'markers', name = 'G1') %>%
  add_trace(y = ~avg.x, name = 'Tree 2') %>%
  add_trace(y = ~avg.x, name = 'Tree 3') %>%
  add_trace(y = ~avg.x, name = 'Tree 4') %>%
  add_trace(y = ~avg.x, name = 'Tree 5')
p

dat

#Classification 

library(party)

new_dat <- sample(2, nrow(dat), replace = TRUE, prob = c(0.7, 0.3))
train_data <- dat[new_dat == 1, ]
test_data <- dat[new_dat == 2, ]

myf <- avg~ rank
tree <- ctree(myf, data = train_data)
table(predict(tree), train_data$avg)
plot(tree)

test_tree <- ctree(myf, data = test_data)
plot(test_tree)
