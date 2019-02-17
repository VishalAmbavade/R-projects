library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)

dataAll <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment I\\AllYears.csv")

tail(dataAll)

meltedData <- melt(dataAll)

filter11 <- dataAll %>%
  filter(Year == 2011)

filter14 <- dataAll %>%
  filter(Year == 2014)

filter15 <- dataAll %>%
  filter(Year == 2015)

filter16 <- dataAll %>%
  filter(Year == 2016)

melt11 <- melt(filter11)
melt14 <- melt(filter14)
melt15 <- melt(filter15)
melt16 <- melt(filter16)

melt11_f <- melt11 %>%
  filter(value != 2011) 

melt14_f <- melt14 %>%
  filter(value != 2014)

melt15_f <- melt15 %>%
  filter(value != 2015)

melt16_f <- melt16 %>%
  filter(value != 2016)

y1 <- ggplot(melt11_f, aes(x = MethodofDelivery, y = value, label = value)) +
  geom_bar(aes(fill = variable), stat = "identity")+
  facet_wrap(~Area) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "LIVE BIRTHS BY AGE OF MOTHER AND METHOD OF DELIVERY  (RURAL & URBAN) -  2011")

y2 <- ggplot(melt14_f, aes(x = MethodofDelivery, y = value, label = value)) +
  geom_bar(aes(fill = variable), stat = "identity")+
  facet_wrap(~Area) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "LIVE BIRTHS BY AGE OF MOTHER AND METHOD OF DELIVERY  (RURAL & URBAN) -  2014")

y3 <- ggplot(melt15_f, aes(x = MethodofDelivery, y = value, label = value)) +
  geom_bar(aes(fill = variable), stat = "identity")+
  facet_wrap(~Area) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "LIVE BIRTHS BY AGE OF MOTHER AND METHOD OF DELIVERY  (RURAL & URBAN) -  2015")

y4 <- ggplot(melt16_f, aes(x = MethodofDelivery, y = value, label = value)) +
  geom_bar(aes(fill = variable), stat = "identity")+
  facet_wrap(~Area) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "LIVE BIRTHS BY AGE OF MOTHER AND METHOD OF DELIVERY  (RURAL & URBAN) -  2016")


ggarrange(y1, y2, y3, y4, 
          labels = c("2011", "2014", "2015", "2016"),
          ncol = 2, nrow = 2)
