library(dplyr)
library(ggplot2)
library(reshape2)

data16 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment I\\data16.csv", nrows = 60)

tail(data16)

melted <- melt(data16, Months = c("Jan", "Feb",	"Mar", "Apr",	"May", "Jun",	"Jul",	
                                    "Aug", "Sep",	"Oct", "Nov", "Dec"))

ggplot(melted, aes(x = District, y = value, group = 1, label = value)) +
  geom_bar(aes(fill = variable), stat = "identity") +
  facet_wrap(~Sex, scales = "free") +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "LIVE BIRTHS BY SEX AND MONTH OF OCCURRENCE - 2016") +
  ylab("Live births")


data16_2 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment I\\data16.csv", skip = 64, nrows = 6)

data16_2 <- Filter(function(x)! all(is.na(x)), data16_2)

melted <- melt(data16_2, Institution = c("Institution", "Area"))

ggplot(melted, aes(x = Institution, y = value, group = 1, label = value)) +
  geom_bar(aes(fill = Area), stat = "identity") +
  facet_wrap(~variable, scales = "free") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  ylab("Number of cases") + xlab("Institutuion Type") +
  labs(title = "Birth types(2016)")

data16_3 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment I\\data16.csv", skip = 73, nrows = 18)
data16_3 <- Filter(function(x)! all(is.na(x)), data16_3)

melted1 <- melt(data16_3, BirtOrder = c("EducationLevelFather", "Area"))

ggplot(melted1, aes(x = EducationLevelFather, y = value, label = value)) +
  geom_bar(aes(fill = Area), stat = "identity", position = "dodge") +
  facet_wrap(~variable, scale = "free") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(title = "LIVE BIRTHS BY LEVEL OF EDUCATION OF THE FATHER  & BIRTH ORDER
(RURAL & URBAN) - 2016")
