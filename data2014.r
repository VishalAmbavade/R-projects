library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(ggpubr)


data14 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment I\\data14.csv", nrows = 60)


tail(data14)
data14 <- Filter(function(x)!all(is.na(x)), data14)

p1 <- ggplot(data14, aes(x = District, y = Jan)) +
  geom_bar(aes(fill = Sex), stat="identity", position ="dodge") + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=-60, hjust=.1))

p2 <- ggplot(data14, aes(x = District, y = Feb)) +
  geom_bar(aes(fill = Sex), stat="identity", position ="dodge") + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=-60, hjust=.1))

p3 <- ggplot(data14, aes(x = District, y = Mar)) +
  geom_bar(aes(fill = Sex), stat="identity", position ="dodge") + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=-60, hjust=.1))

p4 <- ggplot(data14, aes(x = District, y = Apr)) +
  geom_bar(aes(fill = Sex), stat="identity", position ="dodge") + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=-60, hjust=.1))

#ggplot(data14, aes(x=as.numeric(month), y=value, color=variable)) + geom_line()

#grid.arrange(p1, p2, p3, p4)
ggarrange(p1, p2, p3, p4, 
          labels = c("Jan", "Feb", "Mar", "Apr"),
          ncol = 2, nrow = 2)

data14_2 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment I\\data14.csv", skip = 65, header = TRUE, nrows = 27)
tail(data14_2)

data14_2_urban <- data14_2 %>%
  filter(Area == "Urban")

data14_2_rural <- data14_2 %>%
  filter(Area == "Rural")

data14_2_urban$Age.of.Mother

ggplot(data14_2_urban, aes(x = data14_2_urban$Age.of.Mother, group = 1)) +
  geom_line(aes(y = data14_2_urban$X1, colour = "Mothers in urban area")) +
  geom_line(aes(y = data14_2_rural$X1, colour = "Mothers in rural area")) +
  theme(axis.text.x = element_text(angle=-60, hjust=.1)) +
  scale_color_manual(values=c("#CC6666", "#9999CC")) +
  ylab("Live births") + xlab("Age of Mother") +
  labs(title = "First order LIVE BIRTHS BY AGE OF THE MOTHER & BIRTH ORDER  (RURAL & URBAN) - 2014")

ggplot(data14_2, aes(x = Age.of.Mother, y = X1)) +
  geom_boxplot()  +
  labs(title = "Mother's age vs. First Order Birth")


data14_3 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment I\\data14.csv", skip = 95, header = TRUE, nrows = 18)

head(data14_3)

data14_3_filter <- data14_3 %>%
  filter(Area != "All")

p31 <- ggplot(data14_3_filter, aes(x = Level.of.Education.of.Father, y = BO1)) +
  geom_bar(aes(fill = Area), stat="identity", position ="dodge") + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=-60, hjust=.1)) +
  xlab("Education") + ylab("No. of people") +
  labs(title = "LIVE BIRTHS BY LEVEL OF EDUCATION OF THE FATHER & BIRTH ORDER")

p31

