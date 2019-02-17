library(dplyr)
library(ggplot2)
library(reshape2)

data15 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment I\\data15.csv", nrows = 30)
data15 <- Filter(function(x)!all(is.na(x)), data15)
tail(data15)

col <- data15 %>%
  select(RegisteredBirth, BirthRate, RegisteredDeath, DeathRate, RegisteredInfantDeath,
         RegisteredStillBirth, StillBirthRate)

d <- colnames(col)

ggplot(data15, aes(x = District)) +
  geom_bar(aes(y = RegisteredBirth), stat="identity", position ="dodge") + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=-60, hjust=.1)) +
  labs(title = "Registered Births by district")

cor(data15$BirthRate, data15$DeathRate)

ggplot(data15, aes(x = District)) +
  geom_bar(aes(y = RegisteredInfantDeath), stat="identity", position ="dodge") + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=-60, hjust=.1)) +
  labs(title = "Registered Infant Death by district")

data15_2 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment I\\data15.csv", skip = 36, nrows = 60)

head(data15_2)

p1 <- ggplot(data15_2, aes(x = District, y = Jan)) +
  geom_bar(aes(fill = Sex), stat="identity", position ="dodge") + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=-60, hjust = .1))
p1

data15_3 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment I\\data15.csv", skip = 108, nrows = 6, header = TRUE)

data15_3 <- Filter(function(x)! all(is.na(x)), data15_3)

melted <- melt(data15_3, Institution = c("Institution", "Area"))

ggplot(melted, aes(x = Institution, y = value, group = 1, label = value)) +
  geom_bar(aes(fill = Area), stat = "identity") +
  facet_wrap(~variable, scales = "free") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  ylab("Number of cases") + xlab("Institutuion Type") +
  labs(title = "Birth types")

