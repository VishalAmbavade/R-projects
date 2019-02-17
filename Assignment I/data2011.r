library(dplyr)
library(ggplot2)

data2011 = read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment I\\CRS-2011.csv",
                    nrows = 41)

data2011 <- Filter(function(x)!all(is.na(x)), data2011)

summary(data2011)
head(data2011)
sd(data2011$Live_births)
cor(data2011$Live_births, data2011$Deaths)
tail(data2011)

ggplot(data2011, aes(x = Year, y = Live_births)) +
  geom_line() +
  labs(title = "Live births by year")

ggplot(data2011, aes(fill = Deaths, x = Year, y = Live_births)) +
  geom_bar(stat = "identity") +
  labs(title = "Live births and deaths per year")

ggplot(data2011, aes(fill = Percentage_Births, y = Percentage_Births, 
                     Percentage_Deaths, x=Year)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Percentage births per year")


ggplot(data2011, aes(Year, y = Still_births, Deaths)) +
  geom_boxplot() +
  labs(title = "Still births per year")


ggplot(data2011, aes(Year)) + 
  geom_line(aes(y = Deaths, colour = "Deaths")) + 
  geom_line(aes(y = Live_births, colour = "Live births")) +
  labs(title = "Comparison between Live births and deaths by year")

ggplot(data2011, aes(x = Year, y = Deaths)) +
  geom_point()

data2011_2 <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment I\\CRS-2011.csv", skip=44, nrows=30,
                       header = TRUE)

data2011_2 <- Filter(function(x)!all(is.na(x)), data2011_2)
#head(data2011_2)

tb2 <- data2011_2 %>%
  filter(District != "STATE")

ggplot(tb2, aes(x = District, y = Births_Registered)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Births registered by district")

ggplot(tb2, aes(x = District, y = Birth_Rate)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Districtwise birth rate")

popTable <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment I\\CRS-2011.csv", skip=158, nrows=30,
                     header = TRUE)
popTable <- Filter(function(x)!all(is.na(x)), popTable)

summary(popTable)

head(popTable)

ggplot(popTable, aes(x = Districts, y = Actual_population)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Districtwise population(Actual)")

ggplot(popTable, aes(x = Districts, y = Incomplete_returns)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Incomplete returns per district")

liveBirthsReg <- read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment I\\CRS-2011.csv", skip=710, nrows=30,
                     header = TRUE)

liveBirthsReg <- Filter(function(x)!all(is.na(x)), liveBirthsReg)

tail(liveBirthsReg)

summary(liveBirthsReg)

cor(liveBirthsReg$RuralWithinPrescribedTimeLimitMale, liveBirthsReg$RuralWithinPrescribedTimeLimitFemale)

ggplot(liveBirthsReg, aes(x = District, group = 1)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_point(aes(x= District, y = RuralWithinPrescribedTimeLimitMale)) +
  geom_point(aes(y = UrbanWithinPrescribedTimeLimitMale)) +
  geom_line(aes(y = RuralWithinPrescribedTimeLimitMale, colour = "Male reg. in prescribed Time limit(Rural)")) + 
  geom_line(aes(y = UrbanWithinPrescribedTimeLimitMale, colour = "Male reg. in prescribed Time limit(Urban)")) +
  labs(title = "Comparison between on time reg(male) in Rural and Urban areas") 

