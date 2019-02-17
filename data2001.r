library(dplyr)
library(ggplot2)


data2001 = read.csv("D:\\Vishal\\III year\\Data Analytics\\Assignment I\\CRS-2001.csv",
                    nrows = 112)

data2001 <- Filter(function(x)!all(is.na(x)), data2001)

#tail(data2001)

#plot(data2001$Male, data2001$Female)

summary(data2001)

cor(data2001$Male, data2001$Female)

without_state <- data2001 %>%
  filter(District != "State")

only_male <- data2001 %>%
  filter(District != "State", Place == "TOTAL")


urban_male <- data2001 %>%
  filter(Place == "URBAN", District != "State")

rural_male <- data2001 %>%
  filter(Place == "RURAL", District != "State")


ggplot(urban_male, aes(x = District, y = urban_male$Male)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(rural_male, aes(x = District, y = rural_male$Male)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(only_male, aes(x = District, y = only_male$Male)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

counts <- c("Within_the_area", "Outside_the_area") 


ggplot(only_male, aes(fill = only_male$Outside_the_state, x = District, y = only_male$Total)) +
  
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

#barplot(as.matrix(counts))

#barplot(counts)


#urban_male$Male