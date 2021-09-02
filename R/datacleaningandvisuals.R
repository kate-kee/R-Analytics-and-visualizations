library(ggplot2)
library(tidyverse) # metapackage with lots of helpful functions
library(gridExtra) # grid.arrange to make quick subplots
library(reshape2)
who<-read_csv("Life Expectancy Data.csv")
#Number of Nulls

#Cleaning data
replace(who$Hepatitis_B, is.na(who$Hepatitis_B), mean(who$Hepatitis_B, na.rm = TRUE))
who<-na.omit(who)

ggplot(who) + geom_histogram(aes(x = Life_expectancy),boundary=0.5,
                                 binwidth=10,fill="#69b3a2",col = "white")
#Visualising data
#side by side Box Plot
ggplot(who, aes(x = Status, y = Life_expectancy)) + geom_boxplot(fill="#69b3a2")

# any correlation between vaccinations?
cordf <- who %>%
  select(Hepatitis_B, Polio, Measles, HIV_AIDS, Diphtheria)
cormat <- cor(cordf)
melted <- melt(cormat)
ggplot(melted)+
  geom_tile(aes(Var1, Var2, fill=value))
#Relation between Years and life expectancy
who %>%
  ggplot()+ geom_violin(aes(x=Year, y=Life_expectancy, group=Year, fill=Year))


