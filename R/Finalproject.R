
#Cleaning data and visualizations.
library(ggplot2)
library(tidyverse) # metapackage with lots of helpful functions
library(gridExtra) # grid.arrange to make quick subplots
library(reshape2)
who<-read_csv("Life Expectancy Data.csv")
#Number of Nulls
sapply(who, function(x) sum(is.na(x)))
#Cleaning data
#For Population and GDP, we have collected data from internet and added it to excel
#For column Hepatitis_B lots of values were missing, so took average of it

replace(who$Hepatitis_B, is.na(who$Hepatitis_B), mean(who$Hepatitis_B, na.rm = TRUE))
#omitted rest of the NAs
who<-na.omit(who)
#Get Structure of data
str(who)
#Get summary of data
summary(who)
#Please add "_" in the dataset wherever there is space before column name before performing the below steps
#plot histogram of Life expectancy
ggplot(who) + geom_histogram(aes(x = Life_expectancy),boundary=0.5,
                             binwidth=10,fill="#69b3a2",col = "white")
#Visualizing data
#side by side Box Plot of life expectancy
ggplot(who, aes(x = Status, y = Life_expectancy)) + geom_boxplot(fill="#69b3a2")

#Relation between Years and life expectancy
who %>%
  ggplot()+ geom_violin(aes(x=Year, y=Life_expectancy, group=Year, fill=Year))
#Creating box plot to check the spread of the data for important attributes.
boxplot(who$Income_composition_of_resources,xlab="Income Composition")
boxplot(who$Schooling,xlab="Schooling")
boxplot(who$ Life_expectancy,xlab=" Life_expectancy")

#Plotting life expectancy against multiple variables

plot(y = who$Life_expectancy,
    x = who$Income_composition_of_resources,
    main = "Life Expectancy vs. Income compositions",
    xlab = "Income composition of resources",
    ylab = "Life Expectancy",
    pch = 19,
    col = "darkgreen")
abline(60,1,
       col = "maroon")

plot(y = who$Life_expectancy,
     x = who$Income_composition_of_resources,
     main = "Life Expectancy vs. Income compositions",
     xlab = "Income composition of resources",
     ylab = "Life Expectancy",
     pch = 19,
     col = "darkgreen")
abline(60,1,
       col = "maroon")

plot(y = who$Life_expectancy,
     x = who$Schooling,
     main = "Life Expectancy vs.Schooling",
     xlab = "Schooling",
     ylab = "Life Expectancy",
     pch = 19,
     col = "darkblue")
abline(60,1,
       col = "maroon")


plot(y = who$Life_expectancy,
     x = who$Adult_Mortality,
     main = "Life Expectancy vs. Adult Mortality",
     xlab = "Adult Mortality",
     ylab = "Life Expectancy",
     pch = 19,
     col = "yellow")
abline(60,1,
       col = "maroon")


library(forecast)
library(leaps)
library(ggplot2)
library(dplyr)
library(Hmisc)
#Model 1
Life_Expectancy_Data<-read_csv("Life Expectancy Data.csv")
Life_Expectancy <-na.omit(Life_Expectancy_Data)
# select variables for regression 
selected.var <- c(4:22)
# partition the data 
set.seed(1)  # set seed for reproducing the partition
numberOfRows <- nrow(Life_Expectancy)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- Life_Expectancy[train.index, selected.var]
valid.df <- Life_Expectancy[-train.index, selected.var]

search <- regsubsets(Life_expectancy ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)
summary(search)

# use lm() to run a linear regression of life expectancy on all 18 predictors
Life_Expectancy.lm <- lm(Life_expectancy ~ ., data = train.df)
summary(Life_Expectancy.lm)

# use predict() to make predictions on a new set. 
Life_Expectancy.lm.pred <- predict(Life_Expectancy.lm, valid.df)
summary(Life_Expectancy.lm.pred)

#calculate rmse by hand to show how it works.
residuals <- valid.df$Life_expectancy - Life_Expectancy.lm.pred
squaredResiduals <- residuals*residuals
df <- data.frame(
  "Predicted" = Life_Expectancy.lm.pred, "Actual" = valid.df$Life_expectancy,
  "Residual" = residuals, "Squared Residuals" = residuals*residuals)
rmse <- sqrt(mean(df$Squared.Residuals))
View(df)

accuracy(Life_Expectancy.lm.pred , valid.df$Life_expectancy)


###plot predicted price vs target price for range of prices
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Training and Validation data
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 
p1

library(forecast)
library(leaps)
library(ggplot2)
library(dplyr)
library(Hmisc)
#Model 2
Life_Expectancy <- car.df<-na.omit(Life_Expectancy_Data)

# select variables for regression 
selected.var <- c(4:22)
# partition the data 
set.seed(1)  # set seed for reproducing the partition
numberOfRows <- nrow(Life_Expectancy)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- Life_Expectancy[train.index, selected.var]
valid.df <- Life_Expectancy[-train.index, selected.var]

#Exhaustive Search
search <- regsubsets(Life_expectancy ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)
summary(search)

# show models
sum$which

# show metrics
sum$rsq
sum$adjr2


#par(mfrow=c(1,1))
plot(search, scale="r2")

# use lm() to run a linear regression of Price on best 5 predictors
Life_Expectancy.lm <- lm(Life_expectancy ~ Schooling + HIV_AIDS + Adult_Mortality+percentage_expenditure+Income_composition_of_resources , data = train.df)
summary(Life_Expectancy.lm)
Life_Expectancy.lm.pred <- predict(Life_Expectancy.lm, valid.df)
accuracy(Life_Expectancy.lm.pred, valid.df$Life_expectancy)

# use predict() to make predictions on a new set. 
Life_Expectancy.lm.pred <- predict(Life_Expectancy.lm, valid.df)
summary(Life_Expectancy.lm.pred)

#calculate rmse by hand to show how it works.
residuals <- valid.df$Life_expectancy - Life_Expectancy.lm.pred
squaredResiduals <- residuals*residuals
df <- data.frame(
  "Predicted" = Life_Expectancy.lm.pred, "Actual" = valid.df$Life_expectancy,
  "Residual" = residuals, "Squared Residuals" = residuals*residuals)
rmse <- sqrt(mean(df$Squared.Residuals))
View(df)


accuracy(Life_Expectancy.lm.pred , valid.df$Life_expectancy)


###plot predicted price vs target price for range of prices
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Training and Validation data
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 
p1

library(forecast)
library(leaps)
library(ggplot2)
library(dplyr)
library(Hmisc)
#Model 3- a
Life_Expectancy <-na.omit(Life_Expectancy_Data)
colnames(Life_Expectancy)[colnames(Life_Expectancy) == "Life expectancy"] <- "LifeExpectancy"
# select variables for regression 
selected.var <- c(4:22)
# partition the data 
set.seed(1)  # set seed for reproducing the partition
numberOfRows <- nrow(Life_Expectancy)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- Life_Expectancy[train.index, selected.var]
valid.df <- Life_Expectancy[-train.index, selected.var]


# use lm() to run a linear regression of Life Expectancy on single predictors
Life_Expectancy.lm <- lm(Life_expectancy ~ Schooling , data = train.df)
summary(Life_Expectancy.lm)
Life_Expectancy.lm.pred <- predict(Life_Expectancy.lm, valid.df)
accuracy(Life_Expectancy.lm.pred, valid.df$Life_expectancy)

# use predict() to make predictions on a new set. 
Life_Expectancy.lm.pred <- predict(Life_Expectancy.lm, valid.df)
summary(Life_Expectancy.lm.pred)

#calculate rmse by hand to show how it works.
residuals <- valid.df$Life_expectancy - Life_Expectancy.lm.pred
squaredResiduals <- residuals*residuals
df <- data.frame(
  "Predicted" = Life_Expectancy.lm.pred, "Actual" = valid.df$Life_expectancy,
  "Residual" = residuals, "Squared Residuals" = residuals*residuals)
rmse <- sqrt(mean(df$Squared.Residuals))
View(df)

accuracy(Life_Expectancy.lm.pred , valid.df$Life_expectancy)


###plot predicted price vs target price for range of prices
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Training and Validation data
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 
p1


library(forecast)
library(leaps)
library(ggplot2)
library(dplyr)
library(Hmisc)
#Model 3.b
Life_Expectancy <- na.omit(Life_Expectancy_Data)

# select variables for regression 
selected.var <- c(4:22)
# partition the data 
set.seed(1)  # set seed for reproducing the partition
numberOfRows <- nrow(Life_Expectancy)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- Life_Expectancy[train.index, selected.var]
valid.df <- Life_Expectancy[-train.index, selected.var]



# use lm() to run a linear regression of Price on best 5 predictors
Life_Expectancy.lm <- lm(Life_expectancy ~ HIV_AIDS  , data = train.df)
summary(Life_Expectancy.lm)
Life_Expectancy.lm.pred <- predict(Life_Expectancy.lm, valid.df)
accuracy(Life_Expectancy.lm.pred, valid.df$Life_expectancy)

# use predict() to make predictions on a new set. 
Life_Expectancy.lm.pred <- predict(Life_Expectancy.lm, valid.df)
summary(Life_Expectancy.lm.pred)

#calculate rmse by hand to show how it works.
residuals <- valid.df$Life_expectancy - Life_Expectancy.lm.pred
squaredResiduals <- residuals*residuals
df <- data.frame(
  "Predicted" = Life_Expectancy.lm.pred, "Actual" = valid.df$Life_expectancy,
  "Residual" = residuals, "Squared Residuals" = residuals*residuals)
rmse <- sqrt(mean(df$Squared.Residuals))
View(df)

accuracy(Life_Expectancy.lm.pred , valid.df$Life_expectancy)


###plot predicted price vs target price for range of prices
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Training and Validation data
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 
p1


library(forecast)
library(leaps)
library(ggplot2)
library(dplyr)
library(Hmisc)
#Model 3-c
Life_Expectancy <- car.df<-na.omit(Life_Expectancy_Data)

# select variables for regression 
selected.var <- c(4:22)
# partition the data 
set.seed(1)  # set seed for reproducing the partition
numberOfRows <- nrow(Life_Expectancy)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- Life_Expectancy[train.index, selected.var]
valid.df <- Life_Expectancy[-train.index, selected.var]


# use lm() to run a linear regression of Price on best 5 predictors
Life_Expectancy.lm <- lm(Life_expectancy ~ Adult_Mortality, data = train.df)
summary(Life_Expectancy.lm)
Life_Expectancy.lm.pred <- predict(Life_Expectancy.lm, valid.df)
accuracy(Life_Expectancy.lm.pred, valid.df$Life_expectancy)

# use predict() to make predictions on a new set. 
Life_Expectancy.lm.pred <- predict(Life_Expectancy.lm, valid.df)
summary(Life_Expectancy.lm.pred)

#calculate rmse by hand to show how it works.
residuals <- valid.df$Life_expectancy - Life_Expectancy.lm.pred
squaredResiduals <- residuals*residuals
df <- data.frame(
  "Predicted" = Life_Expectancy.lm.pred, "Actual" = valid.df$Life_expectancy,
  "Residual" = residuals, "Squared Residuals" = residuals*residuals)
rmse <- sqrt(mean(df$Squared.Residuals))
View(df)

accuracy(Life_Expectancy.lm.pred , valid.df$Life_expectancy)


###plot predicted price vs target price for range of prices
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Training and Validation data
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 
p1

library(forecast)
library(leaps)
library(ggplot2)
library(dplyr)
library(Hmisc)
#Model 3.d
Life_Expectancy <- car.df<-na.omit(Life_Expectancy_Data)


# select variables for regression 
selected.var <- c(4:22)
# partition the data 
set.seed(1)  # set seed for reproducing the partition
numberOfRows <- nrow(Life_Expectancy)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- Life_Expectancy[train.index, selected.var]
valid.df <- Life_Expectancy[-train.index, selected.var]



# use lm() to run a linear regression of Price on best 5 predictors
Life_Expectancy.lm <- lm(Life_expectancy ~ percentage_expenditure , data = train.df)
summary(Life_Expectancy.lm)
Life_Expectancy.lm.pred <- predict(Life_Expectancy.lm, valid.df)
accuracy(Life_Expectancy.lm.pred, valid.df$Life_expectancy)

# use predict() to make predictions on a new set. 
Life_Expectancy.lm.pred <- predict(Life_Expectancy.lm, valid.df)
summary(Life_Expectancy.lm.pred)

#calculate rmse by hand to show how it works.
residuals <- valid.df$Life_expectancy - Life_Expectancy.lm.pred
squaredResiduals <- residuals*residuals
df <- data.frame(
  "Predicted" = Life_Expectancy.lm.pred, "Actual" = valid.df$Life_expectancy,
  "Residual" = residuals, "Squared Residuals" = residuals*residuals)
rmse <- sqrt(mean(df$Squared.Residuals))
View(df)

accuracy(Life_Expectancy.lm.pred , valid.df$Life_expectancy)


###plot predicted price vs target price for range of prices
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Training and Validation data
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 
p1

# Random Forest Model 4
# Librarires
library(randomForest)
require(caTools)
library(mice)
library(VIM)

# Import Data
data <- read.csv('Life Expectancy Data.csv')
view(data)

# Checking Missing Data
colSums(is.na(data))
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)
md.pattern(data)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#Imputing Data using MICE package
tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500)
completedData <- complete(tempData,1)
colSums(is.na(completedData))

# Converting categorical variable to numeric
completedData$Country = as.numeric(as.factor(completedData$Country))
completedData$Status = as.numeric(as.factor(completedData$Status))
completedData$Year = as.numeric(completedData$Year)
completedData$Adult_Mortality = as.numeric(completedData$Adult_Mortality)
completedData$infant_deaths = as.numeric(completedData$infant_deaths)
completedData$Hepatitis_B = as.numeric(completedData$Hepatitis_B)
completedData$Measles = as.numeric(completedData$Measles)
completedData$underfive.deaths = as.numeric(completedData$underfive.deaths)
completedData$Polio = as.numeric(completedData$Polio)
completedData$Diphtheria = as.numeric(completedData$Diphtheria)
completedData$Population = as.numeric(completedData$Population)
as.data.frame(table(completedData$Status))
colSums(is.na(completedData))
View(completedData)

#Summary of Dataset
summary(completedData)
completedData = na.omit(completedData)
colSums(is.na(completedData))
sapply(completedData, class)

#Random forest Implementation
dim(completedData)

#Splitting data to test and train
sample = sample.split(completedData, SplitRatio = .75)
train = subset(completedData, sample == TRUE)
test  = subset(completedData, sample == FALSE)
dim(train)
dim(test)

# we initialize an instance of the randomForest class
View(completedData)
colSums(is.na(completedData))
summary(completedData)
RandomForest_Model= randomForest(Life_expectancy ~ ., 
                                 data=train, ntree=500, mtry=2, importance=TRUE)
RandomForest_Model
plot(RandomForest_Model)
pred = predict(rf, test)

plot(pred, col="blue")
graph <- data.frame(pred , test$Life_expectancy)
plot(graph)

