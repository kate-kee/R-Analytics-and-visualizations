library(forecast) 
library(leaps)
library(ggplot2)

# open BostonHousing.csv
BostonHousing.df <- read.csv("BostonHousing.csv")
View(BostonHousing.df)

set.seed(1)
numberOfRows <- nrow(BostonHousing.df)
train.index <- sample(numberOfRows, numberOfRows*0.7)

train.df <- BostonHousing.df[train.index, -c(14)] 
validation.df <- BostonHousing.df[-train.index,-c(14)]
View(train.df)

housePrice1.lm <- lm(formula = MEDV ~ RM, data = train.df)
housePrice2.lm <- lm(formula = MEDV ~ CRIM, data = train.df)
housePrice3.lm <- lm(formula = MEDV ~ (RM + CRIM + CHAS), data = train.df)


options(scipen = TRUE)
summary(housePrice1.lm)
summary(housePrice2.lm)
summary(housePrice3.lm)

# use accuracy() from forecast package to compute common accuracy measures including rmse.
# From help file (??accuracy) the measures calculated are:
#  ME: Mean Error
#  RMSE: Root Mean Squared Error
#  MAE: Mean Absolute Error
#  MPE: Mean Percentage Error
#  MAPE: Mean Absolute Percentage Error
#  MASE: Mean Absolute Scaled Error
#options(scipen=999, digits = 3)
housePrice1.pred <- predict(housePrice1.lm, validation.df)
accuracy(housePrice1.pred, validation.df$MEDV)

housePrice2.pred <- predict(housePrice2.lm, validation.df)
accuracy(housePrice2.pred, validation.df$MEDV)

housePrice3.pred <- predict(housePrice3.lm, validation.df)
accuracy(housePrice3.pred, validation.df$MEDV)


#calculate rmse by hand to show how it works.  this will typically be done using accuracy()
residuals <- validation.df$MEDV - housePrice3.pred
squaredResiduals <- residuals*residuals
df <- data.frame("Predicted" = housePrice3.pred, "Actual" = validation.df$MEDV,
                 "Residual" = residuals, "Squared Residuals" = residuals*residuals)
rmse <- sqrt(mean(df$Squared.Residuals))
View(df)
rmse


housePrice.lm <- lm(formula = MEDV ~ ., data = train.df)
summary(housePrice.lm)
housePriceAll.pred <- predict(housePrice.lm, validation.df)
accuracy(housePriceAll.pred, validation.df$MEDV)




# Choose a model by AIC in a Stepwise Algorithm
# Forward regression Start with no predictors add them one by one (add the one with largest contribution based on AIC)
# Stop when the addition is not statistically significant to the model
housePrice.lm <- lm(formula = MEDV ~ ., data = train.df)
housePrice.lm.fwd <- step(housePrice.lm, direction = "forward", trace = 1)

summary(housePrice.lm.fwd)
housePrice.lm.fwd.pred <- predict(housePrice.lm.fwd, validation.df)
accuracy(housePrice.lm.fwd.pred, validation.df$MEDV)

all.residuals <- validation.df$MEDV - housePrice.lm.fwd.pred
all.residuals.df <- data.frame(all.residuals)
ggplot(all.residuals.df) + geom_histogram(aes(x = all.residuals), binwidth = 1)

library(dplyr)
library(Hmisc)
library(ggplot2)

###plot predicted price vs target price for range of prices
df <- data.frame("Predicted" = housePrice.lm.fwd.pred, "Actual" = validation.df$MEDV)
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)
df

# %>% is a pipe operator.  Pipes let you pipe a value forward in an expression.
bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

bin_stats

##Plotting actual vs predicted values for Training and Validation data
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 

p1

# backward regression Start with all predictors and Successively eliminate least useful predictors one by one
# Stop when all remaining predictors have statistically significant contribution
housePrice.lm <- lm(formula = MEDV ~ ., data = train.df)
housePrice.lm.bwd <- step(housePrice.lm, direction = "backward")
summary(housePrice.lm.bwd)
housePrice.lm.bwd.pred <- predict(housePrice.lm.bwd, validation.df)
accuracy(housePrice.lm.bwd.pred, validation.df$MEDV)

all.residuals <- validation.df$MEDV - housePrice.lm.bwd.pred
all.residuals.df <- data.frame(all.residuals)
ggplot(all.residuals.df) + geom_histogram(aes(x = all.residuals), binwidth = 1)

library(dplyr)
library(Hmisc)
library(ggplot2)

###plot predicted price vs target price for range of prices
df <- data.frame("Predicted" = housePrice.lm.bwd.pred, "Actual" = validation.df$MEDV)
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

# stepwise regression
# Like forward selection except at each step, also consider dropping non-significant predictors
housePrice.lm <- lm(formula = MEDV ~ ., data = train.df)
housePrice.lm.step <- step(housePrice.lm, direction = "both")
summary(housePrice.lm.step)
housePrice.lm.step.pred <- predict(housePrice.lm.step, validation.df)
accuracy(housePrice.lm.step.pred, validation.df$MEDV)

all.residuals <- validation.df$MEDV - housePrice.lm.step.pred
all.residuals.df <- data.frame(all.residuals)
ggplot(all.residuals.df) + geom_histogram(aes(x = all.residuals), binwidth = 1)

library(dplyr)
library(Hmisc)
library(ggplot2)

###plot predicted price vs target price for range of prices
df <- data.frame("Predicted" = housePrice.lm.step.pred, "Actual" = validation.df$MEDV)
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

# use regsubsets() in package leaps to run an exhaustive search. 
# regsubsets() will calculate adjusted r2 for every possible combination of predictors
# unlike with lm(), categorical predictors must be turned into dummies manually.

search <- regsubsets(MEDV ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
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

#best 4
best4.lm <- lm(MEDV ~ CHAS + RM + PTRATIO + LSTAT, data = train.df)
summary(best4.lm)
best4.lm.pred <- predict(best4.lm, validation.df)
accuracy(best4.lm.pred, validation.df$MEDV)

 
#library(gains)
library(dplyr)
library(Hmisc)
library(ggplot2)

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







