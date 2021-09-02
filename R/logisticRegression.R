library(gains)
library(caret)
library(ROCR)
 
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]                          # Drop ID and zip code columns.
View(bank.df)

# treat Education as categorical (R will create dummy variables automatically)
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))

bank.df$EducationNumber <- as.numeric(bank.df$Education)
View(bank.df)

# partition data
set.seed(2)
numberOfRows <- nrow(bank.df)
train.index <- sample(numberOfRows, numberOfRows*0.6)  
train.df <- bank.df[train.index,]
valid.df <- bank.df[-train.index,]

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logitI.reg <- glm(Personal.Loan ~ Income, data = train.df, family = "binomial") 
options(scipen=999)
summary(logitI.reg)
confusionMatrix(table(predict(logitI.reg, newdata = valid.df, 
                              type="response") >= 0.1, valid.df$Personal.Loan == 1))


logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df[, -8], type = "response") 

# first 5 actual and predicted records
data.frame(actual = valid.df$Personal.Loan[1:10], predicted = logit.reg.pred[1:10])
confusionMatrix(table(predict(logit.reg, newdata = valid.df, type="response") >= 0.5, valid.df$Personal.Loan == 1))


gain <- gains(valid.df$Personal.Loan, logit.reg.pred, groups=10)

# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$Personal.Loan))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$Personal.Loan))~c(0, dim(valid.df)[1]), lty=2)


# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(valid.df$Personal.Loan)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

# add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

export.df <- data.frame(valid.df, logit.reg.pred)
t.df <- data.frame("Predicted" = logit.reg.pred, "Label" = as.factor(valid.df$Personal.Loan))
View(t.df)

write.csv(export.df, file = "UBpropensities.csv")
 
pred <- prediction(t.df$Predicted, t.df$Label)
perf <- performance( pred, "tpr", "fpr" )
plot( perf )


