library(readxl)
netflix.df <- read_excel("Netflix.xlsx")
View(netflix.df)
str(netflix.df)

#show how to coerce read_excel to a data frame (not needed but good to know)
netflix.df <- as.data.frame(read_excel("Netflix.xlsx"))
View(netflix.df)
str(netflix.df)

#add customerID as a row label store row names and col names in a vector for later use
row.names <- netflix.df[,1]
str(row.names)
col.names <- names(netflix.df)
str(col.names)
print(row.names)
print(col.names)

#two mechanisms for calculating collaborative filter: by hand and using recommender lab package
#for each approach, we can calculate item based (distance and/or correlations) and 
#user based (distance and/or correlation). Goal is to find the closest item(s) and/or user(s)
#we recommend the closest items if using item based. User based requires an additional step.
#for user based, we find the closest (most similar) user and identify the highest ranked items
#for that user that the current user has not watched (purchased). These are the items we recommend

#calculate correlation between items (movies) to get item based recommendations. remove first column as it is customerID
ibcf <- cor(netflix.df[,2:10], method = c("pearson"), use = c("pairwise.complete.obs"))
print(ibcf)

netflixt.df <- t(netflix.df[,2:10])
View(netflixt.df)

#calculate distance between items (movies) to get item based recommendations. remove first column as it is customerID
dist(t(netflix.df[,2:10]), method = "euclidean", diag = TRUE)




#can do the same with recommenderlab package
#load recommendar lab library to calculate user based collaborative filter
library(recommenderlab)
m <- as.matrix(netflix.df[2:10])
str(m)
r <- as(m, "realRatingMatrix")
str(r)


#calculate simialrities between and among users for user based collaborative filter
user_sim <- similarity(r, method = "pearson", which = "user")
print(user_sim)

# user-rating based recommendation for user 4
# will compute the predicted ratings of 3 movies that are the closest from among the movies not watched by user 4
# the ratings will be predicted by taking the weighted average of movie ratings for similar users
# r[4,] stands for the 4th row/user, n = 3 stands for 3 neighbors, type = "topNList" stands for the closest n neighbours
UB.Rec <- Recommender(r, "UBCF")
pred <- predict(UB.Rec, r[4,], n = 3, type = "topNList")
as(pred, "matrix")




