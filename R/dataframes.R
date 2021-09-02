
#Get information about the version of R being used
version

#Basic syntax for getting help is help()
help(version)

#Assign version details to a variable called ver_l
ver_l <- version

#view the structure of the variable we just created
str(ver_l)

#output the contents of the ver_l variable - both commands do the same thing
ver_l
print(ver_l)

#Get the nickname of the version of R
ver_l["nickname"]
ver_l[14]

#Get a list of the installed packages 
.packages(all.available = TRUE)

#Open the help page that shows a list of all datasets that come with Base R
help(package = datasets)



#view mtcars dataset
View(mtcars)

#assign mtcars to a dataframe called cars_df using the as.data.frame function
#as.data.frame coerces an object to a dataframe
cars_df <- as.data.frame(mtcars)
str(cars_df)
head(cars_df)
View(cars_df)

#The data.frame function also coerces objects to data frames but it 
#can also be used to create data frames not just coerce objects to data frames.
#Be familiar with both and use as.data.frame when you want to coerce
cars_df <- data.frame(mtcars)
View(cars_df)
head(cars_df)
str(cars_df)
View(cars_df)

carsmpg20_v <- mtcars$mpg >= 20
str(carsmpg20_v)

carsmpg20_df <- mtcars[carsmpg20_v,]
View(carsmpg20_df)




#Create a subset of the dataframe to only contain mpg
cars_mpg_df <- as.data.frame(cars_df[1])
View(cars_mpg_df)

#does the same thing
cars_mpg_df <- data.frame(cars_df["mpg"])
View(cars_mpg_df)

#get a list of the car names/row names
car_names <- row.names(cars_df)
View(car_names)

#filter the dataframe to get cars with mpg >= 20.  Need to load tidyverse first
library(tidyverse)
cars_highmpg_df <- filter(cars_df, mpg >= 20)
View(cars_highmpg_df)
row.names(cars_highmpg_df) <- car_names[c(1:14)]
cars_highmpg_df$carnames <- car_names[c(1:14)]
View(cars_highmpg_df)

#subset the data frame to get all rows and the first six columns
df <- data.frame(cars_df[,c(1:6)])
View(df)                 

#subset the data frame to get all rows and columns 1, 2, 3, 6, and 7
df <- data.frame(cars_df[,c(1:3, 6, 7)])
View(df)  

#subset the data frame to get the first 10 rows and columns 1, 2, 3, 6, and 7
df <- data.frame(cars_df[c(1:10),c(1:3, 6, 7)])
View(df) 

#subset the data frame to get row 10  and columns 1, 2, 3, 6, and 7
df <- data.frame(cars_df[c(10),c(1:3, 6, 7)])
View(df) 

#subset the data frame to get row 10  and columns 1, 2, 3, 6, and 7
df <- data.frame(cars_highmpg_df["Merc 280",c(1:3, 6, 7)])
View(df) 

#subset the data frame to get row 10  and columns 1, 2, and 3 by column name
df <- data.frame(cars_highmpg_df["Merc 280",c("mpg", "cyl", "disp")])
View(df) 

