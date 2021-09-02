library(readxl)
library(ggplot2)
nrow(fb)
ncol(fb)
View(fb)
splitfb<-c(1,2,3,4,5,8,11,17,20)
fb<-fb[,splitfb]
View(fb)
sum(complete.cases(fb))
fb[is.na(fb)] =0

fb1 <- fb                  # Duplicate data
colnames(fb1) <- gsub(" ", ".", colnames(fb))
write.csv(fb1,"fb1.csv")
