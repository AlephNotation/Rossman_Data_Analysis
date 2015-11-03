setwd("/Users/ty/Desktop/Rossman/Data")
train.store <- read.csv("store.csv")
train.simple <- read.csv("train.csv")
library(ggplot2)
train.total <- merge(train.store, train.simple, by="Store")

#Because the data set is large, and my laptop has limited computing power,
#we will take a random sample of our data to play around with
train.sample<- train.total[sample(1:nrow(train.total), 10000, replace=FALSE),]

#Lets explore the sales data for a quick seconds
hist(train.sample$Sales, 100)

#Interesting. Shockingly, it seemst that stores which are not open cannot make sales. 
#Since predicting the sales figures of non-open stores seems simple, we should only consider cases
#store are open
train.opensample<- subset(train.sample, Open >0)
hist(train.opensample$Sales, 100)

library(randomForest)

rf1 <- randomForest( Sales~Customers+Promo+StateHoliday+SchoolHoliday, data = train.opensample, ntree= 100, importance= TRUE)

library(caret)
imp <- varImp(rf1)
imp
test <-importance(rf1, type=2)

featureImportance <- data.frame(Feature=row.names(test), Importance=test[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +geom_bar(stat="identity", fill="#53cfff") +coord_flip() + theme_light(base_size=20) + ylab("Importance") + ggtitle("Random Forest Feature Importance\n") +theme(plot.title=element_text(size=18))
