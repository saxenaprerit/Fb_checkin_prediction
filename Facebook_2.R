#Second script #

rm(list=ls(all=TRUE))

setwd("C:/Users/ps11337/Desktop/datatest/Facebook")

load("C:/Users/ps11337/Desktop/datatest/Facebook/fb.RData")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

gc()

test$place_id <- -1
All_Data <- rbind(train, test)

All_Data$t1 <- All_Data$time %% 60
All_Data$t2 <- (trunc(All_Data$time /  60 ) %% 24) 
All_Data$t3 <- (trunc(All_Data$time / 1440 ) %% 7)
print(summary(All_Data))

library(caret)

All_Data$x_r <- round(All_Data$x, digits = 0)

table(All_Data$x_r)

All_Data$y_r <- round(All_Data$y, digits = 0)

table(All_Data$y_r)

All_Data$pos <- paste(All_Data$x_r, All_Data$y_r)

table(All_Data$pos)

All_Data2 <- All_Data[,c(1,4,6,12)]
str(All_Data2)

All_Data2$min <- All_Data$t1
All_Data2$hour <- All_Data$t2
All_Data2$day <- All_Data$t3

str(All_Data2)

train <- All_Data2[which(All_Data2$place_id >= 0),]
test <- All_Data2[which(All_Data2$place_id < 0),]

rm(All_Data2)
rm(All_Data)

gc()

#bucketing train #

str(train)
train$row_id <- NULL
train$min <- NULL

hoursb <- function(x)
{
    if(x>6 && x<=12)
    {
      bin <- "Morning"
    }
    else if (x>12 && x<=18)
    {
      bin <- "Afternoon"
    }
    else if (x>18 && x<=24)
    {
      bin <- "Night"
    }
    else
    {
      bin <- "Late Night"
    }
    return(bin)
}

train$houbuc <- as.factor(sapply(train$hour,hoursb))
train$hour <- NULL
str(train)


accub <- function(x)
{
  if(x>=0 && x<100)
  {
    bin <- "Low"
  }
  else if (x>=100 && x<200)
  {
    bin <- "Medium"
  }
  else if (x>=200 && x<300)
  {
    bin <- "High"
  }
  else
  {
    bin <- "Very High"
  }
  return(bin)
}

train$accubuc <- as.factor(sapply(train$accuracy,accub))
train$accuracy <- NULL
str(train)



library(caTools)
set.seed(123)
#spl = sample.split(train, 0.7)
train1 <- train[1:1000,]
train2 <- train[1001:1500,]

train1$row_id <- NULL

####### CART model######

library(rpart)
modelCART <- rpart(place_id~., data = train2, method="class")

library(adabag)
modeladaboost <- boosting(place_id ~.,data=train2,boos=TRUE,mfinal=10)

library(randomForest)
set.seed=123
modelrf=randomForest(place_id~., 
                     data=train1, 
                     ntree=10)
plot(modelrf)
importance(modelrf)

a=table(train1$Flight.Status, predict(modelrf, train1))
r_rftr=(a[2,2])/(a[2,1]+a[2,2])*100

table(test$Flight.Status, predict(modelrf, test))
a=table(test$Flight.Status, predict(modelrf, test))
r_rfte=(a[2,2])/(a[2,1]+a[2,2])*100
