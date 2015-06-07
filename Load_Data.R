# Read competition data files:
library(readr)
train <- read.csv("train.csv")
test <- read.csv("test.csv")
weather <- read.csv("weather.csv")
spray<- read.csv("spray.csv")

listaDatos<-strsplit(as.character(train$Address),split = ",")
sapply(1:length(listaDatos),function(x){
  as.data.frame(t(cbind(listaDatos[[1]],listaDatos[[2]],listaDatos[[3]])))
})

















