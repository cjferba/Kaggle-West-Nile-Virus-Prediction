#install.packages("mefa", repos="http://R-Forge.R-project.org")
library(mefa)
library(lubridate)
path <- "input/"
train = read.csv(paste0(path,"train.csv"),header=TRUE,stringsAsFactors = T)
test = read.csv(paste0(path,"test.csv"),header=TRUE,stringsAsFactors = T)
weather = read.csv(paste0(path,"weather.csv"),header=TRUE,stringsAsFactors = T)
spray = read.csv(paste0(path,"spray.csv"),header=TRUE)
subm = read.csv(paste0(path,"west_nile_v6 (1).csv"),header=TRUE,stringsAsFactors = F)

weather[(weather == " ")] <- NA
weather[(weather == "M")] <- NA
weather[(weather == "-")] <- NA
weather[(weather == "T")] <- NA
weather[(weather == " T")] <- NA
weather[(weather == "  T")] <- NA

weather$Water1 = NULL
weather$Depth = NULL
weather$SnowFall = NULL
weather$Sunrise = NULL
weather$Sunset = NULL
weather$Depart = NULL

#Get the nearest station
train$Station <- ifelse((((train$Latitude-41.995)^2 + (train$Longitude + 87.933)^2) < 
                           ((train$Latitude-41.786)^2 + (train$Longitude + 87.752)^2)),1,2)

test$Station <- ifelse((((test$Latitude-41.995)^2 + (test$Longitude + 87.933)^2) < 
                          ((test$Latitude-41.786)^2 + (test$Longitude + 87.752)^2)),1,2)



w1 = weather[weather$Station ==1,]
w2 = weather[weather$Station ==2,]

#Replace NA's with the nearest value above
W1 <- rbind(w1[2,],w1)
W1 <- fill.na(W1) 
W1 <- W1[-1,]
rownames(W1) <- NULL

W2 <- rbind(w2[2,],w2)
W2 <- fill.na(W2) 
W2 <- W2[-1,]
rownames(W2) <- NULL

Weather <- rbind(W1,W2)

for(i in c(3:9,11:16)){
  Weather[,i] <- as.numeric(Weather[,i])
}
Weather[,10] <- factor(Weather[,10])


train <- merge.data.frame(train,Weather)
test <- merge.data.frame(test,Weather)
test <- test[with(test,order(Id)),]

train$day<-as.numeric(day(as.Date(train$Date)))
train$dayofyear<-as.numeric(yday(as.Date(train$Date))) 
#train$month<-factor(month(as.Date(train$Date)))
train$dayofweek<-as.factor(wday(as.Date(train$Date)))
train$year <- as.factor(year(as.Date(train$Date))) 
train$week <- as.integer(week(as.Date(train$Date)))

test$day<-as.numeric(day(as.Date(test$Date)))
test$dayofyear<-as.numeric(yday(as.Date(test$Date)))
#test$month<-factor(month(as.Date(test$Date)))
test$dayofweek<-as.factor(wday(as.Date(test$Date)))
test$year <- as.factor(year(as.Date(test$Date)))
test$week <- as.integer(week(as.Date(test$Date)))


library(h2o)
localH2O <- h2o.init(nthreads = -1,max_mem_size = '7g')

test.hex <- as.h2o(localH2O,test)
train.hex <- as.h2o(localH2O,train)

# model <- h2o.randomForest(x=c(4:11,14:32),y = 13,data = train.hex,
#                           mtries = 18,
#                           sample.rate = 0.5,
#                           classification = T,ntree = 500,verbose = T)
  for(i in 1:20){
    
  model <- h2o.deeplearning(x=c(4:11,14:32),#nfolds = 3,
                            balance_classes = F,variable_importances = T,
                            y=27,#13,
                            data = train.hex,
                            classification=T,
                            activation="RectifierWithDropout",
                            hidden=c(1340,612,256,180),
                            hidden_dropout_ratio=c(0.5,0.4,0.3,0.25),#c(0.55,0.55,0.55),
                            input_dropout_ratio=0.18,#0.02,
                            epochs=50,#50,
                            l1=1e-5,
                            l2=1e-5,
                            rho=0.99,
                            epsilon=1e-8,
                            train_samples_per_iteration=1800,#2000,
                            max_w2=10,
                            seed=1)
  
  model@model$confusion
  
  pred <- h2o.predict(model,test.hex)
  p <- as.data.frame(pred)
  summary(p)
  subm[,2] =( p[,3]+ subm[,2])/2
  print(i)
  
}

summary(subm)
write.csv(subm,file="wNileVirusRF.csv",row.names=FALSE)




