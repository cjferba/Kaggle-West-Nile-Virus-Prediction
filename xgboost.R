require(xgboost)
require(caret)
require(plyr)
require(Metrics)
require(ROCR)

set.seed(2000)

print("Preparing Data")

train <- read.csv('Input/train.csv',header=TRUE,stringsAsFactors = F)
test <- read.csv('Input/test.csv',header=TRUE,stringsAsFactors = F)
weather <- read.csv('Input/weather.csv',header=TRUE,stringsAsFactors = F)

weather$CodeSum <- NULL
weather$Water1 <- NULL
weather$SnowFall <- NULL
weather$Depth <- NULL
weather$CodeSum <- NULL
weather$Sunrise <- NULL
weather$Sunset <- NULL
weather$Heat <- NULL
weather$Cool <- NULL
weather$Depart <- NULL
weather$SeaLevel <- NULL
weather$PrecipTotal <- NULL
weather$Tavg <- NULL
weather$WetBuld <- NULL
weather[weather == 'M'] <- -1
weather[weather == '-'] <- -1
weather[weather == 'T'] <- -1
weather[weather == ' T'] <- -1
weather[weather == '  T'] <- -1
weather$Tmin <- NULL

weather_station1 <- weather[weather$Station == 1,]
weather_station1$Station <- NULL
weather_station2 <- weather[weather$Station == 2,]
weather_station2$Station <- NULL

weather = join(weather_station1, weather_station2, by = 'Date')
weather[is.character(weather)] <- -1

#Shuffle
train <- train[sample(nrow(train)),]

test$WnvPresent     <- -1
test$Id              <- NULL
train$NumMosquitos    <- NULL

trainlength = nrow(train)

x = rbind(train, test)

x$Year <- as.numeric(lapply(strsplit(x$Date, "-"), function(x) x[1]))
x$Month <- as.numeric(lapply(strsplit(x$Date, "-"), function(x) x[2]))
x$Week <- as.numeric(strftime(x$Date, format="%W"))

x$Latitude <- NULL
x$Longitude <- NULL
x$AddressNumberAndStreet <- NULL
x$Street <- NULL
x$AddressAccuracy <- NULL

date <- x$Date
x$Date <- NULL

dmy = dummyVars(" ~ . ", x)
x <- data.frame(predict(dmy, newdata = x))

x$Date <- date
x = join(x, weather, by = 'Date')

x[is.na(x)] <- -1

x$Date <- NULL

#x = as.matrix(x)

#mode(x) <- "numeric"

train = x[1:trainlength,]
test  = x[(nrow(train)+1):nrow(x),]

y <- train$WnvPresent
y_2011 <- train[train$Year == 2011, 'WnvPresent']
y_not_2011 <- train[train$Year != 2011, 'WnvPresent']

train$WnvPresent <- NULL
test$WnvPresent <- NULL

train_2011 = train[train$Year == 2011,]
train_not_2011 = train[train$Year != 2011,]

train = as.matrix(train)
train_2011 = as.matrix(train_2011)
train_not_2011 = as.matrix(train_not_2011)

mode(train) <- "numeric"
mode(train_2011) <- "numeric"
mode(train_not_2011) <- "numeric"

headers = colnames(train)

test = as.matrix(test)
mode(test) <- "numeric"

print("Training the model")

param <- list("objective" = "binary:logistic",
              "eval_metric" = "auc",
              "nthread" = 16,
              "eta" = .02,
              "max_depth" = 20,
              "lambda_bias" = 0,
              "gamma" = .8,
              "min_child_weight" = 4,
              "subsample" = .6,
              "colsample_bytree" = .45,
              "scale_pos_weight" = sum(y==0) / sum(y==1),
              "base_score" = 0.2)

nround = 700
bst = xgboost(param=param, data = train_2011, label = y_2011, nrounds=nround, verbose = 0)

#print("Plotting Importance")
#importance_matrix <- xgb.importance(headers, model = bst)
#importance_matrix <- subset(importance_matrix, Gain > 0.01)
#xgb.plot.importance(importance_matrix)

pred = predict(bst, train_not_2011)

mismatch_count = 0
for(i in 1:length(pred)) {
#pred[[i]] = ifelse(pred[[i]] < 0.2, 0, pred[[i]])
#pred[[i]] = ifelse(pred[[i]] > 0.8, 1, pred[[i]])
if(abs(pred[[i]] - y_not_2011[[i]]) > 0.5)
{
  #pred[[i]] = y_not_2011[[i]]
  #print(paste("Supposed: ", y_not_2011[[i]]," Predicted: ", pred[[i]]))
  mismatch_count <- mismatch_count + 1
}
}

print(paste("Total complete mismatch: ", toString(mismatch_count)))

auc(y_not_2011, pred)

predic = prediction(c(pred), c(y_not_2011))
perf <- performance(predic, "acc")

plot(perf, main = paste("AUC: ", auc(y_not_2011, pred)))

print(pred[26])
print(y_not_2011[26])
print(train_not_2011[26,])

bst = xgboost(param=param, data = train, label = y, nrounds=nround, verbose = 0)

#print("Plotting Importance")
#importance_matrix <- xgb.importance(headers, model = bst)
#importance_matrix <- subset(importance_matrix, Gain > 0.01)
#xgb.plot.importance(importance_matrix)

print("Making prediction")
pred = predict(bst, test)
#for(i in 1:length(pred)) {
       #pred[[i]] = ifelse(pred[[i]] < 0.2, 0, pred[[i]])
       #pred[[i]] = ifelse(pred[[i]] > 0.8, 1, pred[[i]])
#}
summary(pred)
pred = matrix(pred,1,length(pred))

pred = t(pred)

print("Storing Output")
pred = format(pred, digits=2,scientific=F) # shrink the size of submission
pred = data.frame(1:nrow(pred), pred)
names(pred) = c('Id', 'WnvPresent')
write.csv(pred, file="Output/nile-river-boost.csv", quote=FALSE,row.names=FALSE)
