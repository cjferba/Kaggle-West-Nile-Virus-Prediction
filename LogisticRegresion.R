library(Metrics)
library("psych", lib.loc="C:/Program Files/R/R-3.2.0/library")
#library(h2o)
#h2oServer <- h2o.init()   ##we'd like to try this, but does not initialize
library(data.table)   ## load data in quickly with fread
library(Metrics)
library(xgboost)
x <- fread("train.csv")
test <- fread("test.csv")

## prep the species column by moving the test-only UNSPECIFIED CULEX to CULEX ERRATICUS, and re-doing the levels
## logistic regression will complain otherwise
vSpecies<-c(as.character(x$Species),as.character(test$Species))
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))

## data.table syntax for adding a column; could overwrite the existing column as well
x[,Species2:=factor(vSpecies[1:nrow(x)],levels=unique(vSpecies))]
test[,Species2:=factor(vSpecies[(nrow(x)+1):length(vSpecies)],levels=unique(vSpecies))]

## also add some fields for components of the date using simple substrings
x[,dMonth:=substr(x$Date,6,7)]
x[,dYear:=substr(x$Date,1,4)]
test[,dMonth:=substr(test$Date,6,7)]

## glance at some conditional probabilities
x[,mean(WnvPresent),by="Species"][order(V1)]  ##glance at average by species
x[,mean(WnvPresent),by="Block"][order(V1)]  ##glance at average by block
x[,mean(WnvPresent),by="dMonth"][order(V1)]  ##glance at average by month of year

### Start modeling
## use 2011 as a cross validation year; x1 will include the other three years; x2 will include 2011
x1<-x[dYear!=2011,]
x2<-x[dYear==2011,]

## check sizes of split frames
dim(x1)
dim(x2)

# Set necessary parameter
myeta=0.2#0.2
mydepth=6#8
mylambda=1#1
myalpha=0.8#0.8
mylambda_bias=0
#child_range=c(0.5, 1, 2, 3, 4, 5, 10)
child_range=c(3)
mychild=3
mysubsamp=0.9
mycolsamp=0.6
round_count=3
nround = 180
seednum=20150320
set.seed(seednum)
# Set necessary parameter
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 2,
              "bst:eta" = myeta,
              "bst:max_depth" = mydepth,
              "lambda" = mylambda,
              "lambda_bias" = mylambda_bias,
              "alpha" = myalpha,
              "min_child_weight" = mychild,
              "subsample" = mysubsamp,
              "colsample_bytree" = mycolsamp,
              "nthread" = 3)
## fit a logistic regression model using just three key fields

fitCv<-glm(WnvPresent ~ dMonth + Species2+ Block, data = x1, family = binomial)
p2<-predict(fitCv, newdata = x2, type = "response")
## check for a reasonable AUC of the model against unseen data (2011)
auc(x2$WnvPresent,p2)

## now fit a new model to all the data, so that our final submission includes information learned from 2011 as well
#variables<-Boruta(WnvPresent~., data=x)
Minima<-(x$WnvPresent==1)

ClaseMinima<-x[Minima,]
x<-as.data.frame(rbind(x,ClaseMinima))
describe(x$WnvPresent)
summary(as.factor(x$WnvPresent))
fitSubmit<-glm(WnvPresent ~ dMonth + Species2 + Block, data = x, family = binomial,)

bst.cv = xgb.cv(param=param, data = x[,13] ,label = y,nfold = 3 ,nrounds=cv.nround)

levels(test$Species2)
pSubmit<-predict(fitSubmit, newdata = test, type = "response")

## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
summary(pSubmit)

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"logistic_regression_three_factors.csv",row.names=FALSE,quote=FALSE)
