data<-read.csv("formability.csv",header=FALSE)
str(data)
data$V1<-as.factor(data$V1)
str(data)
table(data$V1)
#data partition
set.seed(123)
ind<-sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
train<-data[ind==1,]
test<-data[ind==2,]
library(randomForest)
set.seed(222)
rf<-randomForest(V1~.,data=train)
print(rf)
attributes(rf)
rf$confusion
#prediction and cinfusion matrix- train data
library(caret)
p1<- predict(rf,train)
head(p1)
head(train$V1)
confusionMatrix(p1,train$V1)
p2<-predict(rf,test)
head(p2)
confusionMatrix(p2,test$V1)
plot(rf)
#Tune mtry
t<-tuneRF(train[,-3],train[,3],mtry=1,ntreeTry=300,stepFactor=0.1,plot=TRUE,trace=TRUE,improve=0.05)

