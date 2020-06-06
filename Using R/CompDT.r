rm(list=ls())
cat("\14")

library(caret)
library(C50)
library(caretEnsemble)
library(mlbench)
library(pROC)
library(rpart)

Comp<-read.csv("E:/itsstudytym/assignments/DT/Company_Data.csv")

str(Comp)
Comp$Urban<-as.numeric(Comp$Urban)
Comp$US<-as.numeric(Comp$US)
Comp$ShelveLoc<-as.numeric(Comp$ShelveLoc)

normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

Comp_Norm<-as.data.frame(lapply(Comp[,2:11],normalize))
Comp_New<-cbind(Comp$Sales,Comp_Norm)
names(Comp_New)[names(Comp_New)=='Comp$Sales']<-'Sales'
Comp_New$Sales<-ifelse(Comp_New$Sales>7.490,1,0)
Comp_New$Sales<-as.factor(Comp_New$Sales)
str(Comp_New)

set.seed(7)
itl<-createDataPartition(Comp_New$Sales,p=0.7,list=F)
training<-Comp_New[itl,]
testing<-Comp_New[-itl,]

model<-C5.0(Sales~.,data=training)
summary(model)

pred<-predict.C5.0(model,testing[,-1])
a<-table(testing$Sales,pred)
sum(diag(a))/sum(a)
plot(model)

##boosting
acc<-c()
for (i in 1:100){
  print(i)
  itl<-createDataPartition(Comp_New$Sales,p=0.7,list=F)
  training<-Comp_New[itl,]
  testing<-Comp_New[-itl,]
  
  model<-C5.0(Sales~.,data=training)
  summary(model)
  
  pred<-predict.C5.0(model,testing[,-1])
  a<-table(testing$Sales,pred)
  acc<-c(acc,sum(diag(a))/sum(a))
}

##boosting
model<-C5.0(Sales~.,data = training,trials=10)
#Generate the model summary
summary(model)
#Predict for test data set
pred<-predict.C5.0(model,testing[,-1])
a<-table(testing$Sales,pred)
sum(diag(a))/sum(a)
plot(model)
