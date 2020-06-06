library(caret)
library(C50)
library(rpart)
library(caretEnsemble)
library(mlbench)
library(pROC)

fc_data<-read.csv("E:/itsstudytym/DT/Fraud_check.csv")
fc_data$Taxable_Income <-  ifelse(fc_data$Taxable_Income <= 30000,'Risky','Good')
set.seed(7)
inTraininglocal<-createDataPartition(fc_data$Taxable_Income,p=.70,list=F)
training<-fc_data[inTraininglocal,]
testing<-fc_data[-inTraininglocal,]

training$Taxable_Income <- as.factor(training$Taxable_Income)
table(training$Taxable_Income)

round(prop.table(table(training$Taxable_Income)) * 100, digits = 1)

##undersampling
good<-which(training$Taxable_Income=="Good")
good_data<-training[good,]
risky_data<-training[-good,]
intraingood<-createDataPartition(good_data$Taxable_Income,p=.27,list=F)
good_data1<-good_data[intraingood,]
finaltraindata<-rbind.data.frame(good_data1,risky_data)

##create model
model<-C5.0(Taxable_Income~.,data=finaltraindata)
summary(model)

pred<-predict.C5.0(model,testing[,-3])
a<-table(testing$Taxable_Income,pred)
sum(diag(a))/sum(a)
plot(model)


#bagging

acc<-c()
for(i in 1:100){
  print(i)
  intraingood<-createDataPartition(good_data$Taxable_Income,p=.35,list=F)
  good_data1<-good_data[intraingood,]
  finaltraindata<-rbind.data.frame(good_data1,risky_data)
  
  ##create model
  model<-C5.0(Taxable_Income~.,data=finaltraindata[,-5],trials=50)
  summary(model)
  
  pred<-predict.C5.0(model,testing[,-3])
  a<-table(testing$Taxable_Income,pred)
  acc<-c(acc,sum(diag(a))/sum(a))
}

##boosting
intraingood<-createDataPartition(good_data$Taxable_Income,p=.27,list=F)
good_data1<-good_data[intraingood,]
finaltraindata<-rbind.data.frame(good_data1,risky_data)

##create model
model<-C5.0(Taxable_Income~.,data=finaltraindata,trials=50)
summary(model)

pred<-predict.C5.0(model,testing[,-3])
a<-table(testing$Taxable_Income,pred)
sum(diag(a))/sum(a)

##staching
set.seed(7)
intraingood<-createDataPartition(good_data$Taxable_Income,p=.27,list=F)
good_data1<-good_data[intraingood,]
finaltraindata<-rbind.data.frame(good_data1,risky_data)

my_control<-trainControl(method = "cv",number = 25)
model<-caretList(x=good_data1[,c(1,2,4,5,6)],y=good_data1[,3],trControl = trainControl(method = 'cv'),methodList = c("glm","knn"))
caretStack(model, method="glm")

pred<-as.data.frame(predict(model,newdata=head(testing)))
pred