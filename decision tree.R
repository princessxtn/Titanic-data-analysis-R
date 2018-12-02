##��������
titanic.train<-read.csv("F:/�μ��������̳�/�μ�/ͳ�Ʒ�������/̩̹��˺�����/train.csv",sep=",")
titanic.test_x<-read.csv("F:/�μ��������̳�/�μ�/ͳ�Ʒ�������/̩̹��˺�����/test.csv",sep=",")
titanic.test_y<-read.csv("F:/�μ��������̳�/�μ�/ͳ�Ʒ�������/̩̹��˺�����/gender_submission.csv",sep=",")
titanic.test<-cbind(titanic.test_x,titanic.test_y[2])
titanic<-rbind(titanic.train,titanic.test)
titanic<-titanic[,!names(titanic) %in% c("home.dest","boat","body")]#ɾȥ����Ҫ����
str(titanic)
##ת����������
  #��������ת��Ϊ�ַ���
titanic$Pclass=as.factor(titanic$Pclass)
titanic$Survived=as.factor(titanic$Survived)
titanic$Name=as.character(titanic$Name)
titanic$Ticket=as.character(titanic$Ticket)
titanic$Cabin=as.character(titanic$Cabin)
str(titanic)
  #����ȱʧֵNA
levels(titanic$Embarked)
table(titanic$Embarked)
levels(titanic$Embarked)[1]<-NA
table(titanic$Embarked,useNA="always")
titanic$Cabin<-ifelse(titanic$Cabin=="",NA,titanic$Cabin)
str(titanic)

##�����������
library(ggplot2)
library("caret", lib.loc="~/R/win-library/3.5")
set.seed(137)
test_idx<-createDataPartition(titanic$Survived,p=0.1)$Resample1
titanic.test<-titanic[test_idx,]
titanic.train<-titanic[-test_idx,]
nrow(titanic.test)
prop.table(table(titanic.test$Survived))
nrow(titanic.train)
prop.table(table(titanic.train$Survived))

##׼���������
#��10�㽻����֤���ָ�����
createFolds(titanic.train$Survived,k=10)#���㱣�������������ݵ����ݱ��
#�������н�����֤�����ݼ�
create_ten_flod_cv<-function(){
  set.seed(137)
  lapply(createFolds(titanic.train$Survived,k=10),function(idx){
    return(list(train=titanic.train[-idx,],validation=titanic.train[idx,]))
  })
}
x<-create_ten_flod_cv()#������������
str(x)
head(x$Fold01$train)#��ʾ��������

##����̽��
library(Hmisc)
library(survival)
library(Formula)
#��������֤��һ���е�ѵ��������������̽��
data<-create_ten_flod_cv()[[1]]$train
#����method="reserve"������Ӧ����survived�ָ��ʾ��������
summary(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=data,method="reverse")
data.complete<-data[complete.cases(data),]
library(caret)
#������������ͼ��
mosaicplot(Survived~Pclass+Sex,data=data,color=TRUE,main="pclass and sex")
#�鿴ÿ��plass��sex����µĳ˿���������������
xtabs(~Sex+Pclass,data=data)

##����ָ��
predicted<-c(1,0,0,1,1)
actual<-c(1,0,0,0,0)
sum(predicted == actual)/NROW(predicted)


##������ģ��
library(rpart)
m<-rpart(
  Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=titanic.train)
p<-predict(m,newdata=titanic.train,type="class")
head(p)
#rpart�������
library(foreach)
folds<-create_ten_flod_cv()
rpart_result<-foreach(f=folds) %do% {
  model_rpart<-rpart(
    Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=f$train)
  predicted<-predict(model_rpart,newdata=f$validation,type="class")
  return(list(actual=f$validation$Survived,predicted=predicted))
}#����ʵ��ֵ��Ԥ��ֵ
head(rpart_result)
#׼ȷ������
evaluation<-function(lst){
  accuracy<-sapply(lst,function(one_result) {
    return(sum(one_result$predicted == one_result$actual)/NROW(one_result$actual))
  })
  print(sprintf("MEAN +/- SD: %.3f +/- %.3f",mean(accuracy),sd(accuracy)))
  return(accuracy)
}
(rpart_accuracy<-evaluation(rpart_result))

#�����ƶϾ�����
library(party)
ctree_result<-foreach(f=folds) %do% {
  model_ctree<-ctree(
    Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=f$train)
  predicted<-predict(model_ctree,newdata=f$validation,type="response")
  return(list(actual=f$validation$Survived,predicted=predicted))
}
(ctree_accuracy<-evaluation(ctree_result))
#�Ƚ�
plot(density(rpart_accuracy),main="rpart VS ctree",ylim=c(0,27))
lines(density(ctree_accuracy),col="red",lty="dashed")
lines(density(family_accuracy),col="green",lty=3)
legend("topright",c("rpart","ctree","family"),col=c("black","red","green"),lty=c(1,2,3))

##����������ȡ
sum(is.na(titanic.train$Ticket))
sum(is.na(titanic.train$Embarked))
sum(is.na(titanic.train$Cabin))
library(plyr)
family_result<-foreach(f=folds) %do% {
  f$train$type<-"T"
  f$validation$type<-"V"
  all<-rbind(f$train,f$validation)
  ctree_model<-ctree(
    Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=f$train)
  all$prob<-sapply(predict(ctree_model,type="prob",newdata=all),
                   function(result){result[1]})
##���Ӽ�ͥID
#��Ʊ�����Ӽ�ͥID
  family_idx<-0
ticket_based_family_id<-ddply(all,.(Ticket),function(rows){
  family_idx<<-family_idx+1
  return(data.frame(family_id=paste0("TICKET_",family_idx)))
})
#����TICketֵ�����ݿ�all����family_id��
all<-adply(all,.margins=1,
           function(row){
             family_id<-NA
             if(!is.na(row$Ticket)){
               family_id<-subset(ticket_based_family_id,Ticket==row$Ticket)$family_id
             }
             return(data.frame(family_id=family_id))
           })
##�ϲ���ͥ��Ա��������
#avg_prob��ͥ��Աƽ��������
all<-ddply(all,
           .(family_id),
           function(rows){
             rows$avg_prob<-mean(rows$prob)
             return(rows)
           })
#maybe_parent maybe_child�ض��˿��Ǹ�ĸ������Ů��������
all<-ddply(all,.(family_id), function(rows){
  rows$maybe_parent<-FALSE
  rows$maybe_child<-FALSE
  if(NROW(rows)==1 ||
     sum(rows$Parch)==0 ||
     NROW(rows)==sum(is.na(rows$Age))){
    return(rows)
  }#��ͥ��ԱΪ1����ĸ��Ů����Ϊ0��δ��������
  max_age<-max(rows$Age,na.rm=TRUE)
  min_age<-min(rows$Age,na.rm=TRUE)
  return(adply(rows,1,function(row){
    if (!is.na(row$Age) && !is.na(row$Sex)){
      row$maybe_parent<-(max_age-row$Age)<15
      row$maybe_child<-(row$Age-min_age)<15
    }
    return(row)
  }))
})

#avg_parent_prob,avg_child_prob��ĸ��ƽ�������ʺ���Ů��ƽ��������
all<-ddply(all,.(family_id),function(rows){
  rows$avg_parent_prob<-rows$avg_prob
  rows$avg_child_prob<-rows$avg_prob
  if(NROW(rows)==1||sum(rows$Parch)==0){
   return(rows) 
  }
  parent_prob<-subset(rows,maybe_parent==TRUE)[,"prob"]
  if(NROW(parent_prob)>0){
    rows$avg_parent_prob<-mean(parent_prob)
  }
  child_prob<-c(subset(rows,maybe_child==TRUE)[,"prob"])
  if(NROW(child_prob)>0){
    rows$avg_child_prob<-mean(child_prob)
  }
  return(rows)
})
#ctreeģ��
f$train<-subset(all,type=="T")
f$validation<-subset(all,type=="V")
(m<-ctree(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+maybe_parent
          +maybe_child+Age+Sex+avg_prob+avg_parent_prob+avg_child_prob,
          data=f$train))
print(m)
predicted<-predict(m,newdata=f$validation)
return(list(actual=f$validation$Survived,predicted=predicted))
}
#ģ������
family_accuracy<-evaluation(family_result)
plot(m)

