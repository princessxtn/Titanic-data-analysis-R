##读入数据
titanic.train<-read.csv("F:/课件及软件教程/课件/统计分析软件/泰坦尼克号数据/train.csv",sep=",")
titanic.test_x<-read.csv("F:/课件及软件教程/课件/统计分析软件/泰坦尼克号数据/test.csv",sep=",")
titanic.test_y<-read.csv("F:/课件及软件教程/课件/统计分析软件/泰坦尼克号数据/gender_submission.csv",sep=",")
titanic.test<-cbind(titanic.test_x,titanic.test_y[2])
titanic<-rbind(titanic.train,titanic.test)
titanic<-titanic[,!names(titanic) %in% c("home.dest","boat","body")]#删去不必要的列
str(titanic)
##转换数据类型
  #因子类型转换为字符串
titanic$Pclass=as.factor(titanic$Pclass)
titanic$Survived=as.factor(titanic$Survived)
titanic$Name=as.character(titanic$Name)
titanic$Ticket=as.character(titanic$Ticket)
titanic$Cabin=as.character(titanic$Cabin)
str(titanic)
  #设置缺失值NA
levels(titanic$Embarked)
table(titanic$Embarked)
levels(titanic$Embarked)[1]<-NA
table(titanic$Embarked,useNA="always")
titanic$Cabin<-ifelse(titanic$Cabin=="",NA,titanic$Cabin)
str(titanic)

##分离测试数据
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

##准备交叉检验
#做10层交叉验证，分割数据
createFolds(titanic.train$Survived,k=10)#各层保存着做检验数据的数据编号
#保存所有交叉验证的数据集
create_ten_flod_cv<-function(){
  set.seed(137)
  lapply(createFolds(titanic.train$Survived,k=10),function(idx){
    return(list(train=titanic.train[-idx,],validation=titanic.train[idx,]))
  })
}
x<-create_ten_flod_cv()#包含所有数据
str(x)
head(x$Fold01$train)#显示部分数据

##数据探索
library(Hmisc)
library(survival)
library(Formula)
#将交叉验证第一层中的训练数据用于数据探索
data<-create_ten_flod_cv()[[1]]$train
#设置method="reserve"，依据应变量survived分割并显示独立变量
summary(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=data,method="reverse")
data.complete<-data[complete.cases(data),]
library(caret)
#因子类型数据图形
mosaicplot(Survived~Pclass+Sex,data=data,color=TRUE,main="pclass and sex")
#查看每个plass与sex组合下的乘客生还率与死亡率
xtabs(~Sex+Pclass,data=data)

##评估指标
predicted<-c(1,0,0,1,1)
actual<-c(1,0,0,0,0)
sum(predicted == actual)/NROW(predicted)


##决策树模型
library(rpart)
m<-rpart(
  Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=titanic.train)
p<-predict(m,newdata=titanic.train,type="class")
head(p)
#rpart交叉检验
library(foreach)
folds<-create_ten_flod_cv()
rpart_result<-foreach(f=folds) %do% {
  model_rpart<-rpart(
    Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=f$train)
  predicted<-predict(model_rpart,newdata=f$validation,type="class")
  return(list(actual=f$validation$Survived,predicted=predicted))
}#保存实际值与预测值
head(rpart_result)
#准确度评估
evaluation<-function(lst){
  accuracy<-sapply(lst,function(one_result) {
    return(sum(one_result$predicted == one_result$actual)/NROW(one_result$actual))
  })
  print(sprintf("MEAN +/- SD: %.3f +/- %.3f",mean(accuracy),sd(accuracy)))
  return(accuracy)
}
(rpart_accuracy<-evaluation(rpart_result))

#条件推断决策树
library(party)
ctree_result<-foreach(f=folds) %do% {
  model_ctree<-ctree(
    Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=f$train)
  predicted<-predict(model_ctree,newdata=f$validation,type="response")
  return(list(actual=f$validation$Survived,predicted=predicted))
}
(ctree_accuracy<-evaluation(ctree_result))
#比较
plot(density(rpart_accuracy),main="rpart VS ctree",ylim=c(0,27))
lines(density(ctree_accuracy),col="red",lty="dashed")
lines(density(family_accuracy),col="green",lty=3)
legend("topright",c("rpart","ctree","family"),col=c("black","red","green"),lty=c(1,2,3))

##其他特征提取
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
##添加家庭ID
#依票号添加家庭ID
  family_idx<-0
ticket_based_family_id<-ddply(all,.(Ticket),function(rows){
  family_idx<<-family_idx+1
  return(data.frame(family_id=paste0("TICKET_",family_idx)))
})
#依据TICket值向数据框all添加family_id列
all<-adply(all,.margins=1,
           function(row){
             family_id<-NA
             if(!is.na(row$Ticket)){
               family_id<-subset(ticket_based_family_id,Ticket==row$Ticket)$family_id
             }
             return(data.frame(family_id=family_id))
           })
##合并家庭成员生还概率
#avg_prob家庭成员平均生还率
all<-ddply(all,
           .(family_id),
           function(rows){
             rows$avg_prob<-mean(rows$prob)
             return(rows)
           })
#maybe_parent maybe_child特定乘客是父母还是子女根据年龄
all<-ddply(all,.(family_id), function(rows){
  rows$maybe_parent<-FALSE
  rows$maybe_child<-FALSE
  if(NROW(rows)==1 ||
     sum(rows$Parch)==0 ||
     NROW(rows)==sum(is.na(rows$Age))){
    return(rows)
  }#家庭成员为1，父母子女数量为0，未保存年龄
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

#avg_parent_prob,avg_child_prob父母的平均生还率和子女的平均生还率
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
#ctree模型
f$train<-subset(all,type=="T")
f$validation<-subset(all,type=="V")
(m<-ctree(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+maybe_parent
          +maybe_child+Age+Sex+avg_prob+avg_parent_prob+avg_child_prob,
          data=f$train))
print(m)
predicted<-predict(m,newdata=f$validation)
return(list(actual=f$validation$Survived,predicted=predicted))
}
#模型评估
family_accuracy<-evaluation(family_result)
plot(m)


