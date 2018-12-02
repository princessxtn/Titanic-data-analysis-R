library(ggplot2)
library(gganimate)
titanic<-read.csv(file = "C:/Users/k/Desktop/train.csv")
dim(titanic)
library(mice)
md.pattern(titanic)#show the NA
library(Hmisc)
titanic$Age[is.na(titanic$Age)]<-mean(titanic$Age,na.rm = T)

anyNA(titanic)
dim(titanic)
write.csv(titanic,file="C:/Users/k/Desktop/train2.csv",row.names = T)

titanic<-read.csv(file = "C:/Users/k/Desktop/train2.csv")
ggplot(titanic, aes(factor(Embarked), Age,fill = Embarked)) + 
  geom_bar(stat=
          'identity') + 
  # Here comes the gganimate code
  transition_states(
    Pclass,
  transition_length =3,
    state_length =1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('cubic-in-out')#根据码头不同，年龄随着阶层的变化sum
ggplot(titanic, aes(factor(Embarked), Age,fill = Embarked)) + 
  geom_boxplot() + 
  # Here comes the gganimate code
  transition_states(
    Pclass,
    transition_length =3,
    state_length =1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('cubic-in-out')#根据码头不同，年龄随着阶层的变化sum

p2 <- ggplot(data = titanic, mapping = aes(x = Age))

p2 + geom_histogram(aes(fill = Survived)) + facet_wrap(Sex~Pclass) + 
  labs(title = "存活率：基于年龄、性别以及阶级")

p3 <- ggplot(data = titanic, mapping = aes(x = as.factor(Parch + SibSp)))
p3 + geom_bar(position = "fill", aes(fill = Survived)) + facet_wrap(~Pclass) +
  labs(title = "存活率：基于阶级以及船上家属数量",
       x = "船上家属数量", y = "百分比")

titanic$Pclass <- as.factor(titanic$Pclass)
ggplot(titanic, aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar(width = 0.5) +
  xlab("Pclass") + 
  ylab("Total Count") + 
  labs(fill = "Survived")
