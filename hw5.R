library(e1071)
library(dplyr)
library(lattice)
library(ggplot2)
library(caret)#cofusionmatrix
summary(iris)

ggplot(iris,aes(x=Species,y=Sepal.Length))+
  geom_boxplot()+coord_flip()+
  labs(y="length",x="species",title=
         'Sepal.Length box by species')

ggplot(iris,aes(x=Species,y=Sepal.Width))+
  geom_boxplot()+coord_flip()+
  labs(y="Width",x="species",title=
         'Sepal.Width box by species')

ggplot(iris,aes(x=Species,y=Petal.Length))+
  geom_boxplot()+coord_flip()+
  labs(y="Length",x="species",title=
         'Petal.Length box by species')

ggplot(iris,aes(x=Species,y=Petal.Width))+
  geom_boxplot()+coord_flip()+
  labs(y="Width",x="species",title=
         'Petal.Width box by species')



plot(Species,Sepal.Length)
plot(iris)
plot(iris$Sepal.Length,iris$Sepal.Width,col=iris$Species)
testId = sample(nrow(iris),100, replace=FALSE)
x <- subset(iris[testId,], select = -Species)
y <- iris$Species[testId]
trainingId = iris[-testId,]
svm_model1 = 
  svmfit = svm(Species ~ ., data = iris[-testId,])

pred = predict(svm_model1,x)
confusionMatrix(pred,y)

require(ggplot2)

theme_set(theme_bw()) # have now fixed theme_bw
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_point()


#H0:μ(Setosa)=μ(Versicolor)=μ(Virginica) Petal.Width
#H1:至少有一種平均數和其他品種不相等
modelPW<-lm(Petal.Width~Species,data=iris)
anova(modelPW) 

modelPL<- lm(Petal.Length~Species, data=iris)
anova(modelPL)




testId = sample(nrow(iris),100, replace=FALSE)
x <- subset(iris[testId,], select = -Species)
y <- iris$Species[testId]
trainingId = iris[-testId,]
svm_model1 = 
  svmfit = svm(Species ~ ., data = iris[-testId,])

pred = predict(svm_model1,x)
confusionMatrix(pred,y)