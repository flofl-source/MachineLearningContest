rm(list=ls())
data <- read.csv("C:\\Users\\flofl\\Documents\\data.csv")
test <- read.csv("C:\\Users\\flofl\\Documents\\test.csv")

head(data)
data
names(data)
str(data) #some datas are not in factor
##.Ca marche pas
library(tidyverse)
sum(is.na(data$SeriousDlqin2yrs)==0)
nrow(data)
nrow(test)
##

##### logreg avec toute les variable
str(data)
classifier.logreg <- glm(dataset$SeriousDlqin2yrs~.,family='binomial',data=dataset )
classifier.logreg

pred.glm = predict(classifier.logreg, newdata = test, type="response")

pred.glm_0_1 = ifelse(pred.glm >= 0.5, 1,0)
head(pred.glm)
head(pred.glm_0_1)

to_be_submitted = data.frame(id=rownames(test), target=pred.glm_0_1)
write.csv(to_be_submitted , file = "to_be_submitted.csv", row.names = F)


cm = table(test_set[,3], pred.glm_0_1)
cm

#RANDOM FOREST

data$SeriousDlqin2yrs=ifelse(data$SeriousDlqin2yrs==FALSE, 0, 1)


str(data)
summary(data)
nrow(data)
## montrer que on a  14465 missing value, starting from row 53039
data[is.na(data)]
dataset <- na.omit(data)
summary(dataset)
nrow(dataset)
set.seed(21)

library(MASS)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
data_idx=sample(1:nrow(dataset),nrow(dataset)/2)
data_train=dataset[data_idx,]
data_test=dataset[-data_idx,]
summary(data_train)


data_tree=rpart(data_train$SeriousDlqin2yrs~.,data=data_train)

rpart.plot(data_tree)
data_tree_pred=predict(data_tree,newdata=data_test,type="vector")



dataset$SeriousDlqin2yrs=as.factor(dataset$SeriousDlqin2yrs)

data_bag=randomForest(dataset$SeriousDlqin2yrs~.,data=dataset,susbset=data_idx,mtry=57)

data_bag_pred=predict(data_bag,newdata=data_test,type="response")


ncol(data_train) #11 colums
data_RF=randomForest(dataset$SeriousDlqin2yrs~.,dataset,susbet=data_idx,mtry=11)
data_RF_pred=predict(data_RF, newdata=data_test, type="response")

data_boost=gbm(data_train$SeriousDlqin2yrs ~ ., data = data_train, distribution = "gaussian", 
               n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
data_boost_pred=predict(data_boost,newdata=data_test,type="response")

data_glm=glm(data_train$SeriousDlqin2yrs~., family="binomial", data=data_train)
data_glm_pred=predict(data_glm, newdata=data_test,type="response")

data_tree_pred_0_1 = ifelse(data_tree_pred > 0.5, 1,0)
data_glm_pred_0_1 = ifelse(data_glm_pred > 0.5, 1,0)
data_boost_pred_0_1 = ifelse(data_boost_pred > 0.5, 1,0)

acc = function(x,y){
  conf=table(x,y)
  tp = conf[1]
  tn = conf[4]
  fp = conf[3]
  fn = conf[2]
  return ((tp+tn)/(tp+tn+fn+fp))
}
acc_tree=acc(data_tree_pred_0_1,data_test$SeriousDlqin2yrs)
acc_glm=acc(data_glm_pred_0_1,data_test$SeriousDlqin2yrs)
acc_RF=acc(data_RF_pred,data_test$SeriousDlqin2yrs)
acc_bag=acc(data_bag_pred,data_test$SeriousDlqin2yrs)
acc_boost=acc(data_boost_pred_0_1,data_test$SeriousDlqin2yrs)


acc_tree
acc_glm
acc_RF
acc_bag
acc_boost



###ON VA ESSAYER AVEC GLM METHOD
rm(list=ls())
data <- read.csv("C:\\Users\\flofl\\Documents\\data.csv")
test <- read.csv("C:\\Users\\flofl\\Documents\\test.csv")
library(MASS)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)

data <- na.omit(data)

data_glm=glm(data$SeriousDlqin2yrs~., family="binomial", data=data)
data_glm_pred=predict(data_glm, newdata=test,type="response")
data_glm_pred_0_1 = ifelse(data_glm_pred > 0.5, 1,0)
to_be_submitted_glm = data.frame(id=rownames(test),SeriousDlqin2yrs=data_glm_pred_0_1)
head(to_be_submitted_glm)
write.csv(to_be_submitted_glm , file = "glm_method2.csv", row.names = F)


data_boost=gbm(data$SeriousDlqin2yrs ~ ., data = data, distribution = "gaussian", 
               n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
data_boost_pred=predict(data_boost,newdata=test,type="response")
data_boost_pred_0_1 = ifelse(data_boost_pred > 0.5, 1,0)

to_be_submitted = data.frame(id=rownames(test),SeriousDlqin2yrs=data_boost_pred_0_1)
head(to_be_submitted)
write.csv(to_be_submitted , file = "boost_method.csv", row.names = F)
#too slow
data_bag=randomForest(data$SeriousDlqin2yrs~.,data=data,mtry=10)
data_bag_pred=predict(data_bag,newdata=test,type="response")

to_be_submitted_bag = data.frame(id=rownames(test),SeriousDlqin2yrs=data_boost_pred_0_1)
head(to_be_submitted_bag)
write.csv(to_be_submitted_bag , file = "bag_method.csv", row.names = F)

names(data)
data$SeriousDlqin2yrs<-as.factor(data$SeriousDlqin2yrs)
scale(data$age)
scale(data$NumberOfTime30_59DaysPastDueNotWorse)
scale(data$DebtRatio)
scale(data$MonthlyIncome)
scale(data$NumberOfOpenCreditLinesAndLoans)
scale(data$NumberOfTimes90DaysLate)
scale(data$NumberRealEstateLoansOrLines)
scale(data$NumberOfTime60_89DaysPastDueNotWorse)
scale(data$NumberOfDependents)

scale(test$age)
scale(test$NumberOfTime30_59DaysPastDueNotWorse)
scale(test$DebtRatio)
scale(test$MonthlyIncome)
scale(test$NumberOfOpenCreditLinesAndLoans)
scale(test$NumberOfTimes90DaysLate)
scale(test$NumberRealEstateLoansOrLines)
scale(test$NumberOfTime60_89DaysPastDueNotWorse)
scale(test$NumberOfDependents)

summary(data_glm)

data_boost=gbm(data$SeriousDlqin2yrs ~ ., data = data, distribution = "gaussian", 
               n.trees = 10000, interaction.depth = 4, shrinkage = 0.01)
data_boost_pred=predict(data_boost,newdata=test,type="response")
data_boost_pred_0_1 = ifelse(data_boost_pred > 0.5, 1,0)
to_be_submitted_boost2 = data.frame(id=rownames(test),SeriousDlqin2yrs=data_boost_pred_0_1)
head(to_be_submitted)
write.csv(to_be_submitted_boost2 , file = "boost_method2.csv", row.names = F)

install.packages("factoextra")
install.packages("FactorMineR")
library("factoextra")
pca_data = princompt ( data [2,], cor = T )
res.pca <- prcomp(data, scale = TRUE)

#sans les scale
data_tree=rpart(data$SeriousDlqin2yrs~., data=data)
data_tree_pred=predict(data_tree,newdata=test,type="vector")
data_tree_pred_0_1=ifelse(data_tree_pred > 0.5, 1,0)
data_tree_pred_0_1
to_be_submitted_tree = data.frame(id=rownames(test),SeriousDlqin2yrs=data_tree_pred_0_1)

write.csv(to_be_submitted_tree , file = "tree.csv", row.names = F)



