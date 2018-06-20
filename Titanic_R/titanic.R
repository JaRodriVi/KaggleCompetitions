##### 1. Libraries #####

library(tidyverse)
library(ggplot2)
library(caTools)

## -------------------------------------------------------------------------
##### 2. Read Data #####

train <- read.csv('train.csv')
test <- read.csv('test.csv')

## -------------------------------------------------------------------------
##### 3. Clean Data #####

summary(train)
summary(test)

train$Pclass <- as.factor(train$Pclass)

train$Parch[train$Parch==0] <- "0"
train$Parch[train$Parch==1] <- "1"
train$Parch[train$Parch>2] <- "3+"
train$Parch <-as.factor(train$Parch)

train$SibSp[train$SibSp==0] <- "0"
train$SibSp[train$SibSp==1] <- "1"
train$SibSp[train$SibSp>1 & train$SibSp<4] <- "2o3"
train$SibSp[train$SibSp>3] <- "4+"
train$SibSp <- as.factor(train$SibSp)

test$Pclass <- as.factor(test$Pclass)

test$Parch[test$Parch==0] <- "0"
test$Parch[test$Parch==1] <- "1"
test$Parch[test$Parch>2] <- "3+"
test$Parch <-as.factor(test$Parch)

test$SibSp[test$SibSp==0] <- "0"
test$SibSp[test$SibSp==1] <- "1"
test$SibSp[test$SibSp>1 & test$SibSp<4] <- "2o3"
test$SibSp[test$SibSp>3] <- "4+"
test$SibSp <- as.factor(test$SibSp)


train <- train %>%
  mutate(
    Age = ifelse(is.na(Age), mean(full$Age, na.rm=TRUE), Age),
    'AgeGroup' = case_when(Age < 13 ~ "00-12", 
                           Age >= 13 & Age < 18 ~ "13-17",
                           Age >= 18 & Age < 60 ~ "18-59",
                           Age >= 60 ~ "60+"))
train$AgeGroup <-as.factor(train$AgeGroup)

test <- test %>%
  mutate(
    Age = ifelse(is.na(Age), mean(full$Age, na.rm=TRUE), Age),
    'AgeGroup' = case_when(Age < 13 ~ "00-12", 
                           Age >= 13 & Age < 18 ~ "13-17",
                           Age >= 18 & Age < 60 ~ "18-59",
                           Age >= 60 ~ "60+"))
test$AgeGroup <-as.factor(test$AgeGroup)

train$title <- gsub("^.*, (.*?)\\..*$", "\\1", train$Name)
train$title[train$title == 'Mlle']           <- 'Miss' 
train$title[train$title == 'Ms']             <- 'Miss'
train$title[train$title == 'Mme']            <- 'Mrs' 
train$title[train$title == 'Lady']           <- 'Miss'
train$title[train$title == 'Dona']           <- 'Miss'
train$title[train$title == 'Capt']           <- 'Officer' 
train$title[train$title == 'Col']            <- 'Officer' 
train$title[train$title == 'Major']          <- 'Officer'
train$title[train$title == 'Dr']             <- 'Officer'
train$title[train$title == 'Rev']            <- 'Officer'
train$title[train$title == 'Don']            <- 'Officer'
train$title[train$title == 'Sir']            <- 'Officer'
train$title[train$title == 'the Countess']   <- 'Officer'
train$title[train$title == 'Jonkheer']       <- 'Officer' 

train$title <- as.factor(train$title)

test$title <- gsub("^.*, (.*?)\\..*$", "\\1", test$Name)
test$title[test$title == 'Mlle']           <- 'Miss' 
test$title[test$title == 'Ms']             <- 'Miss'
test$title[test$title == 'Mme']            <- 'Mrs' 
test$title[test$title == 'Lady']           <- 'Miss'
test$title[test$title == 'Dona']           <- 'Miss'
test$title[test$title == 'Capt']           <- 'Officer' 
test$title[test$title == 'Col']            <- 'Officer' 
test$title[test$title == 'Major']          <- 'Officer'
test$title[test$title == 'Dr']             <- 'Officer'
test$title[test$title == 'Rev']            <- 'Officer'
test$title[test$title == 'Don']            <- 'Officer'
test$title[test$title == 'Sir']            <- 'Officer'
test$title[test$title == 'the Countess']   <- 'Officer'
test$title[test$title == 'Jonkheer']       <- 'Officer'  

test$title <- as.factor(test$title)

## -------------------------------------------------------------------------
##### General Linear Model #####

SAMPLE <- sample.split(train$Survived,SplitRatio=0.75)
train <- subset(train,SAMPLE==TRUE)
validation <- subset(train,SAMPLE==FALSE)
validation <- validation[!is.na(validation$Survived),]

summary(train)
summary(validation)
summary(test)

modelLM <- glm(Survived~Pclass+Sex+SibSp+Parch+AgeGroup+title ,data = train, family=binomial(link="probit"))
modelLMFinal <- step(modelLM,direction="both",trace=1)
modelLMFinal <- glm(Survived~Pclass+Sex+SibSp+title ,data = train)

summary(modelLM)
summary(modelLMFinal)
anova(modelLMFinal,modelLM)

coef(modelLMFinal)
exp(coef(modelLMFinal))
confint(modelLMFinal,level=0.95)

train$Prediction <- predict(modelLMFinal,type ="response")
Predauxiliar= prediction(train$Prediction, train$Survived, label.ordering = NULL)
auc.tmp = performance(Predauxiliar, "auc");
aucModeloLMtrain = as.numeric(auc.tmp@y.values)
aucModeloLMtrain

CurvaRocModeloLMTrain <- performance(Predauxiliar,"tpr","fpr")
plot(CurvaRocModeloLMTrain,colorize=TRUE)
abline(a=0,b=1)

validation$prediccion=predict(modelLMFinal, newdata=validation,type="response")
PredauxiliarV = prediction(validation$prediccion, validation$Survived, label.ordering = NULL)
auc.tmpV = performance(PredauxiliarV, "auc");
aucModeloLMValidation = as.numeric(auc.tmpV@y.values)
aucModeloLMValidation

CurvaRocModeloLMValidation <- performance(PredauxiliarV,"tpr","fpr")
plot(CurvaRocModeloLMValidation,colorize=TRUE)
abline(a=0,b=1)

test$pred=predict(modelLMFinal, newdata=test,type="response")

test <- test %>%
  mutate(
    pred = ifelse(is.na(pred), mean(test$pred, na.rm=TRUE), pred),
    'Survived' = case_when(pred < 0.5 ~ 0, 
                           pred >= 0.5 ~ 1))
solution <- data.frame(test$Survived)
colnames(solution) = c("Survived")
rownames(solution) <- test$PassengerId
write.csv(solution, file = "MyRtitanic.csv",row.names=TRUE)