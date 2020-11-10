---
title: "Big Data Project 3"
author: "No Error"
date: "3/13/2020"
output:
  word_document: default
  html_document: default
---

Load packages 
```{r,warning=FALSE, message=FALSE}
library(data.table)
library(GGally)
library(tidyverse)
library(caret)
library(class)
library(FNN)
library(rpart)
library(rpart.plot)
library(caret)
library(broom)
library(car)
library(moments)
```


# Classification

## 1.1 KNN

read files
```{r}
Air_Quality_Lung_Cancer_Data <- read.csv(file = 'Air_Quality_Lung_Cancer_Data_categorical.csv')
```

do clustering for target variable
```{r}
set.seed(1)
str(Air_Quality_Lung_Cancer_Data$Lung.Cancer)

ratio_ss <- rep(0,7)

for (k in 1:7) {
  
  # Apply k-means to school_result: school_km
  LungCancer_km <- kmeans(Air_Quality_Lung_Cancer_Data$Lung.Cancer, k, nstart = 20)
  
  # Save the ratio between of WSS to TSS in kth element of ratio_ss
  ratio_ss[k] <- LungCancer_km$tot.withinss
  
}

# Make a scree plot with type "b" and xlab "k"
plot(ratio_ss, type = 'b', xlab = 'k')
```
According to the plot above, k = 3 is the best choice.
```{r}
LungCancer_km_3 <- kmeans(Air_Quality_Lung_Cancer_Data$Lung.Cancer, 2, nstart = 20)

#plot kmeans
LungCancer_value_and_label <- data.frame(LungCancer_value = Air_Quality_Lung_Cancer_Data$Lung.Cancer, LungCancer_label = LungCancer_km_3$cluster)
LungCancer_value_and_label <- LungCancer_value_and_label %>% arrange(LungCancer_value)
plot(x = LungCancer_value_and_label$LungCancer_value, col = LungCancer_value_and_label$LungCancer_label, ylab = "Lung Cancer Mortality")

#add label to plot
Air_Quality_Lung_Cancer_Data$Lung.Cancer.level <- LungCancer_km_3$cluster

Air_Quality_Lung_Cancer_Data$Lung.Cancer.level[LungCancer_km_3$cluster == 2] = 0

write.csv(Air_Quality_Lung_Cancer_Data,"Air_Quality_Lung_Cancer_Data_01.csv", row.names = FALSE)
```

```{r}
#cross-validation knn
Air_Quality_Lung_Cancer_Data <- read.csv(file = 'Air_Quality_Lung_Cancer_Data_categorical.csv')

important_variables_7 <- Air_Quality_Lung_Cancer_Data[,c(6,8,9,10,18,19,24,31)]
important_variables_7$Lung.Cancer.level[important_variables_7$Lung.Cancer.level == 0] = 'low'
important_variables_7$Lung.Cancer.level[important_variables_7$Lung.Cancer.level == 1] = 'high'
set.seed(107)

inTrain <- createDataPartition(y = important_variables_7[,8], p = .6, list = FALSE)
training <- important_variables_7[inTrain,]
testing <- important_variables_7[-inTrain,]
nrow(training)
nrow(testing)

ctrl <- trainControl(method = "repeatedcv", repeats = 3)
kNNFit <- train(Lung.Cancer.level ~ PM2.5 + O3 + CO + Land_EQI + Sociod_EQI + Built_EQI + Water_EQI + PM2.5**2, 
                data = training,
                method = "knn",
                tuneLength = 15,
                trControl = ctrl,
                preProc = c("center", "scale"))
print(kNNFit)

#evaluation
knnPredict <- factor(predict(kNNFit, newdata = testing ))
#Get the confusion matrix to see accuracy value and other parameter values
testing$Lung.Cancer.level <- factor(testing$Lung.Cancer.level)
confusionmatrix <- confusionMatrix(knnPredict, testing$Lung.Cancer.level)
confusionmatrix$byClass
```
```

## 1.2 Classification Trees

prepare data
```{r}
lung <- fread('Air_Quality_Lung_Cancer_Data_categorical.csv')

# only keep the target and candidate variables
lungcancer <- lung[ , .(Lung.Cancer,PM2.5,SO2, NO2, O3, CO, CS2, Water_EQI, Land_EQI, Built_EQI, Sociod_EQI, EQI)]
# change the target variable to categorical
lungcancer= data.frame(lungcancer)
lungcancer[,"Lung.Cancer"] = ifelse(lungcancer[,"Lung.Cancer"]>70.3,1,0)
#add higher order term
lungcancer$PMX = lungcancer$PM2.5^2
# partition
set.seed(123)
train.index <- sample(c(1:dim(lungcancer)[1]), dim(lungcancer[1])*0.6)
train.lung <- lungcancer[train.index, ]
valid.lung <- lungcancer[-train.index, ]
```

Run Classification Tree
```{r,warning=FALSE, message=FALSE}
# use cross validation to run the classification tree
lung.tree <- rpart(Lung.Cancer ~ ., data = train.lung, method = "class",cp = 0.00001, minsplit = 5, xval = 5)
# argument cp sets the smallest value for the complexity parameter
printcp(lung.tree)
# prune by lower cp
pruned.lung <- prune(lung.tree,
    cp = lung.tree$cptable[which.min(lung.tree$cptable[,"xerror"]),"CP"])
# length of nsplit
length(pruned.lung$frame$var[pruned.lung$frame$var == "<leaf>"])
# plot tree
prp(pruned.lung, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

# make prediction on validation data
valid.tree <- predict(pruned.lung,valid.lung,type = "class")
# make confusion matrix
a = confusionMatrix(table(valid.tree,valid.lung$Lung.Cancer))
a
a$byClass
```

# 1.3 Logsitic Regression
```{r}
# load data
Air_Quality_Lung_Cancer_Data <- read.csv('Air_Quality_Lung_Cancer_Data_categorical.csv')
names(Air_Quality_Lung_Cancer_Data)[1] <- 'Lung_Cancer_Mortality'

# select variables based on previous work

Selected_Cancer_Data <- Air_Quality_Lung_Cancer_Data[,c(5,6,8,9,10,16,17,18,19,22,24,31)]

```

```{r}
# partition data
set.seed(1)
train.index <- sample(c(1:dim(Selected_Cancer_Data)[1]), dim(Selected_Cancer_Data)[1]*0.6)  
train.df <- Selected_Cancer_Data[train.index, ]
valid.df <- Selected_Cancer_Data[-train.index, ]

```


```{r}

# Logistic Regression
logit.reg <- glm(formula = Lung.Cancer.level ~ PM2.5 + O3 + CO + Land_EQI + Sociod_EQI + Built_EQI + Water_EQI + I(PM2.5^2), family = binomial, data = train.df)
    
options(scipen=999)
summary(logit.reg)
logit.reg.pred0 <- predict(logit.reg, valid.df, type = "response")
confusionMatrix(as.factor(ifelse(logit.reg.pred0 > 0.5, 1, 0)),
                  as.factor(valid.df$Lung.Cancer.level))
# residual plot
plot(predict(logit.reg),residuals(logit.reg))
abline(h=0,lty=2,col="grey")

# Store the residuals as a new column in dataframe
train.df$Resid<-resid(logit.reg)
# Find out what 2 standard deviations is and save it to SD2
SD2<-2*sd(resid(logit.reg))
train.df$Outs<-ifelse(abs(train.df$Resid)>SD2, 1, 0)
plot(train.df$Resid, col=train.df$Outs+1, pch=16,ylim=c(-3,3))
#Make a new data.frame with no outliers
train.df1<-train.df[!train.df$Outs,]
nrow(train.df1)
nrow(train.df)
# Plot new data
plot(train.df1$Resid, col=train.df1$Outs+1,pch=16, ylim=c(-3,3))

logit.reg1 <- logit.reg <- glm(formula = Lung.Cancer.level ~ PM2.5 + O3 + CO + Land_EQI + Sociod_EQI + Built_EQI + Water_EQI + I(PM2.5^2), family = binomial, data = train.df1)

summary(logit.reg1)

# ConfusionMatrix 
logit.reg.pred <- predict(logit.reg1, valid.df, type = "response")
cm<-confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)),
                  as.factor(valid.df$Lung.Cancer.level))
# F1 score
cm$byClass[7] 



```


```{r}
exp(coef(logit.reg1))
```
