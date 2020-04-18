---
title: "Big Data Project 2"
author: "No Error"
date: "2/29/2020"
output: html_document
---
Load packages 
```{R}
library(data.table)
library(tidyverse)
library(GGally)
library(caret)
library(leaps)
library(car)
options(scipen = 200)
```

# Part 1: Descriptive Statistics 
Import dataset
```{R}
dta <- fread('Air_Quality_Lung_Cancer_Data.csv')
dta_key_variables <- dta[ , .(`Lung Cancer`, State, PM2.5, PM10, SO2, NO2, O3, CO, CS2, Air_EQI, Water_EQI, Land_EQI, Built_EQI, Sociod_EQI, EQI)]


descriptive <- data.frame(min = 0, mean = 0, median = 0, max = 0, std = 0)
ind <- c(1,3:15)
for (i in 1:14) {
  j = ind[i]
  column = as.data.frame(dta_key_variables)[ , I(j)]
  descriptive[i, ] = c(min(column), mean(column), median(column), max(column), sd(column))
}
name = c("Lung Cancer", "PM2.5", "PM10", "SO2", "NO2", "O3", "CO", "CS2", "Air_EQI", "Water_EQI", "Land_EQI", "Built_EQI", "Sociod_EQI", "EQI")
descriptive <- cbind(name, descriptive)
```

## 1.1 Descriptive statistics for relevant variables 
```{R}
lungcancer <- ggplot(dta, aes(x = `Lung Cancer`)) + geom_histogram(fill = "#9DC3C1", color = "#6E7783", alpha = 0.8) + labs(x = "Lung Cancer Mortality", y = "Frequency")

#ggsave("lung cancer histogram.png", lungcancer)
```

```{R}
cs2 <- ggplot(dta, aes(x = "CS2", y = CS2)) + geom_boxplot(fill = "#feee7d", alpha = 0.5) + scale_y_log10() + labs(x = "", y = "")
#ggsave("boxplot cs2.png", cs2)
```

```{R}
particles <- dta[ , .(PM2.5, PM10)]
particles <- melt(particles)

particiles_box <- ggplot(particles, aes(y = value, fill = variable)) + geom_boxplot(alpha = 0.7) + scale_fill_manual(values = c('#6d9d88', '#fec9c9'), name = ' ')  + scale_x_continuous(breaks = c(-0.2, 0 ,0.2), labels = c("PM2.5", "", "PM10")) + labs(y = "Concentrations")

#ggsave("particiles_box.png", particiles_box)
```

```{R}
EQI <- dta[ , .(FIPS_code, EQI, Water_EQI, Air_EQI, Land_EQI, Built_EQI, Sociod_EQI, EQI)]
EQI <- melt(EQI, id.vars = "FIPS_code")

EQI_box <- ggplot(EQI, aes(x = variable, y = value, fill = variable)) + geom_boxplot(alpha = 0.6) + scale_fill_manual(values = c("#D09E88", "#FADAD8", "#AF4034","#9B8281", "#F0E5DE", "#ABD0CE", "#7C7877"), name = "") + labs(x = "", y = "")
#ggsave("EQI box.png", EQI_box)
```
## 1.2 Scatter plot 
```{R}
wrap_1 <- wrap(ggally_points, size = 1, color = "#77919d", alpha = 0.3)
wrap_2 <- wrap(ggally_densityDiag, fill = "#77919d", alpha = 0.5)

mat <- ggpairs(dta[ , .(`Lung Cancer`, PM2.5, PM10, SO2, NO2, O3, CO, CS2, Air_EQI, Water_EQI, Land_EQI, Built_EQI, Sociod_EQI, EQI)])

mat1 <- ggpairs(dta[ , .(`Lung Cancer`, PM2.5, PM10)], lower = list(continuous = wrap_1), diag = list(continuous = wrap_2))
ggsave("mat1 particles.png", mat1)

mat2 <- ggpairs(dta[ , .(`Lung Cancer`, SO2, NO2, O3, CO, CS2)], lower = list(continuous = wrap_1), diag = list(continuous = wrap_2))
ggsave("mat2 emissions.png", mat2)

ggpairs(dta[ , .(`Lung Cancer`, log(SO2), log(NO2), log(O3), log(CO), log(CS2))], lower = list(continuous = wrap_1), diag = list(continuous = wrap_2))

#ggsave("mat.png", mat)

mat <- ggpairs(dta[ , .(`Lung Cancer`, PM2.5, PM10, Air_EQI, SO2, NO2, O3, CO, CS2)], lower = list(continuous = wrap_1), diag = list(continuous = wrap_2))

```

```{R}
mat_other <- ggpairs(dta[ , .(`Lung Cancer`, Water_EQI, Land_EQI, Built_EQI, Sociod_EQI, EQI)], lower = list(continuous = wrap_1), diag = list(continuous = wrap_2))

#ggsave("mat_other.png", mat_other)
```

# Part 2: Predictive modeling
```{r}
Air_Quality_Lung_Cancer_Data <- read.csv(file = 'Air_Quality_Lung_Cancer_Data.csv')
summary(Air_Quality_Lung_Cancer_Data)
```

```{r}
#load data
important_variables <- Air_Quality_Lung_Cancer_Data[,c(4,5,7,8,9,15,16,17,18,21,23)]
names(important_variables)[1] <- 'Lung_Cancer_Mortality'
#inspect data
sample_n(important_variables,3)
model_10_variables <- lm(Lung_Cancer_Mortality~., data = important_variables)
summary(model_10_variables)
model_10_variables.res <- resid(model_10_variables)
```

```{r}
vif(model_10_variables)
```

***best subset (exhaustive) search***
http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/155-best-subsets-regression-essentials-in-r/
```{r}
subsets_model <- regsubsets(Lung_Cancer_Mortality~., data = important_variables, nvmax = 10)
summary(subsets_model)
```

The function summary() reports the best set of variables for each model size. From the output above, an asterisk specifies that a given variable is included in the corresponding model.

For example, it can be seen that the best 2-variables model contains only PM2.5 and Sociod_EQI (Lung_Cancer_Mortality ~ PM2.5 + Sociod_EQI). The best three-variable model is (Lung_Cancer_Mortality ~ PM2.5 + Sociod_EQI + Built_EQI), and so forth.

A natural question is: which of these best models should we finally choose for our predictive analytics?

To answer to this questions, we need some statistical metrics or strategies to compare the overall performance of the models and to choose the best one. We need to estimate the prediction error of each model and to select the one with the lower prediction error.

**Model selection criteria: Adjusted R2, Cp and BIC**
The summary() function returns some metrics - Adjusted R2, Cp and BIC (see Chapter @ref(regression-model-accuracy-metrics)) - allowing us to identify the best overall model, where best is defined as the model that maximize the adjusted R2 and minimize the prediction error (RSS, cp and BIC).

The adjusted R2 represents the proportion of variation, in the outcome, that are explained by the variation in predictors values. the higher the adjusted R2, the better the model.

The best model, according to each of these metrics, can be extracted as follow:
```{r}
res.sum <- summary(subsets_model)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)
```

**K-fold cross-validation**
The k-fold Cross-validation consists of first dividing the data into k subsets, also known as k-fold, where k is generally set to 5 or 10. Each subset (10%) serves successively as test data set and the remaining subset (90%) as training data. The average cross-validation error is computed as the model prediction error.

The k-fold cross-validation can be easily computed using the function train() [caret package] (Chapter @ref(cross-validation)).

Here, we’ll follow the procedure below:

Extract the different model formulas from the models object
Train a linear model on the formula using k-fold cross-validation (with k= 10) and compute the prediction error of each model

**We start by defining two helper functions:**

1. get_model_formula(), allowing to access easily the formula of the models returned by the function regsubsets(). Copy and paste the following code in your R console:
```{r}
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}
```

```{r}
get_model_formula(9, subsets_model, "Lung_Cancer_Mortality")
```

2. get_cv_error(), to get the cross-validation (CV) error for a given model:
```{r}
get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 10)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}
```

Finally, use the above defined helper functions to compute the prediction error of the different best models returned by the regsubsets() function:

```{r}
# Compute cross-validation error
model.ids <- 1:10
cv.errors <-  map(model.ids, get_model_formula, subsets_model, "Lung_Cancer_Mortality") %>%
  map(get_cv_error, data = important_variables) %>%
  unlist()
cv.errors
```
```{r}
# Select the model that minimize the CV error
which.min(cv.errors)
```

It can be seen that the model with 7 variables is the best model. It has the lower prediction error. The regression coefficients of this model can be extracted as follow:
```{r}
coef(subsets_model, 7)
```

```{r}
subsets_model_7 <- lm(Lung_Cancer_Mortality ~ PM2.5 + O3 + CO + Land_EQI + Sociod_EQI + Built_EQI + Water_EQI, data = important_variables)
summary(subsets_model_7)
```
**forward-and-backward selection**
```{r}
step_model <- lm(Lung_Cancer_Mortality~., data = important_variables)
summary(step_model)
```

```{r}
tstep<-step(step_model)
summary(tstep)
```

```{r}
drop1(tstep)
```

```{r}
step_model_6 <- lm(Lung_Cancer_Mortality~PM2.5+Land_EQI+Sociod_EQI+Built_EQI+O3+CO, data = important_variables)
summary(step_model_6)
drop1(step_model_6)
```
So for this dataset, the models built by step selection and best subsets selection are the same.
**plot**
```{r}
subsets_model_6 <- lm(Lung_Cancer_Mortality ~ PM2.5 + O3 + CO + Land_EQI + Sociod_EQI + Built_EQI, data = important_variables)
summary(subsets_model_6)
subsets_model_6.res <- resid(subsets_model_6)
```

```{r}
hist(subsets_model_6.res, breaks = 20, col = "lavenderblush2", xlab = "Residuals" , ylab = "Frequency", main = "Histogram of Residuals")
```

residuals plot

```{r}
plot(subsets_model_6)
```


```{r}
vif(subsets_model_6)
```

VIF is still good

```{r}
#delete outliers
standarlized_residual <- as.data.frame(scale(subsets_model_6$residual))
standarlized_residual$serial <- 1:length(standarlized_residual[,1])
#standarlized_residual %>% arrange(V1)
#standarlized_residual %>% arrange(desc(V1))
```
Point number: 649,6,1518,1876,1872,1851

```{r}
important_variables_6 <- Air_Quality_Lung_Cancer_Data[,c(4,5,7,8,9,17,18)]
important_variables_6 <- important_variables_6[-c(649,6,1518,1876,1872,1851),]
names(important_variables_6)[1] <- 'Lung_Cancer_Mortality'
```

Build the model again
```{r}
subsets_model_without_outliers_v1 <- regsubsets(Lung_Cancer_Mortality~., data = important_variables_6, nvmax = 6)
summary(subsets_model_without_outliers_v1)
```
```{r}
res.sum_new <- summary(subsets_model_without_outliers_v1)
data.frame(
  Adj.R2 = which.max(res.sum_new$adjr2),
  CP = which.min(res.sum_new$cp),
  BIC = which.min(res.sum_new$bic)
)
```

```{r}
subsets_model_final <- lm(Lung_Cancer_Mortality ~ ., data = important_variables_6)
summary(subsets_model_final)
subsets_model_final.res <- resid(subsets_model_final)
```
```{r}
sqrt(mean(subsets_model_final$residuals^2))
```

```{r}
hist(subsets_model_final.res, breaks = 20, col = "lavenderblush2", xlab = "Residuals" , ylab = "Frequency", main = "Histogram of Residuals")
```


```{r}
par(mfrow=c(2,2))
plot(subsets_model_final)
```

```{R}
# Use the pairs() plot to determine which variables may benefit from a quadratic relationship with the response.
pairs(important_variables, col = "dodgerblue")
# Get the new database containing only targeted variables
choosen_vairiables <- important_variables[ ,-c(6,7,10)]
#Delete the outliers
choosen_variables_7 <- choosen_vairiables[-c(649,6,1518,1876,1872,1851),]
# Build the regression model
higher_model_7 = lm(
  Lung_Cancer_Mortality ~ .^ 2 + I(PM2.5 ^ 2) + I(O3 ^ 2) + I(CO ^ 2) + I(Land_EQI ^ 2) + I(Sociod_EQI ^ 2) + I(Built_EQI ^ 2) + I(Water_EQI ^ 2),
  data = choosen_variables_7
)
summary(higher_model_7)

autompg_mod_back_aic = step(higher_model_7, direction = "backward", trace = 0)

n = length(resid(higher_model_7))
autompg_mod_back_bic = step(higher_model_7, direction = "backward", 
                            k = log(n), trace = 0)
coef(autompg_mod_back_aic)
coef(autompg_mod_back_bic)

calc_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}
calc_loocv_rmse(higher_model_7)
calc_loocv_rmse(autompg_mod_back_aic)
calc_loocv_rmse(autompg_mod_back_bic)

length(coef(higher_model_7))
length(coef(autompg_mod_back_aic))
length(coef(autompg_mod_back_bic))

summary(higher_model_7)$adj.r.squared
summary(autompg_mod_back_aic)$adj.r.squared
summary(autompg_mod_back_bic)$adj.r.squared

autompg_mod_back_aic

#RMSE
sqrt(mean(autompg_mod_back_aic$residuals^2))
```

