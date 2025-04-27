---
  title: "Quiz7"
author: "Srinivasa Phani Madhav Marupudi"
date: "2024-04-05"
output: pdf_document
---
  
  1. 
```{r}
library(rpart)
library(caTools)
library(caret)
library(glmnet)
library(rattle)
library(elasticnet)

data<-read.csv("C:/Users/MSP/Downloads/Ames_Housing_Data(1).csv", header=T)
head(data)
cat("Number of rows with missing values", sum(is.na(data)))
data<-data[complete.cases(data),]
```

2. 
```{r}
set.seed(123)
split<-createDataPartition(data$SalePrice,p=0.75,list=F)
train.data<-data[split, ]
test.data<-data[-split, ]
```

3. 
```{r}
x <- model.matrix(SalePrice~., train.data)[,-1]
y <- train.data$SalePrice
cv <- cv.glmnet(x, y, alpha = 0)
cv$lambda.min
best_lambda <- cv$lambda.min

model = glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(model)

x.test <- model.matrix(SalePrice ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
data.frame(
  RMSE = RMSE(predictions, test.data$SalePrice),
  Rsquare = R2(predictions, test.data$SalePrice)
)
```

4. 
```{r}
cv <- cv.glmnet(x, y, alpha = 1)
cv$lambda.min

model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min) 
coef(model)

x.test <- model.matrix(SalePrice ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
data.frame(
  RMSE = RMSE(predictions, test.data$SalePrice),
  Rsquare = R2(predictions, test.data$SalePrice)
)
```

5
```{r}
model <- train(
  x, y, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
model$bestTune
# 
# coef(model$finalModel, model$bestTune$lambda)
# x.test <- model.matrix(SalePrice ~., test.data)[,-1]
# predictions <- model %>% predict(x.test)

coefficients <- coef(model$finalModel, model$bestTune$lambda)

# Prepare test data for prediction
x.test <- model.matrix(SalePrice ~ ., test.data)[,-1]
predictions <- predict(model, x.test)
data.frame(
  RMSE = RMSE(predictions, test.data$SalePrice),
  Rsquare = R2(predictions, test.data$SalePrice)
)
```