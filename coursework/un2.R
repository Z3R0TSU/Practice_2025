library(tidyverse)
library(caret)
library(glmnet)
library(MASS)

data1 = read.csv("C:/Users/MSP/Downloads/math.csv", sep = ";")
data1 <- subset(data1, select = - c(G1,G2))
head(data1)


set.seed(123)
training.samples <- data1$G3 %>% createDataPartition(p = 0.75, list = FALSE)
train.data  <- data1[training.samples, ]
test.data <- data1[-training.samples, ]
train.data
test.data

fit <- lm(G3~., data = train.data)
fit_step <- stepAIC(fit, k = log(nrow(train.data)), trace = 1)
pred <- fit_step %>% predict(test.data)
data.frame(
  RMSE = RMSE(pred, test.data$G3),
  Rsquare = R2(pred, test.data$G3)
)


library(leaps)
fit_bs <- regsubsets(G3~., data = train.data, nvmax = 30)
result <- summary(fit_bs)
which.min(result$bic)
result$which[3,]
fit_bs <- lm(G3~Mjob +  failures + romantic, data = train.data)
pred <- fit_bs %>% predict(test.data)
data.frame(
  RMSE = RMSE(pred, test.data$G3),
  Rsquare = R2(pred, test.data$G3)
)
mod1 <- lm(G3~Medu +  failures + romantic, data = train.data)
mod2 <- lm(G3~Mjob +  failures + romantic, data = train.data)
BIC(mod1)
BIC(mod2) 
summary(mod1)
summary(mod2) 
