
# Name : Srinivasa Phani Madhav Marupudi 
# ID : 116036809

# 1
# a
library(caret)
dat <- read.delim('https://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/leukemiaDataSet.txt', header = TRUE, sep = '\t')

str(dat)

set.seed(123)
trainID <- sample(1:72,round(0.7*72))

trainData <- dat[trainID,]
testData <- dat[-trainID,]

#b
bmodel <- train(Group ~ ., data = trainData, method = "rf", trControl = trainControl(method = "oob"))
bmodel
summary(bmodel)
plot(bmodel)

#c
predictions <- predict(bmodel, newdata = testData)

AML <- sum(predictions[testData$Group == "AML"] == "AML") / sum(testData$Group == "AML") * 100
cat("The percentage of AML correctly predicted is :", AML, "%\n")

ALL <- sum(predictions[testData$Group == "ALL"] == "ALL") / sum(testData$Group == "ALL") * 100
cat("The percentage of ALL correctly predicted is :", ALL, "%\n")

overall_accuracy <- sum(predictions == testData$Group) / length(testData$Group) * 100
cat("The overall percentage of AML and ALL correctly predicted is :", overall_accuracy, "%")

Conf_matrix <- table(predictions, testData$Group)


# 2 

func2 <- function(n, lambda, nu) {
  u <- runif(n)  
  x <- -1/lambda * log(1 - u) + nu  
  return(x)
}

# Generate a random sample of size 1000 from the Exp(2, 1) distribution.
set.seed(123)
lambda <- 2
nu <- 1
sample_size <- 1000
exp_sample <- func2(sample_size, lambda, nu)
head(exp_sample)
plot(exp_sample)

# to compare with written function and check the deviations from the actual values 
acv2 <- rexp(1000,rate = 1/2)
qqplot(exp_sample, acv2)
abline(-4, 4, col = "purple")

# 3

func3 <- function(n) {
  u <- runif(n)
  x <- tan(pi * (u - 0.5))  
  return(x)
}

# Generate a random sample of size 1000 from the Cauchy distribution.
set.seed(123)
sample_size <- 1000
Cauchy_sample <- func3(sample_size)
head(Cauchy_sample)
plot(Cauchy_sample)
acv3 <- rcauchy(1000)

# to compare with written function and check the deviations from the actual values 
qqplot(Cauchy_sample, acv3)
abline(0,1 ,col = "purple")

# 4 

func4 <- function(n, a, b) {
  samples <- numeric(n)
  i <- 1
  while (i <= n) {
    x <- runif(1) 
    u <- runif(1)  
    pdf_x <- x^(a-1) * (1-x)^(b-1)  
    c <- (a-1)/(a+b-2)  
    
    if (u <= pdf_x / c) {
      samples[i] <- x
      i <- i + 1
    }
  }
  return(samples)
}

# Generate a random sample of size 1000 from the Beta(3, 2) distribution.
set.seed(123)
a <- 3
b <- 2

sample_size <- 1000
beta_sample <- func4(sample_size, a, b)
head(beta_sample)
plot(beta_sample)

acv4 <- rbeta(1000,3,2)
qqplot(beta_sample,acv4)

# 5

func5 <- function(n, alpha) {
  samples <- numeric(n)
  i <- 1
  while (i <= n) {
    x <- rexp(1, rate = 1/alpha)
    
    pdf_x <- dgamma(x, shape = alpha, rate = 1)  
    pdf_proposal <- dexp(x, rate = 1/alpha)  
    c <- (pdf_x * alpha) / pdf_proposal  
    
    u <- runif(1)
    
    if (u <= c) {
      samples[i] <- x
      i <- i + 1
    }
  }
  return(samples)
}

# Generate a random sample of size 1000 from the Gamma(3, 1) distribution.
set.seed(123)
alpha <- 3
sample_size <- 1000

gamma_sample <- func5(sample_size, alpha)
head(gamma_sample)
plot(gamma_sample)
gam <- rgamma(1000,3)
qqplot(gamma_sample,gam)


















