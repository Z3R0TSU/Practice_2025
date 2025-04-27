
# Q1

df1 <- 5
df2 <- 10
n <- 1000  

set.seed(123)

uni_sample1 <- runif(1000)
uni_sample2 <- runif(1000)

chi_sample1 <- -2 * log(uni_sample1)
chi_sample2 <- -2 * log(uni_sample2)
f_sample <- (chi_sample1 / df1) / (chi_sample2 / df2)

print(f_sample)


act = rf(n, df1, df2)

qqplot(f_sample, act)
abline(0,1)

# Q2

my.polar = function(n){
  res = rep(0, n)
  count = 1
  Z0 = rep(NA, n)
  Z1 = rep(NA, n)
  while(count <= n) {
    while(TRUE){
      v1 = 2 * runif(1) - 1
      v2 = 2 * runif(1) - 1
      
      s = v1 ^ 2 + v2 ^ 2
      if(s > 1){
        next
      }
      else{
        break
      }
    }
    z0 = v1 * sqrt(-2 * log(s)/s)
    z1 = v2 * sqrt(-2 * log(s)/s)
    Z0[count] = z0
    Z1[count] = z1
    count = count + 1
  }
  
  
  return(list(Z0 = Z0, Z1 = Z1))
}

n = 10000
res = my.polar(10000)

mean_Z0 <- mean(res$Z0)
sd_Z0 <- sd(res$Z0)
mean_Z1 <- mean(res$Z1)
sd_Z1 <- sd(res$Z1)

cat("Mean of Z0:", mean_Z0, "\n")
cat("Standard deviation of Z0:", sd_Z0, "\n")
cat("Mean of Z1:", mean_Z1, "\n")
cat("Standard deviation of Z1:", sd_Z1, "\n")

par(mfrow=c(1,2))
hist(res$Z0, main="Histogram of Z0")
hist(res$Z1, main="Histogram of Z1")

plot(res$Z0, res$Z1, xlab="Z0", ylab="Z1", main="Scatterplot of Z0 vs Z1")

# Q3

rchisq_unif <- function(n, df) {
  U <- runif(n)
  X <- -2 * log(U) / df
  X[X < 0] <- 0  
  return(X)
}


standard_model <- function() {
  
  while (TRUE) {
    U1 <- 2 * runif(1) - 1
    U2 <- 2 * runif(1) - 1
    S <- U1^2 + U2^2
    
    if (S < 1) {
      break
    }
  }
  
  Z <- U1 * sqrt(-2 * log(S) / S)
  
  return(Z)
}


mixture_t <- function(n) {
  samples <- numeric(n)
  components <- c(3, 5, 7)
  probabilities <- c(0.3, 0.35, 0.35)
  
  for (i in 1:n) {
    u <- runif(1)
    component <- ifelse(u <= 0.3, 3, ifelse(u <= 0.65, 5, 7))
    
    W <- rchisq_unif(1, df = component)
    Z <- standard_model()
    samples[i] <- Z * sqrt(W / component)
  }
  
  return(samples)
}

set.seed(123)
res = mixture_t(100)
res

set.seed(123)  
builtin_samples <- 0.3 * rt(1000, df = 3) + 0.35 * rt(1000, df = 5) + 0.35 * rt(1000, df = 7)

qqplot(res, builtin_samples)
abline(0, 3)

# Q4

rmultivarNorm <- function(n, mu, Sigma) {
  p <- length(mu)
  z <- matrix(runif(n * p), ncol = p)
  L <- matrix(0, nrow = p, ncol = p)
  
  for (i in 1:p) {
    for (j in 1:i) {
      if (i == j) {
        L[i, i] <- sqrt(Sigma[i, i] - sum(L[i, 1:(i-1)]^2))
      } else {
        L[i, j] <- (Sigma[i, j] - sum(L[i, 1:(j-1)] * L[j, 1:(j-1)])) / L[j, j]
      }
    }
  }
  
  x <- z %*% t(L)
  x <- t(t(x) + mu)
  
  return(x)
}

mu <- c(1, 2)
Sigma <- matrix(c(2, 1, 1, 3), nrow = 2)

n <- 100
samples <- rmultivarNorm(n, mu, Sigma)
cat("Mean of generated samples:\n")
print(colMeans(samples))
cat("Covariance of generated samples:\n")
print(cov(samples))


# Q5

data(ChickWeight, package = "datasets")


my_ls <- function(y, x1, x2) {

  X <- model.matrix(~ x1 + x2, data = data.frame(x1 = x1, x2 = x2))
  coef <- solve(t(X) %*% X) %*% t(X) %*% y
  return(coef)
}

fit <- my_ls(ChickWeight$weight, ChickWeight$Time, ChickWeight$Diet)
print(fit)

# Q6

library(MASS)
library(mgcv)
attach(mcycle)

spline_model <- gam(accel ~ s(times), data = mcycle)
spline_preds <- predict(spline_model)
mse_spline <- mean((mcycle$accel - spline_preds)^2)

cat("MSE of spline model:", mse_spline, "\n")

bic <-c()
for (i in 1:20){
  bic=c(bic,BIC(lm(accel~poly(times,i,raw=T), data = mcycle)))
}
bic

best_degree <- which.min(bic)  

poly_model <- lm(accel ~ poly(times, best_degree, raw = TRUE), data = mcycle)
poly_preds <- predict(poly_model)
mse_poly <- mean((mcycle$accel - poly_preds)^2)

cat("MSE of polynomial regression model:", mse_poly, "\n")

