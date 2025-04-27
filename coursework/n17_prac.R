# Metropolis Hastings sampler
set.seed(123)
n <- 10000
sigma <- 4
x <- numeric(n)
x[1] <- rchisq(1, df = 1)
k <- 0
u <- runif(n)

for  (i in 2:m) {
  xt <- x[i - 1]
  y <- rchisq(1,df = xt)
  num <- f(y,sigma) * dchisq(xt,df = y)
  den <- f(xt,sigma) * dchisq(y,df = y)
  if (u[i] <= num/den)
    x[i] <- y else{
      x[i] <- xt

      k <- k+1  # y is rejected
      }
}

x1 <- sigma*sqrt(-2*log(runif(m)))
  

###########################################


