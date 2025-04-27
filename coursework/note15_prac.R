f <- function(x){
  x^3-2*x-5
}

x1 <- seq(-4,4,length=100)
plot(x1,f(x1),type='l')
abline(h=0,col='purple')

fprime <- function(x) {
  3*x^2 - 2
}

eps  <- 1e-6
xr <- c(0,1)
t <- 2

while(abs(xr[t]-xr[t-1])>eps){
  curx <- xr[t]-f(xr[t])/fprime(xr[t])
  xr <- c(xr,curx)
  t <- t+1
  
}
t
xr
uniroot(f,c(-1,4))


###############################################################################

library(stats4)
y <- c(0,0.04,0.5,0.8,0.06)
theta.hat <- 1/mean(y)
theta.hat

mlogl <- function(theta){
  return(-length(y)*log(theta)+theta*sum(y))
}
mle(mlogl,start = list(theta = 1))


###############################################################################

f <- function(x){
  log(x+log(x))/log(1+x)
}
x1 <- seq(1,8,length=1000)
plot(x1,f(x1),type = 'l')
optimize(f,c(1,8),maximum = T)

x1 <- seq(2,8,length=1000)
plot(x1,f(x1),type = 'l')

x1 <- seq(4,8,length=1000)
plot(x1,f(x1),type = 'l')

abline(v = 5.792316)

###############################################################################
set.seed(123)
r.true <- 5
lambda.true <- 2
x<- rgamma(50,shape = r.true,rate = lambda.true)
  f <- function(lambda,x){
  log(lambda)-digamma(lamdba*mean(x))+mean(log(x))
  }
  
  res1 <- uniroot(f,c(0.001,10000))
  
  lambda.hat <- res1$root
  r.hat <- mean(x)*lambda.hat
  c(lambda.hat,r.hat)