
set.seed(123)
X <- rexp(1000, rate = 2)
Y <- rexp(1000, rate = 2)

qqplot(X, Y,)
abline(0, 1, col = "red")  

lambda <- 2
n <- 10000
u  <- runif(n)
x <- -log(u)/lambda

x.true <- rexp(n,lambda)
qqplot(x,x.true)
abline(0,1,col = 'red')

set.seed(123)
U <- runif(1000)
X <- sqrt(2 * U)
X
qqplot(X,U)
abline(0,1,col='red')

 
set.seed(123)
U <- runif(10000)
X <- qnorm(U)


phi.inv <- function(u){
  t2 <- -2*log(u)
  t <- sqrt(t2)
  return(t-(2.30753+0.27061*t)/(1+0.99229*t+0.04481*t2))
}

n <- 10000
u <- runif(n)
x <- phi.inv(u)
x


n <- 10000
k <- 0
j <- 0
x <- rep(NA,n)

while(k<n){
  y <- runif(1)  # Step1
  j <- j+1
  u <- runif(1) # Step2
  if(u<=y){ # Step 3
    k <- k+1
    x[k] <- y
  }
  
}




x <- rep(NA,n)
while (k<n){
  y <- runif(1)
  j <- j+1
  u <- runif(1)
  if(u<=4*y*(1-y)){
    k <- k+1
    x[k] <- y
  }
}
j
hist(x)
x.gold = rbeta(10000,2,2)
qqplot(x,x.gold)
abline(0,1,col='red')

n <- 10000
c <- 1.5
x <- rep(NA,n)
k <- 0
j <- 0

while (k<n){
  j <- j+1
  y <- runif(1)
  u <- runif(1)
  if(u<=y*(1-y)){
    k <- k+1
    x[k] <- y
  }
}

print(paste("Average number of iterations needed with c=1.5: ", j))
  
