# Simulate 10000 N (0, 1) using the Box Muller method.


u1 <- runif(5000)
u2 <- runif(5000)
x1 <- sqrt(-2*log(u1))*cos(2*pi*u2)
x2 <- sqrt(-2*log(u2))*cos(2*pi*u2)
x <- c(x1,x2)
hist(x)
qqnorm(x)
abline(0,1,col='purple')


#  Simulate 1000 chi-squared r.v. with degrees of freedom 3 using sum of r.v’s method. Use only random uniform generator.

u1 <- runif(1500)
u2 <- runif(1500)
x1 <- sqrt(-2*log(u1))*cos(2*pi*u2)
x2 <- sqrt(-2*log(u2))*cos(2*pi*u2)
x <- c(x1,x2)
w<- apply(matrix(x^2,ncol = 3),1,sum)
wo <- rchisq(1000,3)
qqplot(w,wo)
abline(0,1,col = 'green')

# Simulate 10000 r.v from mixture of normals 0.2N (0, 1) + 0.5N (−1, 1) + 0.3N (2, 1)

K <- samles(1:3.10000,prob = (0.2,0.5,0.3),replace = T)
x <- rep(NA,10000)

for(i in 1:10000) {
  if(k[i]==1)x1[i] <- rnorm(1,0,1)                                           
  if(k[i]==2)x1[i] <- rnorm(1,-1,1)
  if(k[i]==3)x1[i] <- rnorm(1,2,1)

x[k==1] <- rnorm(lenght(which(k==1)),0,1)
x[k==2] <- rnorm(lenght(which(k==1)),-1,1)
x[k==3] <- rnorm(lenght(which(k==1)),2,1)

#  Simulate 10000 negative binomial with p = 0.4 and r = 10 using Gamma-Poisson mixture

p <- 0.4 
r <- 10
n<- 10000
lamda1 <- rgamma(n,r,scale=p/(1-p))
x<- rpois(n,lamda1)
table(x)
mean(x)
var(x)
 


