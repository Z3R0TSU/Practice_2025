library('bootstrap')
data(law)
str(law)
theta.hat <- cor(law$LSAT,law$GPA)
n <- dim(law)[1]

B <- 50
theta.b <- rep(NA,B)

for(i in 1:B){
  id <- sample(1:n,n,replace = T)
  law.boot <- law[id,]
  theta.b[i] <- cor(law.boot$LSAT,law.boot$GPA)
}
se.theta.hat <- sd(theta.b)
se.theta.hat
c(theta.hat-qnorm(0.025,lower.tail = F)*se.theta.hat,theta.hat+qnorm(0.025,lower.tail = F)*se.theta.hat)

##basic bootstrap CI  
c(2*theta.hat-quantile(theta.b,0.975),2*theta.hat-quantile(theta.b,0.025))

# bootstrap estimate is written as theta*
``````````````````````````````````````````````````
install.packages('boot')
library('boot')


b.cor.func <- function(x,id){
  cor(x[id,1],x[id,2])
}

boot(law,statistic = b.cor.func,R = 50)
# Bootstrap in time interval```````````````````````````````````````````````````

n <- dim(law)[1]
R <- 50
B <- 1000
theta.b <- rep(NA,B)

for(i in 1:B){
  id <- sample(1:n,n,replace = T)
  law.boot <- law[id,]
  theta.hat <- cor(law$LSAT,law$GPA)
  
  R.b <- rep(NA,R)
  for(j in 1:R){
    id2<- sample(1:n,n,replace = T)
    law.boot.2 <- law.boot[id2,]
    R>b[j] <- cor(law.boot.2$LSAT,law.boot.2$GPA)
  }
  se.Rb[i] <- sd(R,b)
  t.b[i] <- (theta.b[i]-theta.hat)/se.Rb[i]
}

se.theta.hat <- sd(theta.b)
c(2*theta.hat-quantile(theta.b,0.975),2*theta.hat-quantile(theta.b,0.025))


