n <- 1000
z <- sample(c(0,1),n,replace = TRUE,prob=c(0.25,0.75))
x1 <- rep(NA,n)
x1[z==0] <- rnorm(length(which(z==0),1,0.5))
x1[z==1] <- rnorm(length(which(z==1),1,0.3))



Estep <- function(mu0,mu1,sigma0,sigma1,p1,x){
  taui /,- p1*fnorm(x,mu1,sigma1)/((1))
}




Mstep <- function(taui,x)







#initialization
init.mu0 <- 0
init.mu1 <- 1
init.sigma0 <- init.sigma1 <- 1
init.p1 <- 0.5

cur.Estep <- Estep(init.mu0,init.mu1,init.sigma0,init.sigma1,init.p1,x1)
cur.Mstep <- Mstep(cur,Estep$taui,x1)

all.loglike <- c(0,cur.Estep$loglike)
eps <- 10e-6

while(abs(all.loglike[k+1]-all.loglike[k]>eps)){
  cur.Estep <- Estep(cur.Mstep$mu0,cur.Mstep$mu1,cur.Mstep$sigma0,cur.Mstep$sigma1,cur.Mstep$p1,x1)
  k <- k+1

  }

k
