# Hypothesis test
set.seed(123)

n <- c(20,25,30,35,40,45,50,55,60,65)
m <- c(161:170)
beta <- rep(NA, length(n))
for (k in 1:length(n)) {
  T.rand <- rep(NA, m)
  T.rand.pwr <- rep(NA, m)
  for (i in 1:length(T.rand)) {
    x <- rnorm(n, n[k], sd.est)
    T.rand.pwr[i] <- (mean(x) - 160) * sqrt(n)/sd(x)
  }
  cL <- quantile(T.rand, 0.05)
  cH <- quantile(T.rand, 0.95)
  beta[k] <- length(which(T.rand.pwr >
                            cL & T.rand.pwr < cH))/m
}






####################################################

qnorm()


##########

set.seed(123)
n < 20
m <- 15
x <- rnorm(20)
y <- rnorm(15,1.5)
obs.data <- list(x=x, y=y)
t.test(x,y,var.equal=T)

t0 <- (mean(x) - mean(y))/sqrt(var(x)/n+var(y)/m)