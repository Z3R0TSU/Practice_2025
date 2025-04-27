# 1
N <- 100000

x_values <- runif(N, 0, pi/3)
sin_values <- sin(x_values)
average_sin <- mean(sin_values)

estimate <- (pi/3) * average_sin
cat("Monte Carlo estimate:", estimate, "\n")

exact_value <- 1 - cos(pi/3)
cat("Exact value of the integral:", exact_value, "\n")

error <- abs(estimate - exact_value)
cat("Error:", error, "\n")

#2

# a 
N <- 100000

x_uniform <- runif(N, 0, 0.5)
e_minus_x_uniform <- exp(-x_uniform)
average_e_minus_x_uniform <- mean(e_minus_x_uniform)
estimate_uniform <- 0.5 * average_e_minus_x_uniform
variance_uniform <- var(e_minus_x_uniform)

cat("Monte Carlo estimate Uniform distribution:", estimate_uniform, "\n")
cat("Variance of the estimate Uniform distribution:", variance_uniform, "\n\n")

# b

x_exponential <- rexp(N, rate = 1)
e_minus_x_exponential <- exp(-x_exponential)
average_e_minus_x_exponential <- mean(e_minus_x_exponential)
estimate_exponential <- average_e_minus_x_exponential
variance_exponential <- var(e_minus_x_exponential)

cat("Monte Carlo estimate with Exponential distribution:", estimate_exponential, "\n")
cat("Variance of the estimate with Exponential distribution:", variance_exponential, "\n\n")

cat("Comparison of variances:\n")
if (variance_uniform < variance_exponential) {
    cat("Variance from Uniform distribution is smaller.\n")
} else {
    cat("Variance Exponential distribution is smaller.\n")
}

# 3

# a
my.pbeta1 <- function(x, a, b, n=10000) {
    u <- runif(n, 0, x)
    Y <- qbeta(u, a, b)
    F_x <- mean(Y)
    return(F_x)
}

# b
my.pbeta2 <- function(x, a, b, n=10000) {
    u <- rgamma(n, a, 1)
    v <- rgamma(n, b, 1)
    y <- u / (u + v)
    return(mean(y <= x))
}

# c
x_values <- seq(0.1, 0.9, by=0.1)

for (x in x_values) {
    estimate1 <- my.pbeta1(x, 3, 3)
    estimate2 <- my.pbeta2(x, 3, 3)
    true_val <- pbeta(x, 3, 3)

    cat("x =", x, "\n")
    cat("Estimateof pbeta1:", estimate1, "\n")
    cat("Estimate of pbeta2:", estimate2, "\n")
    cat("True value:", true_val, "\n\n")
}

#4

# a

set.seed(123)
n_sim <- 1000
n <- 20
alpha <- 0.05

compute_t_test_pvalue <- function(x) {
    t <- (mean(x) - 0) / (sd(x) / sqrt(n))
    p_value <- 2 * pt(-abs(t), df = n - 1)
    return(p_value)
}

compute_wilcoxon_pvalue <- function(x) {
    p_value <- wilcox.test(x, mu = 0, alternative = "two.sided", exact = TRUE)$p.value
    return(p_value)
}

t_test_p_values <- numeric(n_sim)
wilcoxon_p_values <- numeric(n_sim)

for (i in 1:n_sim) {

    data <- rnorm(n, 0, 1)
    t_test_p_values[i] <- compute_t_test_pvalue(data)
    wilcoxon_p_values[i] <- compute_wilcoxon_pvalue(data)
}

type_I_error_t_test <- mean(t_test_p_values < alpha)
type_I_error_wilcoxon <- mean(wilcoxon_p_values < alpha)

cat("Empirical Type I error for one-sample t-test:", type_I_error_t_test, "\n")
cat("Empirical Type I error for Wilcoxon signed-rank test:", type_I_error_wilcoxon, "\n")

# b

t_test_p_values <- numeric(n_sim)
wilcoxon_p_values <- numeric(n_sim)

for (i in 1:n_sim) {

    data <- rnorm(n, 0.5, 1)

    t_test_p_values[i] <- compute_t_test_pvalue(data)
    wilcoxon_p_values[i] <- compute_wilcoxon_pvalue(data)
}

power_t_test <- mean(t_test_p_values < alpha)
power_wilcoxon <- mean(wilcoxon_p_values < alpha)

cat("Empirical power for one-sample t-test:", power_t_test, "\n")
cat("Empirical power for Wilcoxon signed-rank test:", power_wilcoxon, "\n")

#5

# a

set.seed(666)
n_sim <- 2000
alpha <- 0.05

two_sample_t_test_pvalue <- function(x, y) {
    t_test <- t.test(x, y, alternative = "two.sided")$p.value
    return(t_test)
}

power <- numeric()
n_values <- seq(10, 100, by = 10)

for (n in n_values) {
    p_values <- numeric(n_sim)

    for (i in 1:n_sim) {
        x <- rnorm(n, 0, 1)
        y <- rnorm(n, 0.5, 1.5)

        p_values[i] <- two_sample_t_test_pvalue(x, y)
    }

    power[n/10] <- mean(p_values < alpha)

    cat("Sample size:", n, ", Power:", power[n/10], "\n")
}

if (any(power >= 0.8)) {
    min_sample_size <- n_values[which.max(power >= 0.8)]
    cat("Minimum sample size to achieve power > 80%:", min_sample_size, "\n")
} else {
    cat("No sample size achieves power > 80% with the current settings.\n")
}

plot(n_values, power, type = "b", xlab = "Sample Size", ylab = "Power", main = "Power vs Sample Size")
abline(h = 0.8, col = "red", lty = 2)

# b

alpha <- 0.05
beta <- 0.2
sigma_X2 <- 1
sigma_Y2 <- 1.5
mu_diff <- 0.5


z_alpha <- qnorm(1 - alpha/2)
z_beta <- qnorm(1 - beta)
n_formula <- (z_alpha + z_beta)^2 * (sigma_X2^2 + sigma_Y2 ^ 2)/mu_diff^2
min_sample_size = round(n_formula)

cat("Minimum sample size from simulations:", min_sample_size, "\n")

